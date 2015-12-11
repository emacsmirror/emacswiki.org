;;; mon-testme-utils.el --- tests and template for inserting them
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2009-2011 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-testme-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED:  2010-01-22T17:20:30-05:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: tools, test, convenience, development, emacs, extensions

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-testme-utils provides templates for inserting ``;;; :TEST-ME '' and MON
;; style `*-TEST' procedures.
;;
;; FUNCTIONS:►►►
;; `mon-insert-test-cases', `mon-insert-lisp-testme',
;; `mon-insert-lisp-testme-fancy', `mon-gensym-counter-randomizer-TEST'
;; `mon-list-nshuffle-TEST', `mon-line-strings-to-list-TEST',
;; `mon-with-inhibit-buffer-read-only-TEST', `mon-line-dolines-TEST',
;; `mon-line-dolines-setup-TEST', `mon-with-buffer-undo-disabled-TEST',
;; `mon-with-inhibit-buffer-read-only-PP-TEST', `mon-string-split-TEST',
;; `mon-line-strings-bq-qt-sym-bol-TEST', `mon-permute-combine-functions-TEST',
;; `mon-write-string-reset-bind-TEST', `naf-mode-state-to-postal-TEST',
;; `mon-booleanp-to-binary-TEST', `mon-string-or-null-and-zerop-TEST',
;; `mon-error-protect-PP-EXPAND-TEST', `mon-line-string-rotate-name-TEST',
;; `mon-line-indent-from-to-col-TEST', `mon-line-strings-pipe-to-col-TEST',
;; `mon-line-string-insert-chars-under-TEST', `mon-list-reorder-TEST',
;; `mon-regexp-clean-ulan-dispatch-chars-TEST',
;; `mon-set-buffer-substring-no-properties-TEST', `mon-plist-values-TEST',
;; `mon-plist-keys-TEST', `mon-help-propertize-regexp-symbol-defs-TEST',
;; `mon-help-regexp-symbol-defs-TEST', `mon-help-propertize-tags-TEST',
;; `mon-help-keys-wikify-TEST', `mon-build-mon-emacsd-example',
;; `mon-build-misc-path-example', `mon-build-user-name-example',
;; `mon-user-system-conditionals-TEST', `mon-build-user-name-example-TEST',
;; `mon-drive-transfer-template-TEST', `mon-sequence-all-booleanp-TEST',
;; `mon-cln-xml-escapes-TEST', `mon-up/down-case-regexp-TEST',
;; `mon-hash-get-symbol-keys-TEST', `mon-file-stamp-buffer-filename-TEST',
;; `mon-cln-freenode-log-TEST',
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
;; `*mon-testme-utils-xrefs*',
;;
;; GROUPS:
;; `mon-testme-utils',
;;
;; ALIASED/ADVISED/SUBST'D:
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;; `mon-insert-lisp-testme'                    <- mon-insertion-utils.el
;; `mon-insert-test-cases'                     <- mon-insertion-utils.el
;; `mon-list-nshuffle-TEST'                    <- mon-utils.el
;; `mon-gensym-counter-randomizer-TEST'        <- mon-utils.el
;; `mon-line-strings-to-list-TEST'             <- mon-utils.el
;; `mon-with-inhibit-buffer-read-only-TEST'    <- mon-utils.el
;; `mon-with-inhibit-buffer-read-only-PP-TEST' <- mon-utils.el
;; `mon-line-dolines-TEST'                     <- mon-utils.el
;; `mon-line-dolines-setup-TEST'               <- mon-utils.el
;; `mon-with-buffer-undo-disabled-TEST'        <- mon-utils.el
;; `mon-string-split-TEST'                     <- mon-utils.el
;; `mon-line-strings-bq-qt-sym-bol-TEST'       <- mon-utils.el
;; `mon-build-copyright-string-TEST'           <- mon-insertion-utils.el
;; `google-define-get-command-TEST'            <- google-define-redux.el
;; `mon-permute-combine-functions-TEST'        <- mon-name-utils.el
;; `mon-regexp-clean-ulan-dispatch-chars-TEST' <- mon-regexp-symbols.el
;; `mon-help-propertize-regexp-symbol-defs-TEST' <- mon-doc-help-utils.el
;; `mon-help-regexp-symbol-defs-TEST'            <- mon-doc-help-utils.el
;; `mon-help-propertize-tags-TEST'               <- mon-doc-help-utils.el
;; `mon-help-keys-wikify-TEST'                   <- mon-doc-help-utils.el
;; `mon-drive-transfer-template-TEST'            <- mon-drive-transfer-utils.el
;; `mon-cln-xml-escapes-TEST'                    <- mon-replacement-utils.el
;; `mon-up/down-case-regexp-TEST'                <- mon-replacement-utils.el
;;
;; TODO:
;;
;; Should these `-TEST' fncns be moved into here?
;; `mon-permute-combine-functions-TEST'
;; `mon-wget-list-to-script-TEST'
;;
;; NOTES:
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-testme-utils.el
;; FIRST-PUBLISHED: <Timestamp: #{2010-02-11T19:48:58-05:00Z}#{10065} - by MON>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-testme-utils. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-01-22T17:20:30-05:00Z}#{10035} - by MON KEY>
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
;; Copyright © 2009-2011 MON KEY 
;;; ==============================

;;; CODE:

 
(eval-when-compile (require 'cl))

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))


 
;;; ==============================
;;; :CHANGESET 2189
;;; :CREATED <Timestamp: #{2010-10-07T16:02:18-04:00Z}#{10404} - by MON KEY>
(defgroup mon-testme-utils nil
  "Customization group for mon test functions.\n
:SEE-ALSO `*mon-testme-utils-xrefs*'.\n►►►"
  :link '(url-link :tag ":EMACSWIKI-FILE" 
                   "http://www.emacswiki.org/emacs/mon-testme-utils.el")
  :link '(emacs-library-link "mon-test-me-utils.el")
  :group 'mon-base)

;;; ==============================
;;; :CHANGESET 2387
;;; :CREATED <Timestamp: #{2011-01-11T14:05:50-05:00Z}#{11022} - by MON KEY>
(defcustom *mon-testme-utils-xrefs* 
  '(mon-booleanp-to-binary-TEST
    mon-build-misc-path-example
    mon-build-mon-emacsd-example
    mon-build-user-name-example
    mon-build-user-name-example-TEST
    mon-cln-freenode-log-TEST
    mon-cln-xml-escapes-TEST
    mon-drive-transfer-template-TEST
    mon-error-protect-PP-EXPAND-TEST
    mon-file-stamp-buffer-filename-TEST
    mon-gensym-counter-randomizer-TEST
    mon-hash-get-symbol-keys-TEST
    mon-help-keys-wikify-TEST
    mon-help-propertize-regexp-symbol-defs-TEST
    mon-help-propertize-tags-TEST
    mon-help-regexp-symbol-defs-TEST
    mon-insert-lisp-testme
    mon-insert-lisp-testme-fancy
    mon-line-dolines-TEST
    mon-line-dolines-setup-TEST
    mon-line-indent-from-to-col-TEST
    mon-line-string-insert-chars-under-TEST
    mon-line-string-rotate-name-TEST
    mon-line-strings-bq-qt-sym-bol-TEST
    mon-line-strings-pipe-to-col-TEST
    mon-line-strings-to-list-TEST
    mon-list-nshuffle-TEST
    mon-list-reorder-TEST
    mon-permute-combine-functions-TEST
    mon-plist-keys-TEST
    mon-plist-values-TEST
    mon-regexp-clean-ulan-dispatch-chars-TEST
    mon-sequence-all-booleanp-TEST
    mon-set-buffer-substring-no-properties-TEST
    mon-string-or-null-and-zerop-TEST
    mon-string-split-TEST
    mon-up/down-case-regexp-TEST
    mon-user-system-conditionals-TEST
    mon-with-buffer-undo-disabled-TEST
    mon-with-inhibit-buffer-read-only-PP-TEST
    mon-with-inhibit-buffer-read-only-TEST
    mon-write-string-reset-bind-TEST
    naf-mode-state-to-postal-TEST
    mon-insert-test-cases
    *mon-testme-utils-xrefs*)
  "Xrefing list `mon-*-TEST' functions, constants, and variables.\n
Symbols defined in :FILE mon-testme-utils.el\n
:SEE-ALSO `*mon-regexp-symbols-xrefs*', `*mon-button-utils-xrefs*',
`*mon-default-loads-xrefs*', `*mon-default-start-loads-xrefs*',
`*mon-dir-locals-alist-xrefs*', `*naf-mode-xref-of-xrefs*',
`*mon-ulan-utils-xrefs*', `*mon-testme-utils-xrefs*'.\n►►►"
  :type '(repeat symbol)
  :group 'mon-testme-utils
  :group 'mon-xrefs)


;;; ==============================
;;; :NOTE See below @BOF for unfinished version `mon-insert-lisp-testme-fancy'
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
:SEE-ALSO `mon-insert-doc-help-tail', `mon-help-regexp-symbol-defs-TEST',
`mon-insert-doc-help-tail', `mon-insert-lisp-stamp', `mon-insert-copyright',
`mon-insert-lisp-CL-file-template', `mon-comment-divider',
`mon-comment-divider-to-col-four', `mon-insert-lisp-evald',
`*mon-testme-utils-xrefs*'.\n►►►"
  (interactive "i\np\ni\np")
  (let* ((get-func)
         (tmc (cond ((and intrp (> test-me-count 1))
                      (if ((lambda () ;; (yes-or-no-p "Search-function-name?: ")))
			     (yes-or-no-p (concat ":FUNCTION `' "
						  "-- search function name?: "))))
			  (progn (setq get-func t)   test-me-count)
			(progn (setq get-func nil)   test-me-count)))
                    ((not test-me-count) 1)
                    (t  test-me-count)))
         (func (if (or search-func get-func)
                   (save-excursion
                     ;; <Timestamp: #{2010-07-28T11:37:06-04:00Z}#{10303} - by MON KEY>
                     ;; :WAS (search-backward-regexp  *regexp-symbol-defs*)
                     (search-backward-regexp  *regexp-symbol-defs-big*)
                     (buffer-substring-no-properties (match-beginning 3) (match-end 3)))))
         (test-me-string (if (or search-func get-func)
                             (format ";;; :TEST-ME (%s )" func)
                           ";;; :TEST-ME "))
         ;; :WAS (limit (make-marker))
         (cnt tmc)
         (return-tms))
    (while (>= cnt 1)
      (setq return-tms (concat test-me-string "\n" return-tms))
      (setq cnt (1- cnt)))
    (if (or intrp insertp)
	;; :NOTE This actually appear to work for (mon-insert-lisp-testme nil 2 nil t) and
	;; I no longer have a clue what i was trying to accomplish with the marker
	;; passing:
	;; :WAS (progn
	;;   (save-excursion
	;;     (when insertp (newline))
	;;     (when (not (bolp))(beginning-of-line))
	;;     (princ return-tms (current-buffer)))
	;;   (set-marker limit (point))
	;;   (search-forward-regexp (format "%s$" test-me-string) (marker-position limit) t)
        ;;   t) ; t needed here to prevent returning buffer position when called externally?
	(save-excursion
	  (when insertp (newline))
	  (when (not (bolp))(beginning-of-line))
	  (princ return-tms (current-buffer)))
      ;; else
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
;;; :TEST-ME (mon-insert-lisp-testme nil 2 nil t)


;;; ==============================
;;; :NOTE This doesn't seem all that useful in rertrospect.
;;; :MODIFICATIONS <Timestamp: 2009-08-01-W31-6T16:03:23-0400Z - by MON KEY>
;;; :CREATED <Timestamp: Wednesday March 04, 2009 @ 06:16.40 PM - by MON KEY>
;;;###autoload
(defun mon-insert-test-cases (&optional insertp intrp)
  "Easily identified tracing vars for debugging `mon-*' and `naf-mode-*' functions.\n
Unbinds all previously bound variables:\n
 test-aaa, test-bbb, test-ccc, test-ddd,
 test-aa, test-AA, test-ag, test-AG, test-a4, test-A4\n
Rebinds vars 'default values'.
Called-interactively or INSERTP non-nil insert test-cases at point.\n
:EXAMPLE\n\n\(mon-insert-test-cases\)\n
:SEE-ALSO `mon-insert-lisp-testme', `*mon-testme-utils-xrefs*'.\n►►►"
  (interactive "i\np") 
  (mapc #'makunbound  '(test-aaa test-bbb test-ccc test-ddd test-aa
                                 test-AA test-ag test-AG test-a4 test-A4))
  (let* ((testing-list 
          '((setq test-aaa '(("1111" "2222") ("3333" "4444")
                             ("5555" "6666") ("7777" "8888") ("9999" "x0x0")))
            (setq test-bbb '(("aaaa" "bbbb") ("cccc" "dddd") 
                             ("eeee" "ffff") ("gggg" "hhhh") ("iiii" "jjjj")))
            (setq test-ccc '(("AAAA" "BBBB") ("CCCC" "DDDD") 
                             ("EEEE" "FFFF") ("GGGG" "HHHH") ("IIII" "JJJJ")))
            (setq test-ddd '(("a1A1" "b2B2") ("c3C3" "d4D4") 
                             ("e5E5" "f6F6") ("g7G7" "h8H8") ("i9I9" "j1J0")))
            (setq test-aa '((a a)(b b)(c c)(d d))) 
            (setq test-AA '((A A)(B B)(C C)(D D)))
            (setq test-ag '((a b)(b c)(d e)(f g))) 
            (setq test-AG '((A B)(B C)(D E)(F G)))
            (setq test-a4 '((a 1) (b 2)(c 3)(d 4))) 
            (setq test-A4 '((A 1)(B 2)(C 3)(D 4)))))
         (put-tests (mapconcat #'(lambda (x)  (format ";;; %S" x)) testing-list "\n")))
    (when (or insertp intrp)
      (save-excursion (newline)(insert put-tests)))
    put-tests))
;;
;;; :TEST-ME (mon-insert-test-cases t)
;;; :TEST-ME (mon-insert-test-cases)
;;; :TEST-ME (call-interactively 'mon-insert-test-cases)


;;; ==============================
;;; :CREATED <Timestamp: #{2010-07-31T14:41:01-04:00Z}#{10306} - by MON>
;;;###autoload
(defun mon-gensym-counter-randomizer-TEST (w-test-str/sym generate-n-results)
  "Test function for `mon-gensym-counter-randomizer'.\n
Return results in buffer named \"*MON-GENSYM-COUNTER-RANDOMIZER-TEST*\".\n
Arg W-TEST-STR/SYM is a string or symbol to build results with.\n
ARG GENERATE-N-RESULTS is the number of results to generate.\n
:EXAMPLE\n\n(mon-gensym-counter-randomizer-TEST \"bubba\" 10000)\n
:SEE-ALSO `mon-gensym', `with-gensyms', `mon-gensym-counter-randomizer',
`*mon-testme-utils-xrefs*'.\n►►►"
  (let ((mgcr-tst-buf (get-buffer-create "*MON-GENSYM-COUNTER-RANDOMIZER-TEST*"))
        mlf-dups)
    (with-current-buffer (get-buffer mgcr-tst-buf)
      (erase-buffer)
      (with-temp-message (concat ":FUNCTION `mon-gensym-counter-randomizer-TEST' "
                                 (format "-- still processing arg %S %d times ... " 
                                         w-test-str/sym generate-n-results))
        (save-excursion
          (dotimes (mlf-i generate-n-results)
            (princ (mon-gensym-counter-randomizer w-test-str/sym) (current-buffer))
            (newline))
          (sort-lines nil (mon-g2be -1 t) (mon-g2be 1 t)) )
        (setq mlf-dups (mon-line-find-duplicates))
        (save-excursion
          (apply #'insert 
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
;;; :COURTESY gene.ressler@gmail.com comp.lang.lisp 2010-08-01
;;; :CREATED <Timestamp: #{2010-08-03T18:29:33-04:00Z}#{10312} - by MON>
;;;###autoload
(defun mon-list-nshuffle-TEST (w-test-times)
  "Test function for `mon-list-nshuffle'\n
Return results of applying `mon-list-nshuffle'W-TEST-TIMES in buffer
named \"*MON-LIST-NSHUFFLE-TEST*\"\n
:EXAMPLE\n\n\(mon-list-nshuffle-TEST '\(a b c d\) 100\)\n
\(mon-list-nshuffle '\(\"a\" \"b\" \"c\" \"d\"\) 100\)\n
:SEE-ALSO `mon-nshuffle-vector', `mon-list-shuffle-safe', `*mon-testme-utils-xrefs*'.\n►►►"
  (with-current-buffer (get-buffer-create "*MON-LIST-NSHUFFLE-TEST*")
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
    (mon-g2be -1) 
    (display-buffer (current-buffer) t)))
;;
;;; :TEST-ME (mon-list-nshuffle-TEST 10000)
;;; :TEST-ME (mon-list-nshuffle-TEST 100)

;;; ==============================
;;; :RENAMED `mon-line-strings-to-list-*test*' -> `mon-line-strings-to-list-TEST'
;;; :CREATED <Timestamp: #{2009-09-13T09:28:46-04:00Z}#{09377} - by MON>
;;;###autoload
(defun mon-line-strings-to-list-TEST (&optional with-cdr with-wrap insrtp)
  "Test function for `mon-line-strings-to-list'.\n
:SEE-ALSO `mon-build-copyright-string-TEST', `mon-help-regexp-symbol-defs-TEST', 
`mon-help-propertize-regexp-symbol-defs-TEST', 
`mon-help-regexp-symbol-defs-TEST', `mon-help-CL-wget-pkgs-TEST', 
`mon-help-propertize-tags-TEST', `mon-insert-test-cases', `*mon-testme-utils-xrefs*'.\n►►►"
  (let ((st01 (make-marker))
        (en01 (make-marker))
        (t-str (concat "hendr erit\norci\nultrices\naugue\nAliquam\n"
                       "odio\nNam\ne ros\nurna\naliquam\nvitae\nlacinia")))
    (cond ((not insrtp)
           (with-temp-buffer
             (insert t-str)
             (mon-line-strings-to-list (mon-g2be -1 t) (mon-g2be 1 t)
                                       with-cdr with-wrap)))
          (insrtp 
           (set-marker st01 (point))           
           (insert t-str)
           (set-marker en01 (point))
           (goto-char st01)
           (prog1 
               (mon-line-strings-to-list st01 en01 with-cdr with-wrap t)
             (set-marker st01 nil)
             (set-marker en01 nil))))))

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
;;; :CHANGESET 2142
;;; :CREATED <Timestamp: #{2010-09-25T12:22:35-04:00Z}#{10386} - by MON KEY>
;;;###autoload
(defun mon-with-inhibit-buffer-read-only-PP-TEST ()
  "Test function for `mon-with-inhibit-buffer-read-only'.
:EXAMPLE\n\n(mon-with-inhibit-buffer-read-only-PP-TEST)\n
\(mon-with-inhibit-buffer-read-only-TEST t\)\n
:SEE-ALSO `mon-with-inhibit-buffer-read-only-TEST',
`mon-build-copyright-string-TEST', `mon-help-keys-wikify-TEST',
`mon-help-propertize-regexp-symbol-defs-TEST', `mon-help-propertize-tags-TEST',
`mon-help-regexp-symbol-defs-TEST', `mon-help-CL-wget-pkgs-TEST',
`mon-inhibit-read-only-TEST', `mon-line-strings-to-list-TEST',
`mon-user-system-conditionals-TEST', `mon-wget-list-to-script-TEST',
`*mon-testme-utils-xrefs*'.\n►►►"
  (let ((miro-bfr (get-buffer-create 
                   "*MON-WITH-INHIBIT-BUFFER-READ-ONLY-TEST*")))
    (with-current-buffer miro-bfr
      (erase-buffer)
      (save-excursion 
        (pp-display-expression 
         (macroexpand '(mon-with-inhibit-buffer-read-only (insert "bubba")))
         (buffer-name miro-bfr)))
      (set (make-local-variable 'buffer-read-only) t))
    (display-buffer miro-bfr t)))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-26T15:25:28-04:00Z}#{10125} - by MON KEY>
;;;###autoload
(defun mon-with-inhibit-buffer-read-only-TEST (&optional w-display-buffer)
  "Test function for `mon-with-inhibit-buffer-read-only'.
:EXAMPLE\n\n(mon-with-inhibit-buffer-read-only-TEST)\n
\(mon-with-inhibit-buffer-read-only-TEST t\)\n
:SEE-ALSO `mon-with-inhibit-buffer-read-only-PP-TEST',
`mon-build-copyright-string-TEST', `mon-help-keys-wikify-TEST',
`mon-help-propertize-regexp-symbol-defs-TEST', `mon-help-propertize-tags-TEST',
`mon-help-regexp-symbol-defs-TEST', `mon-help-CL-wget-pkgs-TEST',
`mon-inhibit-read-only-TEST', `mon-line-strings-to-list-TEST',
`mon-user-system-conditionals-TEST', `mon-wget-list-to-script-TEST',
`*mon-testme-utils-xrefs*'.\n►►►"
  (let ((mwirot (get-buffer-create "*MON-WITH-INHIBIT-BUFFER-READ-ONLY-TEST*"))
        shw-msg)
    (with-current-buffer mwirot
      (erase-buffer)
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
;;; :CREATED <Timestamp: #{2010-05-26T15:07:47-04:00Z}#{10213} - by MON KEY>
;;;###autoload
(defun mon-line-dolines-setup-TEST ()
  "Helper for `mon-line-dolines' macro's test function `mon-line-dolines-TEST'.\n
:SEE (URL `http://lists.gnu.org/archive/html/help-gnu-emacs/2009-12/msg00614.html')\n
:SEE-ALSO `*mon-testme-utils-xrefs*'.\n►►►"
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
        (with-current-buffer (get-buffer-create "*MON-LINE-DOLINES-TEST*")
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
;;;###autoload
(defun mon-line-dolines-TEST ()
  "Test function for `mon-line-dolines' macro.\n
Return the output of that macro as per its original intended use.\n
Values returned in 3 (or 4) seperate buffers named:\n
 \"*GOT-YEARS*\" \"*GOT-MONTHS*\" \"*GOT-DAYS*\"\n
When current-buffer does not have a display or is read-only return additional
details in buffer named \"*MON-LINE-DOLINES-TEST*\".\n
:EXAMPLE\n\n\(mon-line-dolines-TEST\)\n
:SEE (URL `http://lists.gnu.org/archive/html/help-gnu-emacs/2009-12/msg00614.html')\n
:SEE-ALSO `mon-line-dolines-setup-TEST', `*mon-testme-utils-xrefs*'.\n►►►"
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
;;; :CREATED <Timestamp: #{2010-06-14T15:41:21-04:00Z}#{10241} - by MON KEY>
;;;###autoload
(defun mon-with-buffer-undo-disabled-TEST (&optional force-fail)
  "Test function for `mon-with-buffer-undo-disabled' macro.\n
When optional arg FORCE-FAIL is non-nil force test failure.\n
:EXAMPLE\n\n\(mon-with-buffer-undo-disabled-TEST\)\n
\(mon-with-buffer-undo-disabled-TEST 'force-fail\)\n
:SEE-ALSO `buffer-undo-list', `*mon-testme-utils-xrefs*'.\n►►►"
  (let ((rnd-char (mon-nshuffle-vector 
                    (vconcat (append (number-sequence 97 122)
                                     (number-sequence 65 90)))))
        bul)
    (with-temp-buffer 
      (mon-with-buffer-undo-disabled (setq bul buffer-undo-list))
      (unless force-fail
        (dotimes (jnk 100) 
          (insert (make-string (random 79) (elt rnd-char (random 52))) "\n"))
        (delete-region (random (1- (mon-g2be 1 t)))
                       (random (1- (mon-g2be 1 t)))))
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
;;; :CHANGESET 2178
;;; :CREATED <Timestamp: #{2010-10-06T20:15:12-04:00Z}#{10403} - by MON KEY>
;;;###autoload
(defun mon-string-split-TEST (&optional w-msg-usr)
  "Test function for `mon-string-split'.\n
:EXAMPLE\n\n\(mon-string-split-TEST\)\n
:SEE-ALSO `*mon-testme-utils-xrefs*'.\n►►►"
  (let ((w-chk-str "With regexp SPLIT-PATTERN, split W-STRING-TO-SPLIT into a list of substrings.")
        (w-chk-pa '((("s." nil nil) . (16))
                    (("sp" nil nil) . (7 ))
                    (("s." 4   nil) . (9 ))
                    (("s." 0   nil) . (1 ))
                    (("."  nil nil) . (77))
                    (("."  nil nil) . (78 . ?f))
                    (("."  8   nil) . (9))
                    (("."  8   t)   . (16))
                    ((" "  nil nil) . (19))
                    (("."  8   t)   . (18 . ?f)))) 
        (rslt 0)
        pss-fls)
    (dolist (i w-chk-pa)
      (let ((got-len (length (apply 'mon-string-split  `(,(caar i) ,w-chk-str ,@(cdar i)))))
            (chk-len (cadr i)))
        (push  `((,(or (= got-len chk-len)) . ,(cddr i)) . (,rslt ,(car i) ,chk-len)) pss-fls))
      (incf rslt))
    (setq rslt (concat ":FUNCTION `mon-string-split-TEST' \n"
           (mapconcat #'(lambda (fld)
                          (apply #'format 
                                 `(,(cond ((caar fld) ;;(not (integerp (cdar fld))))
                                           "-- test %d passed with args: %S length return value was = %d")
                                          ((and (not (caar fld)) (and (integerp (cdar fld))(= (cdar fld) 102)))
                                           "-- test %d successful failure with args: %S length return value not = %d")
                                          (t "-- test %d FAILED with args: %S length return value NOT = %d"))
                                   ,@(cdr fld))))
                      (nreverse pss-fls) "\n")))
    (or (and w-msg-usr (message rslt))
        rslt)))

;;; ==============================
;;; :CHANGESET 1789
;;; :CREATED <Timestamp: #{2010-05-29T20:29:13-04:00Z}#{10216} - by MON KEY>
;;;###autoload
(defun mon-line-strings-bq-qt-sym-bol-TEST ()
  "Test function for `mon-line-strings-bq-qt-sym-bol'.\n
Return restults to buffer named \"*mon-line-strings-bq-qt-sym-bol-TEST*\".\n
:EXAMPLE\n\n\(mon-line-strings-bq-qt-sym-bol-TEST\)\n
:SEE-ALSO `*mon-testme-utils-xrefs*'.\n►►►"
  (with-current-buffer 
      (get-buffer-create "*MON-LINE-STRINGS-BQ-QT-SYM-BOL-TEST*")
    (let ((mlsbqsb-str  (mapconcat #'identity
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
;;; :TEST-ME (mon-line-strings-bq-qt-sym-bol-TEST)


;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-24T14:27:52-05:00Z}#{09524} - by MON KEY>
;;;###autoload
(defun mon-help-CL-wget-pkgs-TEST ()
  "Helper function to verify `mon-help-CL-wget-pkgs' is functioning as expected.\n
Performs the following checks:\n
./ Writes a temp file with output from mon-help-CL-wget-pkgs;
./ Return inserted contents of temp file in a temporary buffer;
./ Display that buffer with `file-attributes' in header;
./ Kills temp-buffer and file on exit;\n
:EXAMPLE\n\n\(mon-help-CL-wget-pkgs-TEST\)\n
:NOTE On exit this function should cleanup the temp file/buffer objects below:\n
./ A temp file written to:
   /PATH/TO/`default-directory'/tmp-wget-YY-MM-DD\n
./  A temp-buffer with the name *SHOW-WGET-TEMP*.\n
:SEE-ALSO `mon-help-CL-pkgs', `*mon-help-CL-cmu-ai-repo*',
`*mon-help-CL-ext-pkg-map*', `*mon-testme-utils-xrefs*'.\n►►►"
  (save-excursion
    (let ((tmp-wget-cl-pkgs (concat default-directory 
                                    "tmp-wget-"
                                    (format-time-string "%Y-%M-%d")))
          (show-wget-cl-pkgs)
          (tmp-wget-spec))
      (if (file-exists-p tmp-wget-cl-pkgs)
           (error (concat ":FUNCTION `mon-help-CL-wget-pkgs-TEST' "
                          "-- pre existent file: %s")
                  tmp-wget-cl-pkgs)
        (progn
            (mon-help-CL-wget-pkgs tmp-wget-cl-pkgs)
            (setq show-wget-cl-pkgs
                  (with-temp-buffer
                    (insert-file-contents tmp-wget-cl-pkgs)
                    (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
            (setq show-wget-cl-pkgs
                  (multiple-value-bind
                        (d l UID GID ACCESSED MODIFIED s SIZE MODE gmod inod dev)
                      (file-attributes tmp-wget-cl-pkgs) ;; (buffer-file-name))
                    (format (concat "## :FILE #P %s\n## :UID %s\n## :GID %s\n"
                                    "## :ACCESSED %s\n## :MODIFIED %s\n"
                                    "## :SIZE %s\n## :MODE %s\n"
                                    "## :CONTENTS-OF-TEMP-FILE-BELOW\n"
                                    "### ==============================\n%s")
                            tmp-wget-cl-pkgs 
                            UID GID 
                            (format-time-string "%Y-%m-%d %H:%M:%S" ACCESSED)
                            (format-time-string "%Y-%m-%d %H:%M:%S" MODIFIED)
                            SIZE MODE
                            show-wget-cl-pkgs)))
            (delete-file tmp-wget-cl-pkgs)
            (with-output-to-temp-buffer 
                (buffer-name (get-buffer-create "*SHOW-WGET-TEMP*"))
              (princ show-wget-cl-pkgs))
            (sit-for 10)))
      (with-current-buffer (get-buffer-create "*SHOW-WGET-TEMP*")
        (kill-buffer (current-buffer))))))
;;
;;; :TEST-ME (mon-help-CL-wget-pkgs-TEST)


;;; ==============================
;;; :NOTE Calls `mon-help-propertize-tags' when available.
;;; :MODIFICATIONS <Timestamp: #{2010-03-03T14:42:53-05:00Z}#{10093} - by MON KEY>
;;; :CREATED <Timestamp: #{2010-02-10T12:24:33-05:00Z}#{10063} - by MON KEY>
;;;###autoload
(defun mon-build-copyright-string-TEST ()
  "Test function for `mon-build-copyright-string'.\n
:EXAMPLE\n\n(mon-build-copyright-string-TEST)\n
:SEE-ALSO `mon-build-copyright-string-license', `mon-build-copyright-string',
`mon-insert-gnu-licence-gfdl', `mon-insert-gnu-licence',
`mon-insert-file-template', `*mon-mit-license-header*',
`*mon-bsd-license-header*', `*mon-gnu-license-header-gfdl*',
`*mon-gnu-license-header*', `mon-build-copyright-string-TEST',
`mon-help-propertize-regexp-symbol-defs-TEST', `mon-help-propertize-tags-TEST',
`mon-help-regexp-symbol-defs-TEST', `mon-help-CL-wget-pkgs-TEST',
`mon-line-strings-to-list-TEST', `*mon-testme-utils-xrefs*'.\n►►►"
  (interactive)
  (let* ((mbcst (get-buffer-create "*COPYRIGHT-STRING-TEST*"))
         (mbcs-args (format "%s\n" (help-function-arglist 'mon-build-copyright-string)))
         (mbcst-divd (make-string 67 95))        
         (formt-tests `(,(mapconcat #'identity
                                    (save-match-data 
                                      (split-string
                                       (concat 
                                        "| Arglist for `mon-build-copyright-string':\n"
                                        mbcs-args "\n\n"
                                        "Documentation:\n\n"
                                        (documentation 'mon-build-copyright-string)
                                        ) "\n"))
                                    "\n| ")
                         ,(let ((IS-MON-P nil))
                               `(":DEFAULT" 
                                 . ,(mon-build-copyright-string)))
                         (,(concat ":THESE-ARGS nil nil t\n"
                                   "| :WITH MONKEY")
                           . ,(mon-build-copyright-string nil nil t))
                         ,(let ((IS-MON-P nil))
                               `(,(concat ":THESE-ARGS nil nil t" 
                                          "| :DEFAULT :WITH MONKEY (and IS-MON-P nil)")
                                  . ,(mon-build-copyright-string nil nil t)))
                         (,(concat ":THESE-ARGS nil nil t nil nil t\n" 
                                   "| :WITH MONKEY :WITH SHORT-FORM")
                           . ,(mon-build-copyright-string nil nil t nil nil t))
                         (,(concat ":THESE-ARGS nil nil t t\n"  
                                   "| :WITH MONKEY :WITH NO-NL")
                           . ,(mon-build-copyright-string nil nil t t))
                         ,(let ((*MON-ORG-NAME* nil))
                               `(,(concat  ":THESE-ARGS nil nil t nil t\n" 
                                           "| :WITH MONKEY :WITH W-ORG (and *MON-ORG-NAME* nil)")
                                  . ,(mon-build-copyright-string nil nil t nil t )))
                         (,(concat ":THESE-ARGS nil nil t nil t\n"
                                   "| :WITH MONKEY :WITH W-ORG")
                           . ,(mon-build-copyright-string nil nil t nil t ))
                         (,(concat  ":THESE-ARGS nil nil t nil t t\n"
                                    "| :WITH MONKEY :WITH W-ORG :WITH SHORT-FORM" )
                           . ,(mon-build-copyright-string nil nil t nil t t))
                         (,(concat ":THESE-ARGS nil nil t t nil t\n"
                                   "| :WITH MONKEY :WITH NO-NL :WITH SHORT-FORM")
                           . ,(mon-build-copyright-string nil nil t t nil t))
                         (,(concat ":THESE-ARGS nil nil nil nil nil t\n"
                                   "| :WITH SHORT-FORM")
                           . ,(mon-build-copyright-string nil nil nil nil nil t))
                         (,(concat ":THESE-ARGS nil nil nil nil t\n"
                                   "| :WITH MONKEY :WITH NO-NL :WITH W-ORG :WITH SHORT-FORM" )
                           . ,(mon-build-copyright-string nil nil t t t t))
                         (,(concat ":THESE-ARGS nil nil nil t t t\n" 
                                   "| :WITH NO-NL :WITH W-ORG :WITH SHORT-FORM")
                           . ,(mon-build-copyright-string nil nil nil t t t)))))
    (with-current-buffer mbcst
      (erase-buffer)
      (save-excursion
        (princ (concat " " mbcst-divd "\n| \n" (pop formt-tests) "\n") (current-buffer))
        (princ (concat 
                (mapconcat #'(lambda (e) 
                               (concat " " mbcst-divd "\n|\n| " mbcs-args "| " (car e) "\n| =>\n" (cdr e)))
                           formt-tests "\n")
                "\n " mbcst-divd)
               (current-buffer))
      (when (fboundp 'mon-help-propertize-tags)
        (mon-help-propertize-tags
         '(":WITH\\|:DEFAULT" 0 mon-help-DYNATAB-tag)
         '(":THESE-ARGS" 0 mon-help-DYNATAB-tag)
         '("&optional" 0 font-lock-type-face)
         '("^ _\\{67,67\\}$" 0 mon-help-INNER-KEY-tag)
         '("^|" 0 mon-help-INNER-KEY-tag)
         '("\\( \\(MONKEY\\|NO-NL\\|SHORT-FORM\\|W-ORG\\)\\)" 2 mon-help-BUILTIN-tag)
         '("<SOME-ORGANIZATION>" 0 mon-help-META-tag)))))
    (display-buffer mbcst t)))
;;
;;; :TEST-ME (mon-build-copyright-string-TEST)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-06-09T18:00:56-04:00Z}#{10233} - by MON KEY>
;;;###autoload
(defun google-define-get-command-TEST ()
  "Test function for `google-define-get-command'.\n
Return the raw html for the gg definition of `snarf'.
display results in buffer named \"*google-define-get-command-TEST*\"
kill the leftover buffer `*google-define-get-buffer*'.\n
:EXAMPLE\n\n\(google-define-get-command-TEST\)\n
:SEE-ALSO `google-define', `google-define-kill-def-buffers',
`*mon-testme-utils-xrefs*'.\n►►►"
  (let ((gdgc-buff 
         (get-buffer 
          (google-define-get-command 
           "www.google.com" 
           "/search?num=100&hl=en&q=define%3A%22snarf%22&btnG=Search")))
        gdgc)
    (setq gdgc (with-current-buffer gdgc-buff (mon-buffer-sub-no-prop) ))
    (when gdgc-buff (kill-buffer gdgc-buff))
    (with-current-buffer (get-buffer-create "*google-define-get-command-TEST*")
      (save-excursion  (insert gdgc))
      (display-buffer (current-buffer) t))))

;;; ==============================
;;; :RENAMED `mon-test-permute-combine-functions' -> `mon-permute-combine-functions-TEST'
;;; :MODIFICATIONS <Timestamp: #{2010-02-09T20:23:43-05:00Z}#{10063} - by MON KEY>
;;;###autoload
(defun mon-permute-combine-functions-TEST ()
  "Assure functional equivalence of permute/combine functionss.\n
Return test-case results in buffer named \"*MON-PERMUTE-COMBINE*\".\n
Test equivalence of return values of following procedures:\n
 `mon-permute-combine', `mon-permute-combine-1', \n
:EXAMPLE\n\n\(mon-permute-combine-functions-TEST\)\n
:SEE-ALSO `mon-list-variant-forms', `mon-list-permute-variants',
`mon-list-permute-1', `mon-list-permute-2', `*mon-testme-utils-xrefs*'.\n►►►"
  (let* ((sab '("StringA" "StringB"))
         (s1-4 '("String1" "String2" "String3" "String4"))
         (s-symab '(a b))
         (s-num1-4 '(1 2 3 4))
         (spc-n   (mon-permute-combine s-symab s-num1-4))
         (spc-2-n (mon-permute-combine s-symab s-num1-4))
         (spc-3-n (mon-permute-combine s-symab s-num1-4))
         (spc-s   (mon-permute-combine sab s1-4))
         (spc-2-s (mon-permute-combine-1 sab s1-4))
         ;; (spc-3-s (mon-permute-combine-2 sab s1-4))
         (spc-str-n   (mon-permute-combine sab s-num1-4))
         (spc-str-n-2 (mon-permute-combine-1 sab s-num1-4))
         ;; (spc-str-n-3 (mon-permute-combine-2 sab s-num1-4))
         (spc-str-symb   (mon-permute-combine sab s-symab))
         (spc-str-symb-2 (mon-permute-combine-1 sab s-symab))
         ;; (spc-str-symb-3 (mon-permute-combine-2 sab s-symab)
         (spcn= (and (equal spc-n spc-2-n)
                     ;;(equal spc-2-n spc-3-n)
                     ))
         (spcs= (and (equal spc-s spc-2-s) 
                     ;;(equal spc-2-s spc-3-s)
                     ))
         (spc-str-n= (and (equal spc-str-n spc-str-n-2) 
                          ;;(equal spc-str-n-2 spc-str-n-3)
                          ))
         (spc-str-symb-= (and (equal spc-str-symb spc-str-symb-2) 
                              ;;(equal spc-str-symb-2 spc-str-symb-3)
                              ))
         (frmt-list
          '(((spcn=          . "numbers - %S ;")
             (spcs=          . "Strings - %S ;")
             (spc-str-n=     . "Strings w/ numbers - %S ;")
             (spc-str-symb-= . "Strings w/ Symbols - %S ;"))
            (mon-permute-combine    (spc-s spc-n spc-str-n spc-str-symb))
            (mon-permute-combine-1  (spc-2-s spc-2-n spc-str-n-2 spc-str-symb-2))
            ;; (mon-permute-combine-2  (spc-3-s spc-3-n spc-str-n-3 spc-str-symb-3))
            ))
         (dlm ";;; ==============================")
         (tpc (get-buffer-create "*MON-PERMUTE-COMBINE*"))
         rslt)
    (dolist (hd (pop frmt-list)
                (setq rslt (concat dlm "\n;;; "
                                   (mapconcat #'(lambda (x) 
                                                  (format "`%s', " x))
                                              (mapcar #'car frmt-list) "")
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
      (mon-g2be -1))
    (display-buffer tpc t)))
;; 
;;; :TEST-ME (mon-permute-combine-functions-TEST)


;;; ==============================
;;; :CHANGESET 2201
;;; :CREATED <Timestamp: #{2010-10-19T21:40:13-04:00Z}#{10422} - by MON KEY>
;;;###autoload
(defun mon-write-string-reset-bind-TEST ()
  "Test function for `mon-write-string' w/ keyword arg :RESET-BIND.\n
Return and display results in buffer named \"*MON-WRITE-STRING-TEST*\".\n
:EXAMPLE\n\n\(mon-write-string-reset-bind-TEST\)\n
:SEE-ALSO `*mon-testme-utils-xrefs*'.\n►►►"
  (let ((mwst-tst-bfr (get-buffer-create "*MON-WRITE-STRING-TEST*"))
        (mwst-tst-mrk (make-marker))
        mwst-tst-bnd)
    ;; Testing keyword :reset-bind w/ variable arg:
    (mon-write-string :reset-null t)
    (with-current-buffer mwst-tst-bfr (erase-buffer))
    (set-marker mwst-tst-mrk (point) mwst-tst-bfr)
    (set-marker-insertion-type mwst-tst-mrk t)
    (mon-write-string :w-string "passing keyword :reset-bind variable named `mwst-tst-bnd'")  
    (prin1 (setq mwst-tst-bnd 
                 `(:RESET-BIND-W-VAR ,(mon-write-string :reset-bind 'mwst-tst-bnd))) mwst-tst-mrk)
    (setq mwst-tst-bnd (list mwst-tst-bnd))
    ;; Testing keyword :reset-bind w/ buffer arg:
    (mon-write-string :w-string "(:RESET-BIND-W-BFR ")
    (push (mon-write-string :reset-bind mwst-tst-bfr) mwst-tst-bnd)
    (prin1 (car mwst-tst-bnd) mwst-tst-mrk)
    (push `(:RESET-BIND-W-BFR ,(pop mwst-tst-bnd)) mwst-tst-bnd)
    (princ ")" mwst-tst-mrk)
    ;; Testing keyword :reset-bind w/ marker arg:
    ;; :NOTE Crazy difficult compensating for marker offsets and we punt below
    ;; by replacing the markers position w/ "XXX".
    (mon-write-string :w-string "(:RESET-BIND-W-MRK ")
    (push (mon-write-string :reset-bind mwst-tst-mrk) mwst-tst-bnd)
    (prin1 (car mwst-tst-bnd) mwst-tst-mrk)
    (push `(:RESET-BIND-W-MRK ,(pop mwst-tst-bnd)) mwst-tst-bnd)
    (princ ")" mwst-tst-mrk)
    (with-current-buffer mwst-tst-bfr 
      (setq mwst-tst-bnd
            `(,(prog1 ;; (buffer-string)
                   ;; Damn difficult dealing w/ reader/point/marker issues here...
                   (replace-regexp-in-string 
                    "\\( \(moves after insertion\) at \\)\\([0-9]\\{3,3\\}\\)" 
                    "<XXX>"  
                    (buffer-string) nil nil 2)
                 (erase-buffer))
              . ,(progn (mapc #'(lambda (dmpit)
                                  (prin1 dmpit mwst-tst-bfr ))
                              (reverse mwst-tst-bnd))
                        (prog1 ;; (buffer-string)
                            (replace-regexp-in-string 
                             "\\( \(moves after insertion\) at \\)\\([0-9]\\{3,3\\}\\)" 
                             "<XXX>"  
                             (buffer-string) nil nil 2)
                          (erase-buffer)))))
      (let ((mwst-dvdr (concat ";; " (make-string 65 61) "\n")))
        (insert ";; :MIRRORED-RESULTS\n"
                (car mwst-tst-bnd) 
                mwst-dvdr 
                ";; :EVALUATED-RESULTS\n"
                (cdr mwst-tst-bnd)
                mwst-dvdr)
        (set-marker mwst-tst-mrk nil mwst-tst-bfr)
        (pp-buffer)
        (setq mwst-tst-mrk (pop mwst-tst-bnd))
        (setq mwst-tst-bfr `(,mwst-tst-bnd . ,mwst-tst-mrk))
        (mon-g2be -1)
        (insert mwst-dvdr
                ";; :FUNCTION `mon-write-string'\n"
                ";; :TESTING keyword :RESET-BIND equivalencies\n"
                ";; :NOTE Test purposefully sets marker positions to <XXX>\n"
                (if (equal (car mwst-tst-bfr)(cdr mwst-tst-bfr))
                    ";; :ALL-TESTS-PASSED\n"
                  ";; :SOMETHING-FAILED -- See below for deails.\n")
                mwst-dvdr ))
      (display-buffer (current-buffer)  t))
    mwst-tst-bfr))

;;; ==============================
;;; :CHANGESET 2203
;;; :CREATED <Timestamp: #{2010-10-20T20:02:22-04:00Z}#{10423} - by MON KEY>
;;;###autoload
(defun mon-region-capitalize-TEST ()
  "Test function for `mon-region-capitalize'.\n
:EXAMPLE\n\n(mon-region-capitalize-TEST\)\n
:SEE-ALSO `*mon-testme-utils-xrefs*'.\n"
  (with-temp-buffer 
    ;; :NOTE don't know why but this unintern is necessary to prevent `rgn-prps' from
    ;; persisting its variable outside the let binding.
    (unintern "rgn-prps" obarray) 
    (let ((rgn-tst "lowercase string aNd UPERCASE STRING")
          rgn-prps)
      (setq rgn-prps nil)
      (save-excursion (insert rgn-tst))
      (setplist 'rgn-prps 
                (mon-region-capitalize (line-beginning-position 1) (line-end-position 1)))
      (setq rgn-prps (list (get 'rgn-prps :REGION)))
      (and (= (- (cdar rgn-prps) (caar rgn-prps))
              (length rgn-tst))
           (push :REGION rgn-prps)
           (eq (car rgn-prps) :REGION)
           (push (get 'rgn-prps :REGION-ORIGINAL) rgn-prps)
           (equal (car rgn-prps) rgn-tst)
           (push :REGION-ORIGINAL rgn-prps)
           (eq (car rgn-prps) :REGION-ORIGINAL)
           (push (get 'rgn-prps :REGION-CAPITAL) rgn-prps)
           (equal (car rgn-prps) (capitalize rgn-tst))
           (push :REGION-CAPITAL rgn-prps)
           (and (equal rgn-prps 
                       `(:REGION-CAPITAL ,(capitalize rgn-tst)
                                         :REGION-ORIGINAL ,rgn-tst
                                         :REGION (1 . ,(1+ (length rgn-tst)))))
                (push '(:ALL-TESTS-PASSED t) rgn-prps)
                (setq rgn-prps rgn-prps)))
      (if (and (listp (car rgn-prps)) (not (eq (car rgn-prps) 1)))
          rgn-prps
        `(:TEST-FAILED mon-region-capitalize-TEST :FAILED-AFTER ,rgn-prps)))))

;;; ==============================
;;; :CHANGESET 2205
;;; :CREATED <Timestamp: #{2010-10-21T16:08:31-04:00Z}#{10424} - by MON KEY>
;;;###autoload
(defun naf-mode-state-to-postal-TEST ()
  "Test associations forward/backward in variable `*naf-mode-state-to-postal*'.\n
:EXAMPLE\n\n(naf-mode-state-to-postal-TEST\)\n
:SEE-ALSO `*mon-testme-utils-xrefs*'.\n►►►"
  (let (nmstp) 
    (push (car (assoc-string "Puerto Rico" *naf-mode-state-to-postal*)) nmstp)
    (push (assoc-string (pop nmstp) *naf-mode-state-to-postal*)  nmstp)
    (setq nmstp `(,(rassoc (cdar nmstp) *naf-mode-state-to-postal*)
                  ,@nmstp))
    (setq nmstp `(:ASSOCIATIONS-EQ ,(eq (car nmstp)  (cadr nmstp)) ,@nmstp))))

;;; ==============================
;;; :CHANGESET 2208
;;; :CREATED <Timestamp: #{2010-10-23T16:05:15-04:00Z}#{10426} - by MON KEY>
;;;###autoload
(defun mon-booleanp-to-binary-TEST ()
  "Test function for `mon-booleanp-to-binary'.\n
Returns list with format:\n
 \(:all-tests-passed <BOOLEAN>
  \(:passed <BOOLEAN> 
   :with-args \( <TEST-FNCN> <ARG-1> <ARG-2> <TEST-VALUE> \)\n
Key :all-tests-passed is non-nil when all values of :passed are non-nil.\n
Key :passed is non-nil when <TEST-FNCN> passed.\n
Key :with-args indicates test evaulated predicate <TEST-FNCN> with results of
invoking `mon-booleanp-to-binary' with <ARG-1> <ARG-2> to satisfy <TEST-VALUE>.\n
:EXAMPLE\n\n\(mon-booleanp-to-binary-TEST\)\n
:SEE-ALSO `mon-sequence-all-booleanp-TEST', `*mon-testme-utils-xrefs*'.\n►►►"
  (let ((mbtb-tests '((= nil  nil  0)
                      (=  t   nil  1)
                      (=  0   t    0)
                      (eq #o0 nil  nil)
                      (=  1    t   1)
                      (eq #o1 nil  nil)
                      (equal "not-a-boolean" t "not-a-boolean")))
        mbtb-gthr)
    (dolist (tst mbtb-tests (setq mbtb-gthr (nreverse mbtb-gthr)))
      (let* ((tst-it   (apply 'mon-booleanp-to-binary (mon-subseq tst 1 3)))
             (tst-that (apply (car tst) (list tst-it (mon-list-last tst)))))
        (push `(:passed ,tst-that 
                        :with-args ,tst) mbtb-gthr)))
    (nconc `(:all-tests-passed
             ,(= 0 (apply #'+ (mapcar #'mon-booleanp-to-binary 
                                      (mapcar #'(lambda (invrtd)
                                                  (not (cadr invrtd)))
                                              mbtb-gthr)))))
           mbtb-gthr)))

;;; ==============================
;;; :CHANGESET 2291
;;; :CREATED <Timestamp: #{2010-11-10T18:39:25-05:00Z}#{10453} - by MON KEY>
;;;###autoload
(defun mon-sequence-all-booleanp-TEST (&optional w-display-buffer)
  "Test function for `mon-sequence-all-booleanp'.\n
Key :all-tests-passed-p is non-nil if so.\n
When optional arg W-DISPLAY-BUFFER is non-nil return and display results in
buffer with name \"*MON-SEQUENCE-ALL-BOOLEANP-TEST*\"\n
:EXAMPLE\n\n\(mon-sequence-all-booleanp-TEST t\)\n
:SEE-ALSO `mon-booleanp-to-binary-TEST', `*mon-testme-utils-xrefs*'.\n►►►"
  (let (gthr)
    (setq gthr 
          (mapcar #'(lambda (x) 
                      `(:test-passed
                        ,(equal (apply #'mon-sequence-all-booleanp (car x)) (cadr x))
                        :w-args   ,(car x)
                        :w-expect ,(cadr x)))
                  `(((t cadr ((a t) (b t) (c t)))                             (t t))
                    ((t cadr ((a t) (b t) (c nil)))                           (t nil))
                    ((t cadr ((a t) (b t) (c nil)) t)                         ((boolean . 3) (t t nil)))
                    ((t car  ((t a) (t b) ("I'm a string" c)) t)              ((string . 3)  (t t "I'm a string")))
                    ;;
                    ((t cadr  [(a t) (b t) (c t)])                             (t t))
                    ((t car   [(t a) (nil b) (c nil)])                         (t nil))
                    ((t caddr [(a 1 t) (b 2 t) (c 3 nil)] t)                   ((boolean . 3) (t t nil)))
                    ((t caddr ,`[(a 1 t) (b 2 t) (c 3 ,(make-vector 3 '6))] t) ((vector . 3) (t t [6 6 6])))
                    ;;
                    ((nil cadr ((a nil) (b nil) (c nil)))                      (nil t))
                    ((nil cadr ((a nil) (b nil) (c t)))                        (nil nil))
                    ((nil cadr ((a nil) (b nil) (c t)) t)                      ((boolean . 3)      (nil nil t)))
                    ((nil car  ((nil a) (nil b) (,(make-vector 3 '6) c)) t)    ((vector . 3) (nil nil [6 6 6])))
                    ;;
                    ((nil cadr  [(a nil) (b nil) (c nil)])                     (nil t))
                    ((nil car   [(nil a) (t b) (c t)])                         (nil nil))
                    ((nil caddr [(a 1 nil) (b 2 nil) (c 3 t)] t)               ((boolean . 3) (nil nil t)))
                    ((nil car  ,`((nil a) (nil b) ("I'm a string" c)) t)       ((string . 3) (nil nil "I'm a string")))
                    )))
    (setq gthr `(:all-tests-passed ,(cadr (mon-sequence-all-booleanp t #'cadr gthr t))
                                   ,@gthr))
    (if w-display-buffer
        (pp-display-expression 
         gthr
         (with-current-buffer 
             (get-buffer-create "*MON-SEQUENCE-ALL-BOOLEANP-TEST*")
           (erase-buffer)
           (buffer-name (current-buffer))))
      gthr)))

;;; ==============================
;;; :CHANGESET 2208
;;; :CREATED <Timestamp: #{2010-10-26T12:46:36-04:00Z}#{10432} - by MON KEY>
;;;###autoload
(defun mon-string-or-null-and-zerop-TEST ()
  "Test function for `mon-string-or-null-and-zerop'.\n
Tests evauluation with args as:\n
 - zero length string 
 - string with length not `zerop'
 - null value -- :NOTE There is not a way to verify an emtpy list, so doesn't.
 - integer 0  -- Fails successfully\n
Return value has the format:\n
\(:all-tests-passed-p <BOOLEAN> 
  \(:test-passed-p <BOOLEAN> :w-sym <LOCAL-SYM> :w-arg <ARG-EVALUATED>\)* ... \)\n
Key :all-tests-passed-p is non-nil if all tests succeeded.\n
Key :test-passed-p is non-nil if individual test suceeded.\n 
:EXAMPLE\n\n\(mon-string-or-null-and-zerop-TEST\)\n
:SEE-ALSO `*mon-testme-utils-xrefs*'.\n►►►"
  (let* ((am-zero-str "")
         am-null       
         (am-bigr-str "am bigger")
         (am-zero-not-str 0)
         (gthr (mapcar #'(lambda (chk-z-or-nul)
                           (mon-string-or-null-and-zerop (symbol-value chk-z-or-nul)))
                       '(am-zero-str am-null am-bigr-str am-zero-not-str)))
         into)
    (setq gthr (mon-mapcar #'(lambda (x y) 
                               (push (mon-booleanp-to-binary x) into)
                               `(:test-passed-p ,x :w-sym ,y :w-arg ,(symbol-value y)))
                           gthr '(am-zero-str am-null am-bigr-str am-zero-not-str)))
    (mapc #'(lambda (0-1-p) (setf 0-1-p (mon-booleanp-to-binary 0-1-p)))
          into)
    (setq gthr (nconc `(:all-tests-passed-p ,(= (apply '+ (nbutlast (nreverse into) 2)) 2))
                      gthr))))

;;; ==============================
;;; :CHANGESET 2141
;;; :CREATED <Timestamp: #{2010-09-16T21:12:01-04:00Z}#{10374} - by MON KEY>
;;;###autoload
(defun mon-error-protect-PP-EXPAND-TEST (expand-form)
  "Debugging function for macro `mon-error-protect'.\n
Return and display results to buffer named \"*PP-EXPAND-ALL*\"
:NOTE Not actually a test.\n
:SEE-ALSO `*mon-testme-utils-xrefs*'.\n►►►"
  (with-current-buffer (get-buffer-create "*PP-EXPAND-ALL*")
    (erase-buffer)
    (save-excursion
      (prin1 (macroexpand-all expand-form) (current-buffer))
      (emacs-lisp-mode)
      (pp-buffer)
      (display-buffer (current-buffer) t))))


;;; ==============================
;;; :CHANGESET 2208
;;; :CREATED <Timestamp: #{2010-10-27T21:18:14-04:00Z}#{10433} - by MON KEY>
;;;###autoload
(defun mon-line-string-rotate-name-TEST ()
  "Test function for `mon-line-string-rotate-name'.\n
Key :all-tests-passed is non-nil if so.\n
:EXAMPLE\n\n\(mon-line-string-rotate-name-TEST\)\n
\(and \(cadr \(memq :all-tests-passed \(mon-line-string-rotate-name-TEST\)\)\)\)\n
:SEE-ALSO `*mon-testme-utils-xrefs*'.\n►►►"
  (let* (gthr (*standard-output*  gthr))
    (setq gthr                  
          (nconc
           (list (equal
                  (with-output-to-string
                    (mapc #'(lambda (x) (princ (concat (mon-line-string-rotate-name x) "\n" gthr)))
                          '(("George Charles Aid")("Thomas Pollock Anshutz")
                            ("Cecilia Beaux")("Frank Weston Benson")
                            ("Thomas Hart Benton")("Saul Bernstein")
                            ("George Biddle")("Gutzon Borglum"))))
                  (concat (mapconcat #'identity '("Aid (George Charles)" "Anshutz (Thomas Pollock)" 
                                                  "Beaux (Cecilia)" "Benson (Frank Weston)" "Benton (Thomas Hart)"
                                                  "Bernstein (Saul)" "Biddle (George)" "Borglum (Gutzon)") "\n")
                          "\n")))
           (mapcar #'(lambda (x)
                       (equal
                        (apply 'mon-line-string-rotate-name `(,(car x) ,(cadr x)))
                        (caddr x)))
                   '(("Elvis" nil "Elvis")
                     ("István Tisza" nil "Tisza (István)")
                     ("Thomas Pollock Anshutz" nil "Anshutz (Thomas Pollock)")
                     ("Thomas Pollock Anshutz" t ("Anshutz (Thomas Pollock)"))
                     (("Thomas Pollock Anshutz") t ("Anshutz (Thomas Pollock)"))))))
    (list :all-tests-passed (= (apply #'+ (mapcar #'mon-boolean-to-binary (not gthr))) 0))))


;;; ==============================
;;; :CHANGESET 2208
;;; :CREATED <Timestamp: #{2010-10-28T18:57:10-04:00Z}#{10434} - by MON KEY>
;;;###autoload
(defun mon-line-indent-from-to-col-TEST ()
  "Test function for `mon-line-indent-from-to-col'.\n
Key :all-tests-passed is non-nil if so.\n
:EXAMPLE\n\n\(mon-line-indent-from-to-col-TEST\)\n
\(and \(cadr \(memq :all-tests-passed \(mon-line-indent-from-to-col-TEST\)\)\)\)\n
:SEE-ALSO `*mon-testme-utils-xrefs*'.\n►►►"
  (let ((fndr  #'(lambda (y) (search-forward-regexp y nil t)))
        (mliftc-T-with
         (mapconcat #'identity 
                    ;; :NOTE Any whitespace below is significant!
                    '("emacsen.auto_apart      001           001\n"
                      "emacsen.rug_compat_42   00            00\n"
                      "emacsen.rug_compt_adorn 00            00\n"
                      "emacsen.cache_empire    080           080\n"
                      "emacsen.hashdelimiter   no-hash       no-hash\n"
                      "emacsen.rookie_romain   no value      no value") ""))
        (mliftc-T-for
         (mapconcat #'identity 
                    ;; :NOTE The whitespace below is significant!
                    '("emacsen.auto_apart      	001           		  001\n"
                      "emacsen.rug_compat_42   	00            		  00\n"
                      "emacsen.rug_compt_adorn 	00            		  00\n"
                      "emacsen.cache_empire    	080           		  080\n"
                      "emacsen.hashdelimiter   	no-hash       		  no-hash\n"
                      "emacsen.rookie_romain   	no value      		  no value") ""))
        mliftc-T-got)
    (setq mliftc-T-got     
          (with-temp-buffer
            (save-excursion 
              (insert mliftc-T-with))
            (mon-line-indent-from-to-col 24 32 (mon-g2be -1 t) (mon-g2be 1 t))
            (mon-line-indent-from-to-col 46 58 (mon-g2be -1 t) (mon-g2be 1 t))
            (buffer-substring-no-properties (mon-g2be -1 t) (mon-g2be 1 t))))
    `(:all-tests-passed ,(equal mliftc-T-got mliftc-T-for)
      :with-expressions ,'((mon-line-indent-from-to-col 24 32 st-pnt nd-pnt)
                           (mon-line-indent-from-to-col 46 58 st-pnt nd-pnt))
      :with-test-data   ,(concat (truncate-string-to-width mliftc-T-for 63) "{...}")
      :with-test-result ,(concat (truncate-string-to-width mliftc-T-got 63) "{...}"))))


;;; ==============================
;;; :CHANGESET 2208
;;; :CREATED <Timestamp: #{2010-10-28T20:16:19-04:00Z}#{10434} - by MON KEY>
;;;###autoload
(defun mon-line-strings-pipe-to-col-TEST ()
  "Test function for `mon-line-strings-pipe-to-col'.\n
Key :all-tests-passed-p is non-nil if so.\n
:EXAMPLE\n\n\(mon-line-strings-pipe-to-col-TEST\)\n
:SEE-ALSO `*mon-testme-utils-xrefs*'.\n►►►"
  (let ((with-names '("►" "William Gibson" "Bruce Sterling" 
                      "Dan Brown" "Neal Stephenson"
                      "Loyd Blankenship" "Erik Gordon Corley" "◄"))
        (with-ree (make-marker))
        (with-pad-to 12)
        with-reb)
    `(:all-tests-passed
      ,(equal (setq with-reb 
                    (with-temp-buffer
                      (save-excursion (insert (mapconcat #'identity  with-names "\n")))
                      (setq with-reb (save-excursion (1+ (search-forward-regexp "►"))))
                      (set-marker with-ree (save-excursion (- (search-forward-regexp "◄") 2)))
                      (set-marker-insertion-type with-ree t)
                      (mon-line-strings-pipe-to-col with-reb with-ree with-pad-to t)
                      (prog1 
                          (mon-buffer-sub-no-prop with-reb with-ree)
                        (set-marker with-ree nil))))
              (setq with-ree (mapconcat #'(lambda (pre)
                                            (concat (make-string with-pad-to 32) "| " pre))
                                        (mon-subseq with-names 1 7) "\n")))
      :with-expression ,`(mon-line-strings-pipe-to-col with-reb with-ree ,with-pad-to t)
      :with-results ,with-reb 
      :with-names ,(substring (setq with-names (mapconcat #'identity with-names "\n"))
                              2 (- (length with-names) 2)))))


;;; ==============================
;;; :CHANGESET 2208
;;; :CREATED <Timestamp: #{2010-10-28T21:09:57-04:00Z}#{10434} - by MON KEY>
;;;###autoload
(defun mon-line-string-insert-chars-under-TEST ()
  "Test function for `mon-line-string-insert-chars-under'.\n
Key :all-tests-passed-p is non-nil if so.\n
:EXAMPLE\n\n\(mon-line-string-insert-chars-under-TEST\)\n
:SEE-ALSO `*mon-testme-utils-xrefs*'.\n►►►"  
(let ((with-value '((nil 43 95 42) . "I will be followed below by "))
      with-check
      with-result)
    (with-temp-buffer
      (dolist (chk (car with-value) 
                   (progn
                     (setq with-result (mapconcat #'identity (nreverse with-result) "\n"))
                     (setq with-check  (mapconcat #'identity (nreverse with-check) "\n"))))
        (insert (cdr with-value) (char-to-string (or chk 61)))
        (mon-line-string-insert-chars-under chk)
        (push (delete-and-extract-region (mon-g2be -1 t) (mon-g2be 1 t)) with-result)
        (push (concat (cdr with-value) (char-to-string (or chk 61)) "\n"
                      (make-string 29 (or chk 61))) with-check)))
    `(:all-tests-passed ,(equal with-result with-check)
      :with-expressions ,`(mapcar #'(lambda (rslt)
                                     (list 'mon-line-string-insert-chars-under rslt))
                                 (car with-value))
      :with-result ,with-result
      :with-check  ,with-check)))

;;; ==============================
;;; :CHANGESET 2208
;;; :CREATED <Timestamp: #{2010-10-29T17:13:16-04:00Z}#{10435} - by MON KEY>
;;;###autoload
(defun mon-list-reorder-TEST ()
  "Test function for `mon-list-reorder'.\n
Key :all-tests-passed-p is non-nil if so.\n
:EXAMPLE\n\n\(mon-list-reorder-TEST\)\n
\(let \(\(gbc \(get-buffer-create \"*mon-list-reorder-TEST*\"\)\)\)
  \(with-current-buffer gbc \(erase-buffer\)\)
  \(pp-display-expression \(mon-list-reorder-TEST\)  \(buffer-name gbc\)\)\)\n
:SEE-ALSO `%mon-list-reorder', `*mon-testme-utils-xrefs*'.\n►►►"
 (let ((with-test
        ;; :ARGS                                     :EXPECT
        '(((nil nil t)                                nil)
          ((nil (t . nil) t)                          nil)
          ((nil (z z a b z c q w z) nil)              nil)
          (((z z a b z c q w z) nil t)                (z a b c q w))
          (((z z a b z c q w z) nil nil)              (z z a b z c q w z))
          (([z z a b z c q w z] nil t)                [z a b c q w])
          (([z z a b z c q w z] nil nil)              [z z a b z c q w z])
          (([z z a b z c q w z] (z z a b c q w) t)    [z a b c q w])
          (((2 6 3 2 1) (1 2 3 4 5 6) nil)            (1 2 3 6 2))
          (((q w b c s a w) (a b c q z w) nil)        (a b c q w s w))
          (((q w b c s a w) (a b c q z w) t)          (a b c q w s))
          (([a b w z w c] [a b c q z w] t)            [a b c z w])
          (([a b w z w c] [a b c q z w] nil)          [a b c z w w])
          (((a b w z w c) [a b c q z w] t)            (a b c z w))
          (((a b q z w c) [a b c q z w] nil)          (a b c q z w))
          (([a b w z w c] (a b c q z w) t)            [a b c z w])
          (([a b w z w c] (a b c q z w) nil)          [a b c z w w])
          (((q w [1 2 3] b c a w) (a b c q z w) t)    (a b c q w [1 2 3]))
          (((q w [1 2 3] b c a w) (a b c q z w) nil)  (a b c q w [1 2 3] w))
          (((q w [1 2 3] b c a w) (a b c q z w) nil)  (a b c q w [1 2 3] w))))
       with-result
       mlrdr-gthr)
   (dolist (mlrdr-D-1 with-test
                      (dolist (mlrdr-D-2
                               ;; :ARGS                                     :EXPECT
                               `(((,(make-bool-vector 3 t) (q . z) t) failed-succesfully)
                                 (((q  z) ,(make-bool-vector 3 t) t)  failed-succesfully)
                                 (([q  z] ,(current-buffer)  t)       failed-succesfully)
                                 (((a . b) (z z a b c q w)  t)        failed-succesfully)
                                 (((z z a b c q w) (q . z)  t)        failed-succesfully))
                               (setq mlrdr-gthr (nreverse mlrdr-gthr)))
                        (setq with-result 
                              (and (null (ignore-errors 
                                           (apply #'mon-list-reorder (car mlrdr-D-2))))
                                   (cadr mlrdr-D-2)))
                        (push `(:test-passed ,(equal with-result (cadr mlrdr-D-2))
                                :with-test   ,`(mon-list-reorder ,@(car mlrdr-D-2))
                                :with-result ,with-result
                                :with-expect ,(cadr mlrdr-D-2))
                              mlrdr-gthr)
                        (setq with-result nil)))
     (setq with-result
           (apply #'mon-list-reorder (car mlrdr-D-1)))
     (push `(:test-passed ,(equal with-result (cadr mlrdr-D-1))
             :with-test   ,`(mon-list-reorder ,@(car mlrdr-D-1))
             :with-result ,with-result
             :with-expect ,(cadr mlrdr-D-1))
           mlrdr-gthr)
    (setq with-result nil))
   (nconc `(:all-tests-passed 
            ,(zerop (apply #'+ 
                           (mapcar #'(lambda (chk-t) 
                                       (mon-booleanp-to-binary (not (cadr chk-t))))
                                   mlrdr-gthr))))
          mlrdr-gthr)))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-07-08T15:34:38-04:00Z}#{10274} - by MON>
;;;###autoload
(defun mon-regexp-clean-ulan-dispatch-chars-TEST (&optional kywd-str ndl-str)
  "Test function for variable `*regexp-clean-ulan-dispatch-chars*'.\n
When optional arg KYWD-STR is non-nil it is a regexp to which should
satisfy the predicate `string-match-p' with the the car of a sublist
in `*regexp-clean-ulan-dispatch-chars*'.  Default is:\n
 \".*:TEACHER-OF\"\n
When optional arg NDL-STR is non-nil it is a string to search for.
Default is as follows (not there are two trailing spaces after \"Eman\":\n
 \":TEACHER-OF Ivory, Percy van Eman\x20\x20
 (American painter and illustrator, 1883-1960) [500105044]\"\n
When KYWD-STR is non-ni and NDL-STR is ommitted signal an error.\n
When KYWD-STR is ommitted and NDL-STR is non-nil use NDL-STR's default.\n
When a match is made return value is a plist of the form:\n
 (:w-replace \"<STRING-AFTER-REPLACEMENT>\"
  :w-match   \"<STRING-THAT-MATCHED>\"
  :w-regexp  \"<REGEXP-THAT-MATCHED>\"
  :w-groups  \"<REGEXP-REPLACEMENT-MATCH-GROUPS>\")\n
:EXAMPLE\n\n\(mon-regexp-clean-ulan-dispatch-chars-TEST\)\n
\(mon-regexp-clean-ulan-dispatch-chars-TEST
 \".*:GRANDPARENT-WAS\"
 \(concat
 \":GRANDPARENT-WAS Van-Winkle, Pappy von  \"
 \"\(Dutch painter and illustrator, 1800-1903\) [500000010]\"\)\)\n
:USED-IN `naf-mode'.\n
:SEE-ALSO `mon-cln-ulan', `*mon-testme-utils-xrefs*'.\n►►►"
  (let* ((the-kwd (if kywd-str
                      (progn
                        (unless ndl-str 
                          (error (concat 
                                  ":FUNTION `mon-regexp-clean-ulan-dispatch-chars-TEST' "
                                  "-- arg KYWD-STR given but NDL-STR ommitted")))
                        kywd-str)
                    ".*:TEACHER-OF"))
         (the-ndl (or (and kywd-str ndl-str)
                      (concat 
                       ;; :NOTE The two trailing spaces  vv
                       ":TEACHER-OF Ivory, Percy van Eman  \n" 
                       "(American painter and illustrator, 1883-1960) [500105044]")))
         (rcudc-pop *regexp-clean-ulan-dispatch-chars*)
         fnd-tchr
         fnd-this)
    (while (not fnd-tchr)
      (if (null rcudc-pop)
          nil 
        (if (string-match-p  the-kwd (caar rcudc-pop))
            (setq fnd-tchr (car rcudc-pop))
          (pop rcudc-pop))))
    (if (not fnd-tchr)
        (error (concat
                ":FUNTION `mon-regexp-clean-ulan-dispatch-chars-TEST' "
                "-- could not satisfy predicate `string-match-p' w/ regexp: %S")
               the-kwd)
      (setq fnd-this
            ;; :NOTE Uncomment when debugging
            ;; (with-current-buffer (get-buffer-create "*regexp-clean-ulan-dispatch-chars*")
            ;; (display-buffer (current-buffer) t)
            (with-temp-buffer 
              (save-excursion (insert the-ndl))
               (when (search-forward-regexp (car fnd-tchr) nil t)
                (let ((mtch-got (match-string-no-properties 0)))
                  (replace-match (cadr fnd-tchr))
                  `(:w-replace ,(mon-buffer-sub-no-prop)
                    :w-match   ,mtch-got
                    :w-regexp  ,(car fnd-tchr) 
                    :w-groups  ,(cadr fnd-tchr)))))))))

;;; ==============================
;;; :CHANGESET 2208
;;; :CREATED <Timestamp: #{2010-11-05T16:39:34-04:00Z}#{10445} - by MON KEY>
;;;###autoload
(defun mon-set-buffer-substring-no-properties-TEST ()
  "Test function for `%mon-set-buffer-substring-no-properties', `%mon-set-buffer-substring'
Which in turn are requirements to make `buffer-substring-no-properties' `setf'able.
:EXAMPLE\n\n\(mon-set-buffer-substring-no-properties-TEST\)
:SEE-ALSO `*mon-testme-utils-xrefs*'.\n►►►"
  (with-current-buffer (get-buffer-create "*DEFSETF-BUF-NO-PROPS*")
    (erase-buffer)
    (emacs-lisp-mode)
    (save-excursion (insert "was: lots of lots and lots of bubba"))
    (display-buffer (current-buffer) t)
    (sit-for 2)
    (with-no-warnings
      (setf (buffer-substring (buffer-end 0) (buffer-end 1))
          (propertize "now: lots of lots and lots of bubba" 'font-lock-face 'bold)))
    (sit-for 2)
    
    (prog1 
        (when
            ;; Rogue CL compiler warning. `setf' forms like this shouldn't warn...
             (with-no-warnings 
               (equal
                (setf (buffer-substring-no-properties (buffer-end 0) (buffer-end 1)) 
                      ;; won't get inserted 
                      (propertize "Are there no bold props here?" 'font-lock-face 'bold))
                "Are there no bold props here?"))
          "Good, no bold props were seen")
      (unwind-protect 
          (sit-for 2)
        (kill-buffer (current-buffer))))))


;;; ==============================
;;; :CHANGESET 2208
;;; :CREATED <Timestamp: #{2010-11-08T13:58:27-05:00Z}#{10451} - by MON KEY>
;;;###autoload
(defun mon-plist-keys-TEST ()
  "Test function for `mon-plist-keys'.\n
:EXAMPLE\n\n\(mon-plist-keys-TEST\)\n
:SEE-ALSO `mon-plist-values-TEST', `*mon-testme-utils-xrefs*'.\n►►►"
  (let ((w-plist (mon-alphabet-as-type 'plistD->num))
        gthr)
    (push `(:plist-orig ,w-plist) gthr)
    (push `(:plist-keys ,(mon-plist-keys w-plist)) gthr)
    (setq gthr `(,:all-tests-passed
                 ,(equal (cadr (assq :plist-keys gthr))
                         (loop for chk in w-plist
                               if (not (numberp chk)) collect chk))
                 ,@gthr))))

;;; ==============================
;;; :CHANGESET 2208
;;; :CREATED <Timestamp: #{2010-11-08T13:42:10-05:00Z}#{10451} - by MON KEY>
;;;###autoload
(defun mon-plist-values-TEST ()
  "Test function for `mon-plist-values'
Keyword :all-tests-passed is non-nil if so.\n
:EXAMPLE\n\n\(mon-plist-values-TEST\)\n
:SEE-ALSO `mon-plist-keys-TEST', `*mon-testme-utils-xrefs*'.\n►►►"
  (let ((chk-rtn (mon-alphabet-as-plistD->num)) ;; (mon-alphabet-as-type 'plistD->num)
        rtn)
    (push `(,chk-rtn                    . :plist-orig) rtn)
    (push `(,(mon-plist-keys chk-rtn)   . :plist-keys) rtn)
    (push `(,(mon-plist-values chk-rtn) . :plist-values) rtn)
    ;; Succesfully signaled an error when not `mon-list-proper-p':
    (push `(,(null (ignore-errors (mon-plist-values (append chk-rtn 'a))))
            . :errored-when-not-proper-list) rtn)
    ;; Succesfully signaled an error when not length not `evenp':
    (push `(,(null (ignore-errors (mon-plist-values (append chk-rtn '(a)))))
            . :errored-when-not-evenp) rtn)
    (setq rtn 
          `(:all-tests-passed
            ,(and 
              (equal (car (rassq :plist-values rtn)) (number-sequence 1 26))
              (equal (car (rassq :plist-keys rtn)) (mon-plist-keys chk-rtn))
              (equal (car (rassq :plist-orig rtn)) (mon-alphabet-as-plistD->num))
              (car (rassq :errored-when-not-proper-list rtn))
              (car (rassq :errored-when-not-evenp rtn)))
            ,@rtn))))

;; (mon-plist-values-TEST)

;;; ==============================
;;;  mhprsdt
;;; :CREATED <Timestamp: #{2010-02-11T20:48:23-05:00Z}#{10065} - by MON KEY>
;;;###autoload
(defun mon-help-propertize-regexp-symbol-defs-TEST ()
  "Helper function for `mon-help-regexp-symbol-defs-TEST'.\n
Propertize symbols matched by regexp `*regexp-symbol-defs*'.\n
:SEE-ALSO `mon-help-overlay-result', `*mon-testme-utils-xrefs*'.\n►►►"
  (let ((fnf ;; `font-lock-function-name-face'
         '(defun defun* defmacro defmacro* defsubst defsubst* defclass defadvice
           defgeneric defmethod defsetf defalias))
        (ftf ;; `font-lock-type-face'
         '(defclass deftheme defgroup deftype defpackage defstruct))
        (fvf ;; `font-lock-variable-name-face'
         '(defface defconstant defconst defvar defparameter defvaralias))
        (next-ms #'(lambda (n) (search-forward-regexp (concat "match-string" n ": ") nil t)))
        (got-sym-tp #'(lambda (fc) (let ((mhprsdt-botap ;; :WAS (bounds-of-thing-at-point 'symbol)))
                                          (save-match-data (bounds-of-thing-at-point 'symbol))))
                                     ;;(with-syntax-table emacs-lisp-mode-syntax-table <- fails why??
                                     (unless (null mhprsdt-botap)
                                       (put-text-property (car mhprsdt-botap) (cdr mhprsdt-botap) 'face fc)))))
        ;; got-sym-tp 
        got-def bro)
    (unwind-protect
         (when (buffer-local-value 'buffer-read-only (current-buffer))
           (set 'buffer-read-only nil)
           (setq bro t))
      (set-syntax-table emacs-lisp-mode-syntax-table)
      (while (funcall next-ms "1")
        (setq got-def (symbol-at-point))
        (funcall got-sym-tp 'font-lock-keyword-face)
        (funcall next-ms "3")
        (when ;; :WAS (looking-at "'") (forward-char))  
            (eq (char-after (point)) 39)(forward-char))
        (cond ((memq got-def fnf)(funcall got-sym-tp 'font-lock-function-name-face))
              ((memq got-def ftf)(funcall got-sym-tp 'font-lock-type-face))
              ((memq got-def fvf)(funcall got-sym-tp 'font-lock-variable-name-face))))
      (when bro (set 'buffer-read-only t)))))

;;; ==============================
;;; :RENAMED `mon-help-regexp-symbol-defs-TEST' -> `mon-help-regexp-symbol-defs-TEST'
;;; :MODIFICATIONS <Timestamp: #{2010-02-24T20:24:30-05:00Z}#{10084} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-09-02T16:11:07-04:00Z}#{09363} - by MON KEY>
;;;###autoload
(defun mon-help-regexp-symbol-defs-TEST (&optional dis-p do-big-regexp)
  "Return overlays for matches of regexp in region.\n
When optional arg DIS-P is non-nil or when called-interactively return formatted
results to the buffer named *REGEXP-SYMBOL-DEFS-REPORT*.\n
When optional arg DO-BIG-REGEXP is non-nil use regexps in the 
variable `*regexp-symbol-defs-big*'. Default is `*regexp-symbol-defs*'.\n
:EXAMPLE\n\n\(mon-help-regexp-symbol-defs-TEST t\)\n
\(mon-help-regexp-symbol-defs-TEST t t\)\n
►
\(defun some-function \(&optional optional\)
\(defvar som-bq-list `\(
\(defun some-function-22 \(&optional optional\)
\(defvar som-var t
\(defun *some/-symbol:->name<-2* \(somevar
\(defmacro some-macro \(\)
\(defvaralias 'som-var 'som-other-var
\(defmacro some-macro*:22 \(&rest\)
\(defvar som-double-list \(\(
\(defun *some/-symbol:->name<-2* \(somevar
\(defvar *some-var* 'var
\(defun *some/-symbol:->name<-2* 'somevar
\(defconst som-const \"somestringval\"
\(defun* *some/-symbol:->name<-2* \(&
\(defmacro some-macro*:22 \(&rest\)
\(defun *some/-symbol:->name<-2* \(somevar
\(defvar *some-var* 'var
\(defun *some/-symbol:->name<-2* 'somevar
\(defmacro some-macro \(\)
\(defvar *som-var* nil
\(defmacro* some-macro*:22 \(&rest\)
\(defcustom reb-re-syntax 'read
\(defun* *some/-symbol:->name<-2* \(somevar
\(defsubst *some/subtst-symbol:->name<-2* \(
\(defface *some/-symbol:->name<-2* \(\(
\(defsubst* *some/subtst-symbol:->name<-2* \(
\(defcustom *some/-custom-symbol:->name<-2* 'somecustom
\(defconst *some/-symbol:->name<-2* \(someconst
\(defface *some/-face-symbol:->name<-2* \(someface
\(defgroup *some/-group-symbol:->name<-2* \(somegroup
\(deftheme *some/-theme-symbol:->name<-2* \(sometheme
◄

:SEE-ALSO `mon-help-propertize-regexp-symbol-defs-TEST', `mon-help-overlay-result',
`lisp-font-lock-keywords', `lisp-font-lock-keywords-1', `lisp-font-lock-keywords-2',
`*mon-testme-utils-xrefs*'.\n►►►"
  (interactive "p")
  (eval-when-compile (require 'boxquote))
  (let ((botp          #'(lambda () `(,(line-beginning-position) . ,(line-end-position))))
        (mhrsdt-mhor   #'(lambda (bd) (mon-help-overlay-result (car bd) (cdr bd) 78)))
        (srcher        #'(lambda (srch bnds) (search-forward-regexp srch bnds t)))        
        (ntst-maybe    #'(lambda (m-fncn m-grp) 
                           (let ((nmp (funcall m-fncn m-grp)))
                             (if (numberp nmp) (number-to-string nmp) "!no-match!"))))
        ;; :IS-STILL 
        (srched (if do-big-regexp *regexp-symbol-defs-big* *regexp-symbol-defs*))
        ;; (srched *tt-rsd*) ;; :TEMP-TESTING
        (mhrsdt-bnd-s (make-marker))
        (mhrsdt-bnd-e (make-marker))
        (mhrsdt-step 1)
        match-report mhrsdt-cnt )
    (save-excursion
      (funcall srcher "^►" nil)
      (set-marker mhrsdt-bnd-s (point))
      (funcall srcher "◄$" nil)
      (set-marker mhrsdt-bnd-e (point)))
    (setq mhrsdt-cnt (- (line-number-at-pos (- (marker-position mhrsdt-bnd-e) 2))
                        (line-number-at-pos (marker-position mhrsdt-bnd-s))))
    (save-excursion
      ;;(goto-char mhrsdt-bnd-s)
      (goto-char (1+ mhrsdt-bnd-s))
      (while (< mhrsdt-step mhrsdt-cnt)
        (funcall srcher srched mhrsdt-bnd-e)
        (funcall mhrsdt-mhor (funcall botp))
        (push (concat 
               "------------------------------------\n"
               "Match iteration: " (format "%d\n" mhrsdt-step)
               ;; `font-lock-keyword-face'
               "\nmatch-string1: " (match-string-no-properties 2) " start2: " 
               (funcall ntst-maybe 'match-beginning 2) " end2: " 
               (funcall ntst-maybe 'match-end 2) "\nmatch-string3: "
               ;; `font-lock-type-face', `font-lock-variable-name-face', `font-lock-function-name-face'
               (match-string-no-properties 3)" start3: " 
               (funcall ntst-maybe 'match-beginning 3) " end3: " 
               (funcall ntst-maybe 'match-end 3) "\nmatch-string4: "
               (match-string-no-properties 4) " start4: " 
               (funcall ntst-maybe 'match-beginning 4)  " end4: " 
               (funcall ntst-maybe 'match-end 4)"\n")
              match-report)
        (setq mhrsdt-step (1+ mhrsdt-step))))
    ;; (push          (match-report
    ;; (setq match-report (nreverse match-report))
    (setq srched  ;; :WAS (buffer-substring-no-properties (1- mhrsdt-bnd-s) mhrsdt-bnd-e))
          (mon-buffer-sub-no-prop (1- mhrsdt-bnd-s) mhrsdt-bnd-e))
    (setq srched 
          (with-temp-buffer
            (save-excursion
              (princ srched (current-buffer)))
            (if (fboundp 'boxquote-region)
                (boxquote-region (mon-g2be -1 t) (mon-g2be 1 t))
              (comment-region (mon-g2be -1 t) (mon-g2be 1 t)))
            ;; :WAS (goto-char (buffer-end 0))
            (mon-g2be -1) 
            (princ ";;; Regexp Match Report for the following lines:\n" (current-buffer))
            ;;:WAS (buffer-substring-no-properties (buffer-end 0)(buffer-end 1)) ))
            (mon-buffer-sub-no-prop)))
    (setq match-report (nreverse match-report))
    (push srched match-report)
    (prog1
        (setq match-report 
              (concat (mapconcat #'identity match-report "\n") 
                      "------------------------------------\n"))
      (when (or dis-p)
        (with-current-buffer (get-buffer-create "*REGEXP-SYMBOL-DEFS-REPORT*")
          (unwind-protect 
               (let ((buffer-read-only nil))
                 (erase-buffer)
                 (save-excursion (princ match-report (current-buffer)))
                 (mon-help-propertize-regexp-symbol-defs-TEST)
                 (display-buffer (current-buffer) t t))
            (set (make-local-variable 'buffer-read-only) t)))))))
;;
;;,---- :UNCOMMENT-BELOW-TO-TEST
;;|
;;| (mon-help-regexp-symbol-defs-TEST t)
;;|
;;|►
;;|(defun some-function (&optional optional)
;;|(defvar som-bq-list `(
;;|(defun some-function-22 (&optional optional)
;;|(defvar som-var t
;;|(defun *some/-symbol:->name<-2* (somevar
;;|(defmacro some-macro ()
;;|(defvaralias 'som-var 'som-other-var
;;|(defmacro some-macro*:22 (&rest)
;;|(defvar som-double-list ((
;;|(defun *some/-symbol:->name<-2* (somevar
;;|(defvar *some-var* 'var
;;|(defun *some/-symbol:->name<-2* 'somevar
;;|(defconst som-const "somestringval"
;;|(defun* *some/-symbol:->name<-2* (&
;;|(defmacro some-macro*:22 (&rest)
;;|(defun *some/-symbol:->name<-2* (somevar
;;|(defvar *some-var* 'var
;;|(defun *some/-symbol:->name<-2* 'somevar
;;|(defmacro some-macro ()
;;|(defvar *som-var* nil
;;|(defmacro* some-macro*:22 (&rest)
;;|(defcustom reb-re-syntax 'read
;;|(defun* *some/-symbol:->name<-2* (somevar
;;|(defsubst *some/subtst-symbol:->name<-2* (
;;|(defface *some/-symbol:->name<-2* ((
;;|(defsubst* *some/subtst-symbol:->name<-2* (
;;|(defcustom *some/-custom-symbol:->name<-2* 'somecustom
;;|(defconst *some/-symbol:->name<-2* (someconst
;;|(defface *some/-face-symbol:->name<-2* (someface
;;|(defgroup *some/-group-symbol:->name<-2* (somegroup
;;|(deftheme *some/-theme-symbol:->name<-2* (sometheme
;;|◄
;;`----



;;; ==============================
;;; :REQUIRES `mon-string-justify-left' <- mon-utils.el
;;; :CREATED <Timestamp: #{2009-11-21T18:45:40-05:00Z}#{09476} - by MON>
;;;###autoload
(defun mon-help-propertize-tags-TEST ()
  "Test function to verify `mon-help-propertize-tags' is properly propertizing.\n
:EXAMPLE\n\n\(mon-help-propertize-tags-TEST\)\n
Test the following regexps:\n
 `*regexp-mon-doc-help-comment-tags*'
 `*regexp-mon-doc-help-docstring-tags*'
 `*regexp-mon-doc-help-meta-tags*'
 `*regexp-mon-doc-help-pointer-tags*'
 `*regexp-mon-doc-help-docstring-tags-URL*'
 `*regexp-mon-doc-help-docstring-tags-DYNAMIC*'
 `*regexp-mon-doc-help-docstring-tags-TABLES*'
 `*regexp-mon-doc-help-docstring-tags-URL*'
 `*regexp-mon-doc-help-builtin-dynamic-tags*'
 `*regexp-mon-doc-help-builtin-static-tags*'\n
Test the following keywords in:\n
 `*mon-help-mon-tags-alist*', `*mon-help-custom-faces-builtins-tags*'\n
Test font-locking of the following faces:\n
 `mon-help-URL-wrap-tag', `mon-help-DYNATAB-tag', `mon-help-INNER-KEY-tag',
 `mon-help-KEY-tag', `mon-help-META-tag', `mon-help-PNTR-tag',
 `mon-help-COMMENT-tag', `mon-help-BUILTIN-tag'\n
:SEE-ALSO `mon-help-mon-tags', `mon-help-insert-tags',
`*mon-testme-utils-xrefs*'.\n►►►"
  (let ((tags-divd (concat "\n" (make-string 68 95) "\n"))
         test-mhpt)
    (setq test-mhpt
          (with-temp-buffer
            (princ
             (mapconcat 
              #'identity 
              `(,tags-divd
                ";;; (*regexp-mon-doc-help-docstring-tags* 'mon-help-KEY-tag) \"light steel blue\"\n"
                ,@(cadr (assoc 'docstr-tags *mon-help-mon-tags-alist*))
                ":ALIASED-BY" ":CALLED-BY" ":EXAMPLE" ":FACE-DEFINED-IN" ":FACE-DOCUMENTED-IN"
                ":FILE" ":IDIOM" ":NOTE" ":SEE" ":SEE-ALSO" ":SOURCE" ":USED-BY"
                ,tags-divd
                ";;; (*regexp-mon-doc-help-pointer-tags* 'mon-help-PNTR-tag) \"powder blue\"\n"
                "->" ";->" "; ->" "=>" ";=>" "; =>" "-->" ";-->" "; -->" "--->" "<--" "<--" "; <--"
                "<---" "<--" ";<--" "; <--" "<---" "==>" ";==>" "; ==>" "===>"
                ,tags-divd
                ";;; (*regexp-mon-doc-help-docstring-tags-TABLES*  'mon-help-DYNATAB-tag) \"cadet blue\"\n"
                "| :SOME-SECTIONA | :SOME-SECTIONB | :SOME-SECTIONC |"
                ,tags-divd
                ";;; (*regexp-mon-doc-help-meta-tags* 'mon-help-META-tag)\n"
                ,(replace-regexp-in-string "$" " "
                                           (mon-string-justify-left 
                                            (mapconcat #'identity 
                                                       (cadr (assoc 'meta-tags *mon-help-mon-tags-alist*))
                                                       " ") 68 1))
                ,tags-divd
                ";;; (*regexp-mon-doc-help-docstring-tags-DYNAMIC* 'mon-help-DYNATAB-tag) \"cadet blue\"\n"
                ";; :FUNCTION-LISTS" ";; :SEARCHING" ";; :HELP" ";; :TIME" ";; :EIEIO" ";; :CL"
                ";; :ASCII-ART" ";; :PRESENTATION" ";; :KEYS" ";; :CHAR-TABLES" ";; :RECIPES"
                ";; :INTROSPECTION-AND-UTILITY" ";; :VARIABLES" ";; :DOUBLED-1 :SEE" ";; :DOUBLED-2 :SEE-ALSO"
                ,tags-divd
                ";;; (*regexp-mon-doc-help-comment-tags* mon-help-COMMENT-tag) \"DarkSlateGray3\"\n"
                ,@(cadr (assoc 'comment-tags *mon-help-mon-tags-alist*))
                ,tags-divd
                ";;; (*regexp-mon-doc-help-builtin-dynamic-tags* mon-help-BUILTIN-tag) \"SteelBlue\""
                ";;; (*regexp-mon-doc-help-builtin-static-tags* mon-help-BUILTIN-tag) \"SteelBlue\"\n"
                ,(replace-regexp-in-string "$" " "
                                           (mon-string-justify-left 
                                            (mapconcat #'identity *mon-help-custom-faces-builtins-tags* " ") 68 1 ))
                " :bubba" " :more-bubba" " :another-bubba" ;; Make sure dynamic-tags are matching
                ,tags-divd
                ";;; (*regexp-mon-doc-help-docstring-tags-URL* 'mon-help-URL-wrap-tag) \"LightSkyBlue\"\n"
                ":SEE (URL `http://www.IWasArpanet.com/i-am-really-gopher.html'\)"
                "some surrounding (URL `http://www.ThisIsNotMilnet.com/not-for-u.html'\) text"
                "(URL `http://www.IamTheInterWeb.com/wow-i-can-blah.htm'\)"
                "(URL `http://www.IamTheInterTubes.com/now-blah-is-blahging.htm'\)"
                ) "\n")
             (current-buffer))
            (mon-help-propertize-tags
             '(*regexp-mon-doc-help-comment-tags*           0 mon-help-COMMENT-tag)
             '(*regexp-mon-doc-help-docstring-tags-URL*     2 mon-help-URL-wrap-tag)
             '(*regexp-mon-doc-help-docstring-tags-URL*     4 mon-help-URL-wrap-tag)
             '("^_\\{68,68\\}$"                             0 mon-help-INNER-KEY-tag)
             '(*regexp-mon-doc-help-builtin-dynamic-tags*   1 mon-help-BUILTIN-tag)
             '(*regexp-mon-doc-help-builtin-static-tags*    0 mon-help-BUILTIN-tag))
            (buffer-string)))
    (mon-help-temp-docstring-display test-mhpt "*MON-PROPERTIZE-TAGS-TEST*")))
;;
;;; :TEST-ME (mon-help-propertize-tags-TEST)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-27T14:32:18-05:00Z}#{10086} - by MON KEY>
;;;###autoload
(defun mon-help-keys-wikify-TEST ()
  "Test function for `mon-help-keys-wikify' helper functions.\n
Return cumulative result of evaluating:\n
 `mon-help-keys-wikify', `mon-help-keys-wikify-anchors',
 `mon-help-keys-wikify-heading'\n
Return value displayed in buffer named \"*MON-HELP-KEYS-WIKIFY-TEST*\".\n
:SEE `mon-help-keys-wikify' for details of return value format.\n
:EXAMPLE\n\n(mon-help-keys-wikify-TEST)\n
:SEE-ALSO `*mon-help-reference-keywords*', `mon-help-keys',
`mon-help-escape-for-ewiki', `mon-help-unescape-for-ewiki', 
`mon-help-key-functions', `*mon-testme-utils-xrefs*'.\n►►►"
  (let ((mhkwh (mon-help-keys-wikify)))
    (setq mhkwh (mon-help-keys-wikify-heading `(,(cadr mhkwh) ,(caddr mhkwh))
                                              2 "Keybinding Table Sections"))
    (with-current-buffer (get-buffer-create "*MON-HELP-KEYS-WIKIFY-TEST*")
      (erase-buffer)
      (save-excursion (prin1 mhkwh (current-buffer)))
      (pp-buffer)
      (display-buffer (current-buffer) t))))

;;; ==============================
;;; :CHANGESET 1727
;;; :CREATED <Timestamp: #{2010-05-13T13:14:12-04:00Z}#{10194} - by MON KEY>
;;;###autoload
(defun mon-drive-transfer-template-TEST (&optional log-dest-test)
  "Test function for `mon-drive-transfer-template-cln'.\n
Return results in new buffer-name'd \"*MON-DRIVE-TRANSFER-CLN-TEST*\".
When optional arg LOG-DEST-TEST is non-nil return results of evaluating
`mon-drive-transfer-template-cln-log-dest' instead.\n
:SEE-ALSO `mon-drive-transfer-template-cln-all',
`mon-insert-drive-transfer-template', `*mon-drive-transfer-template*',
`*mon-drive-transfer-template-src-dest-log*',
`mon-drive-transfer-template-subst-src-dest-log',
`*mon-testme-utils-xrefs*'.\n►►►"
  (with-current-buffer (get-buffer-create "*MON-DRIVE-TRANSFER-CLN-TEST*")
    (erase-buffer)
    (if log-dest-test
        (let ((mdtcle (concat (mon-drive-transfer-template-subst-src-dest-log)
                              (mon-drive-transfer-template-subst-src-dest-log t))))
          (save-excursion (insert mdtcle))
          (mon-drive-transfer-template-cln-log-dest
           "/mnt/frm-this-drv" 
           "/mnt/to-this-other-drv/subdir" 
           "/home/me/log-this-to-here"))
      (mon-drive-transfer-template-cln-all
       "/mnt/frm-this-drv" "/mnt/to-this-other-drv/subdir"  
       "/home/me/log-this-to-here" "/mnt/to-this-other-drv" 
       "bubba-user" "bubba-group"))
    (display-buffer (current-buffer) t)))
;;
;;; :TEST-ME (mon-drive-transfer-template-TEST)
;;; :TEST-ME (mon-drive-transfer-template-TEST t)

;;; ==============================
;;; :NOTE Don't remove from this file. Mirrors the defition in:
;;; :FILE mon-site-local-defaults.el
;;; :CREATED <Timestamp: #{2010-02-10T16:32:23-05:00Z}#{10063} - by MON KEY>
;;;###autoload
(defun mon-build-mon-emacsd-example (&optional bind-var force-bind)
  "Return an alist of example key value pairs suitable for `*mon-emacsd*'.\n
When `bind-var' is non-nil if `*mon-emacsd*' is unbound bind it.\n
When `force-bind' is non-nil force the binding even if `*mon-emacsd*' bound.\n
:EXAMPLE\n\n\(mon-build-mon-emacsd-example\)\n
\(assoc 1 \(mon-build-mon-emacsd-example\)\)\n
\(nth 8 \(assoc 1 \(mon-build-mon-emacsd-example\)\)\)\n
\(assoc 5 \(mon-build-mon-emacsd-example\)\)\n
\(assoc 'IS-USER-4-P \(cadr \(assoc 5 \(mon-build-mon-emacsd-example\)\)\)\)\n
\(assoc \(cadr \(nth 3 \(cadr \(assoc 5 \(mon-build-mon-emacsd-example\)\)\)\)\)\n
   \(mon-build-mon-emacsd-example\)\)\n\n
:SEE-ALSO `mon-get-mon-emacsd-paths', `mon-build-misc-path-example'.
`mon-build-user-name-example', `*mon-emacsd*', `set-emacs-root',
`*mon-testme-utils-xrefs*'.\n►►►"
  (let (gthr-emacsd usr-map)
    (dotimes (p 5) 
      (unless (= p 0)
        (push `(,p ,@(nreverse (do* ((j 0 (1+ j))
                                      (k (format "<USER-%s-EMACSD-PATH-Nth-%d>" p j)
                                         (format "<USER-%s-EMACSD-PATH-Nth-%d>" p j))
                                      (l () (cons k l)))
                                     ((>= j 14) l))))
              gthr-emacsd)
        (push `(,(car (read-from-string (format "IS-USER-%d-P" p))) ,p) usr-map)))
    (setq gthr-emacsd (nreverse gthr-emacsd))
    (setq gthr-emacsd (append gthr-emacsd `((5 ,(nreverse usr-map)))))
    (cond (bind-var (unless (and (intern-soft "*mon-emacsd*" obarray)  ;; *IS-MON-OBARRAY*
                                 (bound-and-true-p *mon-emacsd*))
                      (setq *mon-emacsd* gthr-emacsd)))
          (force-bind (setq *mon-emacsd* gthr-emacsd))
          (t gthr-emacsd))))
;; 
;;; :TEST-ME (mon-build-mon-emacsd-example)
;;; :TEST-ME (assoc 1 (mon-build-mon-emacsd-example))
;;; :TEST-ME (nth 8 (assoc 1 (mon-build-mon-emacsd-example)))
;;; :TEST-ME (assoc 5 (mon-build-mon-emacsd-example))
;;; :TEST-ME (assoc 'IS-USER-4-P (cadr (assoc 5 (mon-build-mon-emacsd-example))))
;;; :TEST-ME (assoc (cadr (nth 3 (cadr (assoc 5 (mon-build-mon-emacsd-example)))))
;;;             (mon-build-mon-emacsd-example))

;;; ==============================
;;; :NOTE Don't remove from this file. Mirrors the defition in:
;;; :FILE mon-site-local-defaults.el
;;; :MODIFICATIONS <Timestamp: #{2010-03-24T16:52:01-04:00Z}#{10123} - by MON KEY>
;;; :CREATED <Timestamp: #{2010-02-10T14:19:41-05:00Z}#{10063} - by MON KEY>
;;;###autoload
(defun mon-build-misc-path-example (&optional bind-var force-bind)
  "Return pre-formattedkey value pairs for use with `*mon-misc-path-alist*'.\n
When `bind-var' is non-nil if `*mon-misc-path-alist*' is unbound, bind it with
return value.\n
When `force-bind' is non-nil force the binding even if `*mon-misc-path-alist*'
is already bound.\n
:EXAMPLE\n\n\(mon-build-misc-path-example\)\n
\(assoc 'the-1-path \(mon-build-misc-path-example\)\)\n
\(assoc 'the-emacs-vars \(mon-build-misc-path-example\)\)\n
\(nth 3 \(assoc 'the-sub \(mon-build-misc-path-example)\)\)\n
:SEE-ALSO `mon-get-mon-emacsd-paths', `mon-build-mon-emacsd-example',
`mon-build-user-name-example', `mon-get-env-vars-emacs',
`mon-get-env-vars-symbols', `mon-get-env-vars-strings',
`*mon-testme-utils-xrefs*'.\n►►►"
  (let (pth egs)
    (do ((i 1 (1+ i)))
        ((> i 10) i)
      (push `(,(car (read-from-string  (concat "the-" (number-to-string i) "-path")))
              ,(concat "<PATHSTRING-" (number-to-string i) ">")) pth))
    (setq egs (nreverse pth))
    (setq pth nil)
    (do ((su 1 (1+ su)))
        ((> su 10) su)
      (push `(,(car (read-from-string  (concat "the-sub-" (number-to-string su))))
              ,(concat "<SUB-PATHSTRING-1-" (number-to-string su) ">")
              ,(concat "<SUB-PATHSTRING-2-" (number-to-string su) ">"))
            pth))
    (setq pth `((the-sub ,@(nreverse pth))))
    (setq egs (append egs pth))
    (setq pth nil)
    ;; <Timestamp: #{2010-06-23T15:54:36-04:00Z}#{10253} - by MON KEY>
    ;; :NOTE Following lists are supposed to get pushed onto the local var `egs'. 
    ;; However, Emacs 23.2's backquote template can become broken on quoted
    ;; lists with , character.  And no matter what we do the list is formed as: 
    ;; "\, (form to splice)" <- note the whitespace between comma and opening paren.
    ;; All of the reasonable workarounds I tried failed with even \"best\"
    ;; solution causing the reader to return a cons instead of a proper
    ;; list.... IOW, waiting for awhile before incorporating the example forms
    ;; below. Best I can tell thh fix was checked into trun a few days ago.
    ;; :SEE Emacs bazaar trunk revno: 100617 timestamp:
    ;; Fri 2010-06-18 10:05:43 -0400 Which ads a variable
    ;; `edebug-read-backquote-level' to tweak how comma constructs are
    ;; read. e.g. the docstring: 
    ;; 
    ;; "This controls how we read comma constructs"
    ;; 
    ;; '(the-gnu-case-path
    ;;   ,(case (car (mon-gnu-system-conditionals))
    ;;      (1 "/home/gnu-user1/dir1")
    ;;     (2 "/home/gnu-user1/dir1")))
    ;;
    ;; '(the-user-case-path
    ;;  ,(case  (mon-user-name-conditionals t)
    ;;     (IS-MON-P-GNU    "/mnt/SOME-MOUNT")     
    ;;     (IS-BUG-P-REMOTE "//some-network-host")))
    ;;
    ;; '(the-user-case-lambda-pth
    ;;  ,(case (mon-user-name-conditionals t)
    ;;     (IS-MON-P-GNU     #'(lambda ()
    ;;                           (expand-file-name "SOME-MON-FILE" (getenv "HOME"))))
    ;;     (IS-BUG-P         #'(lambda () 
    ;;                           (expand-file-name  "SOME-BUG-FILE" (getenv "HOME"))))))
    ;;
    ;; '(the-system-case-lambda-pth
    ;;  ,(case (mon-system-type-conditionals t)
    ;;     (IS-GNU-P #'(lambda ()
    ;;                   (expand-file-name "SOME-GNU-FILE" user-emacs-directory)))
    ;;     (IS-W32-P #'(lambda () 
    ;;                   (expand-file-name "SOME-GNU-FILE" user-emacs-directory)))))
    (let ((var-cnt 0))
      (dolist (E '(EMC_BIN EMC_CUR EMC_PTH EMC_REPO EMC_W32 EMC_GNUW32
                           EMACS_LAUNCH EMCS_DUMMY1 EMCS_DUMMY2)
                 (setq pth `((the-emacs-vars ,(nreverse pth)))))
        (incf var-cnt)
        (push (car (read-from-string (format "%S-%d" E var-cnt))) pth)))
    (setq egs (append egs pth))    
    (cond (bind-var (unless (and (intern-soft "*mon-misc-path-alist*" obarray) ;; *IS-MON-OBARRAY*
                                 (bound-and-true-p *mon-misc-path-alist*))
                      (setq *mon-misc-path-alist* egs)))
          (force-bind (setq *mon-misc-path-alist* egs))
          (t egs))))
;;
;;; :TEST-ME (assoc 'the-1-path (mon-build-misc-path-example))
;;; :TEST-ME (assoc 'the-emacs-vars (mon-build-misc-path-example))
;;; :TEST-ME (nth 3 (assoc 'the-sub (mon-build-misc-path-example)))

 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-02-10T17:17:59-05:00Z}#{10063} - by MON KEY>
;;;###autoload
(defun mon-build-user-name-example (name-count &optional w-this-var bind-var force-bind)
  "Return pre-formatted list of username  pairs for use with mon packages.\n
User names are those with the format:\n
 `*BUG-NAME*', `*MON-NAME*', `*MON-ORG-NAME*', etc.\n
When `bind-var' is non-nil if `' is unbound bind it.\n
When `force-bind' is non-nil force the binding even if `' bound.\n
Elements of the list might have a form such as this:\n
 \(\(1 \"Short-First Last\"\)\n \(2 \"First\"\) \ \(3 \"FML\"\) 
  \(4 \"Full-First Last\"\) \n \(5 \"fl\"\) \n \(6 \"MONIKER NAME\"\) 
  \(7 \"MONIKER\"\) \n \(8 \"MONIKER_NAME\"\) \n \(9 \"moniker_name\"\)\)\n
:EXAMPLE\n\n(mon-build-user-name-example 5)\n
\(mon-build-user-name-example-TEST\)\n
:SEE-ALSO `mon-build-user-name-example-TEST', `mon-get-mon-emacsd-paths',
`mon-build-misc-path-example' `mon-build-mon-emacsd-example',
`*mon-testme-utils-xrefs*'.\n►►►"
  (let ((wonky-user (mon-string-wonkify
                     (concat (if w-this-var 
                                 (cond ((equal (symbol-name w-this-var) "*MON-NAME*") 
                                        user-login-name)
                                       ((equal (symbol-name w-this-var) "*BUG-NAME*") 
                                        (format "BUG-%s" user-login-name))
                                       (t "some-name"))
                                 user-login-name)
                             (when (> (length user-full-name) 0)
                               (concat "-" (length user-full-name)))) name-count))
        wonky
        (wonky-step 0))
    (while wonky-user
      (setq wonky-step (1+ wonky-step))
      (push `(,wonky-step ,(format "<%s-%s-%d>" 
                                   (pop wonky-user) 
                                   (car (mon-string-wonkify "NAMEFORM" 1)) 
                                   wonky-step)) 
            wonky))
    (setq wonky (nreverse wonky))
    ;; Don't allow MON to force-bind `*MON-NAME*' or `*BUG-NAME*' by accident : )
    (if (and force-bind 
             ;; :WAS (or (eq w-this-var '*MON-NAME*) (eq w-this-var '*BUG-NAME*))
             (member (format "%s" w-this-var) '("*MON-NAME*" "*BUG-NAME*"))
             (and  (intern-soft "*mon-misc-path-alist*" obarray) ;; *IS-MON-OBARRAY*
                   (bound-and-true-p *mon-misc-path-alist*)
                   (cdr (assoc 'the-only-if-its-a-mon-system *mon-misc-path-alist*))))
        ;; Its a MON KEY so just return
        wonky 
      (cond (force-bind (set w-this-var wonky))
              (bind-var (unless (bound-and-true-p w-this-var)
                          (setq w-this-var wonky)))
              (t wonky)))))
;;
;;; :TEST-ME (mon-build-user-name-example 5)
;;; :TEST-ME (mon-build-user-name-example 5 '*BUG-NAME*)
;;; :TEST-ME (mon-build-user-name-example 5 '*BUG-NAME* t)
;;; :TEST-ME (mon-build-user-name-example 5 '*BUG-NAME* nil t)
;;; :TEST-ME (mon-build-user-name-example 5 '*SOME-RANDOM-VAR* t)

;;; ==============================
;;; :CHANGESET 1745
;;; :CREATED <Timestamp: #{2010-03-24T13:38:11-04:00Z}#{10123} - by MON>
;;;###autoload
(defun mon-user-system-conditionals-TEST (&rest user-system-test-with)
  "Test function for `mon-user-name-conditionals' and `mon-system-type-conditionals'.\n
Return pp'ed results to buffer named \"*MON-USR/SYS-COND-TESTS*\".
When rest args USER-SYSTEM-TEST-WITH are supplied these are additional username
system-type tests to be performed.\n
Each USER-SYSTEM-TEST-WITH is quoted list of the form:\n
 (<LIST-KEY> <NAME-PROP> <SYSTEM-TYPE> <USER-NAME>)\n
For example:\n
 '\(:W32-THIS-USER :W-REAL-USER windows-nt \"i-am-w32-username\"\)
 '\(:GNU-THIS-USER :W-REAL-USER gnu/linux \"i-am-gnu-username\"\)\)\n
:EXAMPLE\n
\(mon-user-system-conditionals-TEST
 '\(:W32-THIS-USER :W-REAL-USER windows-nt \"i-am-w32-username\"\)
 '\(:GNU-THIS-USER :W-REAL-USER gnu/linux \"i-am-gnu-username\"\)
 '\(:DARWIN-THIS-USER :DARWIN-REAL-USER darwin \"i-am-darwin-username\"\)
 '\(:CYGWIN-THIS-USER :CYGWIN-REAL-USER cygwin \"i-am-cygwin-username\"\)  
 '\(:MSDOS-THIS-USER :MSDOS-REAL-USER  ms-dos \"i-am-msdos-username\"\)\)\n
:SEE-ALSO `mon-build-copyright-string-TEST',
`mon-help-propertize-regexp-symbol-defs-TEST', `mon-help-propertize-tags-TEST',
`mon-help-regexp-symbol-defs-TEST', `mon-help-CL-wget-pkgs-TEST',
`mon-wget-list-to-script-TEST', `mon-line-strings-to-list-TEST',
`mon-help-keys-wikify-TEST', `mon-insert-lisp-testme-fancy',
`mon-insert-lisp-testme', `mon-insert-test-cases', `*mon-testme-utils-xrefs*'.\n►►►"
  (let* (gthr
         (gthrer #'(lambda (lst-key nm-prop sys-typ usr-rln)
                    (let ((system-type sys-typ)
                          (user-real-login-name usr-rln))
                      (push `(,lst-key ,nm-prop ,usr-rln
                                       (:USER-NAME-COND
                                        ,(mon-user-name-conditionals)
                                        ,(mon-user-name-conditionals t))
                                       (:SYSTEM-TYPE-COND ,nm-prop ,usr-rln
                                                          ,(mon-system-type-conditionals)
                                                          ,(mon-system-type-conditionals t)))
                            gthr))))
        (test-with `((:W32-NO-USER :W-ANON-USER windows-nt "bubba")
                     (:W32-THIS-USER :W-REAL-USER windows-nt ,user-real-login-name)
                     (:GNU-NO-USER :W-ANON-USER gnu/linux  "bubba")
                     (:GNU-THIS-USER :W-REAL-USER gnu/linux ,user-real-login-name)
                     ,@user-system-test-with)))
    (dolist (tw test-with (setq gthr (nreverse gthr)))
      (funcall gthrer (nth 0 tw) (nth 1 tw) (nth 2 tw) (nth 3 tw)))
    (pp-display-expression gthr "*MON-USR/SYS-COND-TESTS*")))
;;
;;; :TEST-ME (mon-user-system-conditionals-TEST
;;;              '(:W32-THIS-USER :W-REAL-USER windows-nt "i-am-w32-username")
;;;              '(:GNU-THIS-USER :W-REAL-USER gnu/linux "i-am-gnu-username")
;;;              '(:DARWIN-THIS-USER :DARWIN-REAL-USER darwin "i-am-darwin-username")
;;;              '(:CYGWIN-THIS-USER :CYGWIN-REAL-USER cygwin "i-am-cygwin-username")  
;;;              '(:MSDOS-THIS-USER :MSDOS-REAL-USER  ms-dos "i-am-msdos-username"))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-25T15:35:10-04:00Z}#{10124} - by MON>
;;;###autoload
(defun mon-build-user-name-example-TEST ()
  "Test function for `mon-build-user-name-example'.\n
Return value displayed in buffer \"*MON-BUILD-USER-NAME-EXAMPLE-TEST*\".\n
:SEE-ALSO `mon-user-name-conditionals', `mon-system-type-conditionals',
`*mon-testme-utils-xrefs*'.\n►►►"
  (let* (mbunet-gthr
         (mk-eu #'(lambda (USER-BIND) 
                   (eval (defconst USER-BIND nil
                           "An example user for `mon-build-user-name-example'."))))
         (rmv-eu #'(lambda (USR)
                     (progn (makunbound USR) (unintern (symbol-name USR) obarray))))
         (shw-eu #'(lambda (test-key w-var ev-rslt)
                     (push `(,test-key :W-NAME-VAR ,(format "%s" w-var) ,ev-rslt) mbunet-gthr)))
         (l-o-a '((:W-BIND-VAR *SOME-BV-USER* nil t)
                  (:W-FORCE-BIND *SOME-FB-USER* nil t)
                  (:W-NO-BIND/FORCE nil nil nil)))
         )
    (dolist (ev l-o-a (setq mbunet-gthr (nreverse mbunet-gthr)))
      (let ((tk  (nth 0 ev))
            (wtv (or (nth 1 ev) '*EXAMPLE-USER*))
            (bv  (nth 2 ev))
            (fb  (nth 2 ev)))
        (progn
          (funcall mk-eu wtv)
          (funcall shw-eu tk wtv
                   (mon-build-user-name-example 5 wtv nil t))
          (funcall rmv-eu wtv))))
    (pp-display-expression mbunet-gthr "*MON-BUILD-USER-NAME-EXAMPLE-TEST*")))

;;; ==============================
;;; :CHANGESET 2108
;;; :CREATED <Timestamp: #{2010-09-03T15:45:12-04:00Z}#{10355} - by MON KEY>
;;;###autoload
(defun mon-cln-xml-escapes-TEST ()
  "Test function for `mon-cln-xml-escapes'.\n
Return and display results in buffer named \"*MON-CLN-XML-ESCAPES-TEST*\".\n
:EXAMPLE\n\n\(mon-cln-xml-escapes-TEST\)\n
:SEE-ALSO `*mon-testme-utils-xrefs*'.\n►►►"
  (let ((mcxet (mapconcat #'identity
                          '("e&amp;#769;" "a&amp;#769;" "i&amp;#769;" "o&amp;#769;"
                            "u&amp;#769;" "y&amp;#769;" "w&amp;#769;" "E&amp;#769;"
                            "A&amp;#769;" "I&amp;#769;" "O&amp;#769;" "U&amp;#769;"
                            "Y&amp;#769;" "W&amp;#769;")
                          "\n")))
    (with-current-buffer (get-buffer-create "*MON-CLN-XML-ESCAPES-TEST*")
      (erase-buffer)
      (save-excursion (insert mcxet)
                      (mon-cln-xml-escapes)
                      (mon-g2be -1) ;; :WAS (goto-char (buffer-end 0))
                      (insert ";; :FUNCTION `mon-cln-xml-escapes'\n"
                              ";; :CLEANED-FOLLOWING\n"
                              (make-string 68 59) "\n"
                              mcxet "\n"
                              (make-string 68 59) "\n" 
                              ";; :RESULTS-BELOW\n"))
      (display-buffer (current-buffer) t))))

;;; ==============================
;;; :CHANGESET 1956
;;; :CREATED <Timestamp: #{2010-07-08T18:25:41-04:00Z}#{10274} - by MON KEY>
;;;###autoload
(defun* mon-up/down-case-regexp-TEST (&key test-fncn)
  "Test function for case toggling functions: 
Keyword TEST-FNCN is one of:\n
 upcase-regexp           -> `mon-upcase-regexp'
 downcase-regexp         -> `mon-downcase-regexp'
 toggle-regexp-up        -> `mon-toggle-case-regexp'
 toggle-regexp-down      -> `mon-toggle-case-regexp'\n
:EXAMPLE\n
\(mon-up/down-case-regexp-TEST :test-fncn 'downcase-regexp\)\n
\(mon-up/down-case-regexp-TEST :test-fncn 'upcase-regexp\)\n
\(mon-up/down-case-regexp-TEST :test-fncn 'toggle-regexp-down\)\n
\(mon-up/down-case-regexp-TEST :test-fncn 'toggle-regexp-up\)\n
:SEE-ALSO `mon-toggle-case-query-user', `mon-toggle-case-regexp',
`mon-downcase-regexp', `mon-downcase-regexp-region', `mon-upcase-regexp',
`mon-upcase-regexp-region', `mon-downcase-commented-lines',
`mon-toggle-case-regexp-region', `mon-downcase-hex-values',
`mon-rectangle-downcase', `mon-rectangle-upcase',
`mon-upcase-commented-lines', `*mon-testme-utils-xrefs*'.\n►►►"
  (let* ((mudcrT-w-fncn
          (case test-fncn
            ;; (upcase-regexp-region    '(up mon-upcase-regexp-region
            ;;                               ((buffer-end 0) (buffer-end 1)
            ;;                                "^\\(#[a-z0-9]\\{6,6\\}$\\)")))
            ;; (downcase-regexp-region   '(down mon-downcase-regexp-region
            ;;                                  ((buffer-end 0) (buffer-end 1) 
            ;;                                   "^\\(#[A-Z0-9]\\{6,6\\}$\\)")))
            (upcase-regexp      '(up mon-upcase-regexp
                                     ("^\\(#[a-z0-9]\\{6,6\\}$\\)" nil t)))
            (downcase-regexp    '(down mon-downcase-regexp
                                  ("^\\(#[A-Z0-9]\\{6,6\\}$\\)" nil t)))
            (toggle-regexp-up   '(up  mon-toggle-case-regexp
                                      ("^\\(#[a-z0-9]\\{6,6\\}$\\)" up nil t)))
            (toggle-regexp-down '(down mon-toggle-case-regexp
                                       ("^\\(#[A-Z0-9]\\{6,6\\}$\\)" down nil t t)))
            (t '(up mon-upcase-regexp  '("^\\(#[a-z0-9]\\{6,6\\}$\\)" nil t)))))
         ;; {upcase|downcase|toggle} {region|nil}
         (mudcrT-bfr-nm (upcase (format "*%s-eg*" (cadr mudcrT-w-fncn)))))
    (with-current-buffer 
        (get-buffer-create mudcrT-bfr-nm)
      (erase-buffer)
      ;; :NOTE uncomment when debugging.
      (display-buffer (current-buffer) t)
      (let ((brlstr (vconcat (if (eq (car mudcrT-w-fncn) 'down)    
                                 (number-sequence 65 70) ;uppers
                               (number-sequence 97 102)) ;lowers
                             (number-sequence 48 57)))   ;digits
            brlvar
            brlgthr)
        (dotimes (mure 10 
                       (progn 
                         (save-excursion 
                           (insert "\n;;\n;; :AFTER-REGEXP\n;;\n\n"
                                   (mapconcat #'identity brlgthr "\n")))
                         (setq brlvar 
                               (format ";; :W-FUNCTION `%s'\n;;\n;; :W-REGEXP %S\n;;\n;; :BEFORE-REGEXP\n;;\n%s\n"
                                       (cadr mudcrT-w-fncn) 
                                       (caaddr mudcrT-w-fncn)
                                       (mapconcat #'(lambda (mudcrT-bgn) (concat ";; " mudcrT-bgn)) 
                                                  brlgthr "\n")))))
          (setq brlvar nil)
          (dotimes (brl 6 (push (concat "#" (mapconcat 'char-to-string brlvar "")) brlgthr))
            (push (aref brlstr (random 16)) brlvar)))
        (if (or (eq test-fncn 'toggle-regexp-down)
                (eq test-fncn 'toggle-regexp-up))
                (setq mudcrT-bfr-nm (apply (cadr mudcrT-w-fncn) (caddr mudcrT-w-fncn)))
          (progn (setq mudcrT-bfr-nm)
                 (apply (cadr mudcrT-w-fncn) (caddr mudcrT-w-fncn))))
        (goto-char (buffer-end 0))
        (when mudcrT-bfr-nm (insert 
                      (mapconcat #'(lambda (mudcrT-semi)(format ";; %S" mudcrT-semi))
                                 (save-match-data (split-string mudcrT-bfr-nm "\n"))
                                 "\n")
                      ";;\n;;\n"))
        (insert brlvar))
      ) ;(display-buffer (current-buffer) t))
    mudcrT-bfr-nm))


 
;;; ==============================
;;; :CHANGESET 2316
;;; :CREATED <Timestamp: #{2010-11-12T23:36:08-05:00Z}#{10455} - by MON KEY>
;;;###autoload
(defun mon-hash-get-symbol-keys-TEST ()
  "Test function for macro `mon-hash-get-symbol-keys'
:EXAMPLE\n\n\(mon-hash-get-symbol-keys-TEST\)\n
:SEE-ALSO `*mon-testme-utils-xrefs*'.\n►►►"
  (let ((wonky-hash (make-hash-table))
        (wonky-tst (mon-string-wonkify 
                    (concat (mon-nshuffle-vector 
                              (vconcat (number-sequence 97 122)))) 16))
        gthr)
    (mapc #'(lambda (ph) (puthash ph (sxhash ph) wonky-hash)) wonky-tst)
    (setq gthr (nreverse `(,(mon-hash-get-symbol-keys wonky-hash t)
                           ,(mapcar #'(lambda (not-in-ob)
                                        (intern-soft not-in-ob)) wonky-tst))))
    (and (cadr (mon-sequence-all-booleanp nil #'car (car gthr)))
         (nconc '(#:all-tests-passed t) gthr ))))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-11T20:15:00-04:00Z}#{10147} - by MON>
;;;###autoload
(defun mon-file-stamp-buffer-filename-TEST ()
  "Test function for `mon-file-stamp-buffer-filename'.\n
:SEE-ALSO `mon-file-stamp', `mon-file-stamp-minibuffer',
`mon-help-time-functions', `mon-help-mon-time-functions', `mon-help-iso-8601',
`mon-help-CL-time', `mon-help-CL-local-time', `*mon-testme-utils-xrefs*'.\n►►►"
  (dolist (i `("" ".el" ".gz" 
               ,(format-time-string "-%Y-%m-%d.el")
               ,(format-time-string "-%Y-%m-%d.el.gz"))
             (dired-other-window temporary-file-directory))
    (let* ((mfsbft (concat "MON-FILE-STAMP-BUFFER-FILENAME-TEST" i))
           (mfsbft-tmp temporary-file-directory))
      (with-current-buffer (get-buffer-create mfsbft)
        (cd mfsbft-tmp)
        (when (equal (file-name-extension i t) ".gz")
          (write-file (concat mfsbft-tmp (buffer-name))))
        (mon-file-stamp-buffer-filename)
        (insert mfsbft)
        (if (equal i "")
            (write-file mfsbft-tmp)
          (save-buffer))
        (kill-buffer (get-buffer mfsbft))))))

;;; ==============================
;;; :PREFIX "mcflt-"
;;; :CHANGESET 2387
;;; :CREATED <Timestamp: #{2011-01-11T13:25:22-05:00Z}#{11022} - by MON KEY>
;;;###autoload
(defun mon-cln-freenode-log-TEST ()
  "Test/example function for `mon-cln-freenode-log'.\n
:EXAMPLE\n\n\(mon-cln-freenode-log-TEST\)\n
:SEE-ALSO `mon-wget-freenode-lisp-logs', `*regexp-clean-IRC-logs*',
`*freenode-lisp-logs*', `mon-help-CL-minion', `*mon-testme-utils-xrefs*'.\n►►►"
  (let ((mcflt-eg
         '("00:03:22 --- join: <USER1> (~<USER1>@some.ip.address.abc) joined #lisp"
           "00:04:52 --- join: <USER2> (~<USER2>@some.ip.address.abc) joined #lisp"
           "00:06:14 --- join: <USER3> (~<USER3>@some.ip.address.abc) joined #lisp"
           "00:08:30 --- quit: <USER1> (Remote host closed the connection)"
           "00:05:28 --- quit: <USER2> (Changing host)"
           "04:53:12 --- quit: <USER3> (Read error: Connection reset by peer)"
           "08:52:58 --- quit: <USER1> (Read error: Operation timed out)"
           "04:55:08 --- quit: <USER2> (Client Quit)"
           "04:35:47 --- quit: <USER3> (Ping timeout: <NNN> seconds)"
           "04:34:31 --- quit: <USER1> (Max SendQ exceeded)"
           "04:13:06 --- quit: <USER2> (Quit: <REASON>)"
           "04:30:03 --- quit: <USER3> (Excess Flood)"
           "00:06:37 --- part: <USER1> left #lisp"))
        (mcflt-bfr (get-buffer-create "*MON-CLN-FREENODE-LOG-TEST*")))
    (with-current-buffer mcflt-bfr
      (erase-buffer)
      (save-excursion 
        (apply #'insert "\n" 
               (setq mcflt-eg (mapcar #'(lambda (mcflt-L-0) 
                                          (concat mcflt-L-0 "\n")) mcflt-eg))))      
      (display-buffer mcflt-bfr t)
      (let ((inhibit-quit t)
            (minibuffer-message-timeout 2.5)
            (mcflt-div (concat (make-string 68 59)"\n")))
        (minibuffer-message "Getting read to clean logs...")
        (mon-cln-freenode-log)
        (mon-g2be -1)
        (save-excursion
          (apply #'insert mcflt-div
                 `(";;\n;; Following lines were cleaned:\n;;\n"
                   ,@(mapcar #'(lambda (mcflt-L-1)
                             (concat ";; " mcflt-L-1))
                         mcflt-eg)
                   ";;\n" 
                   ,mcflt-div
                   "\n { ... What was here is no longer ... } \n")))
        (minibuffer-message "After cleaning logs")))))

;;; ==============================
(provide 'mon-testme-utils)
;;; ==============================

 
;; Local Variables:
;; generated-autoload-file: "./mon-loaddefs.el"
;; coding: utf-8
;; End:

;;; ====================================================================
;;; mon-testme-utils.el ends here
;;; EOF

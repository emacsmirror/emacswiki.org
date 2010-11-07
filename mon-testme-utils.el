;;; mon-testme-utils.el --- tests and template for inserting them
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2009-2010 MON KEY. All rights reserved.
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
;; `mon-set-buffer-substring-no-properties-TEST', 
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
;; `mon-testme-utils'
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
;; TODO:
;;
;;
;; Should these `-TEST' fncns be moved into here?
;; `google-define-get-command-TEST'
;; `mon-build-copyright-string-TEST' *
;; `mon-build-user-name-example-TEST'
;; `mon-drive-transfer-template-TEST'
;; `mon-file-stamp-buffer-filename-TEST'
;; `mon-help-keys-wikify-TEST'
;; `mon-help-propertize-regexp-symbol-defs-TEST'
;; `mon-help-propertize-tags-TEST'
;; `mon-help-regexp-symbol-defs-TEST'
;; `mon-help-CL-wget-pkgs-TEST' *
;; `mon-regexp-clean-ulan-dispatch-chars-TEST'
;; `mon-permute-combine-functions-TEST'
;; `mon-user-system-conditionals-TEST'
;; `mon-up/down-case-regexp-TEST'
;; `mon-wget-list-to-script-TEST'
;; `mon-with-inhibit-buffer-read-only-TEST'
;; `mon-with-buffer-undo-disabled-TEST'
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
;; Copyright © 2009-2010 MON KEY 
;;; ==============================

;;; CODE:

(eval-when-compile (require 'cl))

;; `cl::position'     <- `mon-insert-lisp-testme-fancy'
;; `cl::intersection' <- `mon-insert-lisp-testme-fancy'
(eval-when-compile (require 'mon-cl-compat))


;;; ==============================
;;; :CHANGESET 2189
;;; :CREATED <Timestamp: #{2010-10-07T16:02:18-04:00Z}#{10404} - by MON KEY>
(defgroup mon-testme-utils nil
  "Customization group for mon test functions.\n
:SEE-ALSO .\n►►►"
  :link '(url-link :tag ":EMACSWIKI-FILE" 
                   "http://www.emacswiki.org/emacs/mon-testme-utils.el")
  :link '(emacs-library-link "mon-test-me-utils.el")
  :group 'mon-base)

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
`mon-comment-divider-to-col-four', `mon-insert-lisp-evald'.\n►►►"
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
  "Easily identified tracing vars for debugging `mon-*' and `naf-mode-*' functions.
Unbinds all previously bound variables:
 test-aaa, test-bbb, test-ccc, test-ddd,
 test-aa, test-AA, test-ag, test-AG, test-a4, test-A4\n
Rebinds vars 'default values'.  Called-interactively or INSERTP non-nil insert
test-cases at point.\n
:EXAMPLE\n\(mon-insert-test-cases\)\n:SEE-ALSO `mon-insert-lisp-testme'.\n►►►"
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
:SEE-ALSO `mon-gensym', `with-gensyms', `mon-gensym-counter-randomizer'.\n►►►"
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
          (sort-lines nil (buffer-end 0) (buffer-end 1)))
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
:SEE-ALSO `mon-nshuffle-vector', `mon-list-shuffle-safe'.\n►►►"
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
    (mon-g2be -1) ;; (goto-char (buffer-end 0))
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
`mon-help-propertize-tags-TEST', `mon-insert-test-cases'.\n►►►"
  (let ((st01 (make-marker))
        (en01 (make-marker))
        (t-str (concat "hendr erit\norci\nultrices\naugue\nAliquam\n"
                       "odio\nNam\ne ros\nurna\naliquam\nvitae\nlacinia")))
    (cond ((not insrtp)
           (with-temp-buffer
             (insert t-str)
             (mon-line-strings-to-list (mon-g2be -1 t) (mon-g2be 1 t) with-cdr with-wrap)))
          (insrtp 
           (set-marker st01 (point))           
           (insert t-str)
           (set-marker en01 (point))
           (mon-g2be st01)
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
`mon-user-system-conditionals-TEST', `mon-wget-list-to-script-TEST'.\n►►►"
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
`mon-user-system-conditionals-TEST', `mon-wget-list-to-script-TEST'.\n►►►"
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
:SEE-ALSO .\n►►►"
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
:SEE-ALSO `mon-line-dolines-setup-TEST'.\n►►►"
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
:SEE-ALSO `buffer-undo-list'.\n►►►"
  (let ((rnd-char (mon-nshuffle-vector (vconcat (append (number-sequence 97 122)
                                                   (number-sequence 65 90)))))
        bul)
    (with-temp-buffer 
      (mon-with-buffer-undo-disabled (setq bul buffer-undo-list))
      (unless force-fail
        (dotimes (jnk 100) 
          (insert (make-string (random 79) (elt rnd-char (random 52))) "\n"))
        (delete-region (random (1- (buffer-end 1))) (random (1- (buffer-end 1)))))
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
:SEE-ALSO .\n►►►"
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
:SEE-ALSO .\n►►►"
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
:SEE-ALSO `mon-help-CL-pkgs', `*mon-help-CL-cmu-ai-repo*', `*mon-help-CL-ext-pkg-map*'.\n►►►"
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
`mon-line-strings-to-list-TEST'.\n►►►"
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
:SEE-ALSO `google-define', `google-define-kill-def-buffers'.\n►►►"
  (let ((gdgc-buff 
         (get-buffer 
          (google-define-get-command 
           "www.google.com" 
           "/search?num=100&hl=en&q=define%3A%22snarf%22&btnG=Search")))
        gdgc)
    (setq gdgc (with-current-buffer gdgc-buff
                 (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
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
Return test-case results in buffer named \"*MON-PERMUTE-COMBINE-TEST*\".\n
Test equivalence of return values of following procedures:\n
 `mon-permute-combine', `mon-permute-combine-1', `mon-permute-combine-2'\n
:EXAMPLE\n\n\(mon-permute-combine-functions-TEST\)\n
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
         (tpc (get-buffer-create "*MON-PERMUTE-COMBINE-TEST*"))
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
;;; :TEST-ME (mon-permute-combine-functions-TEST)


;;; ==============================
;;; :CHANGESET 2201
;;; :CREATED <Timestamp: #{2010-10-19T21:40:13-04:00Z}#{10422} - by MON KEY>
;;;###autoload
(defun mon-write-string-reset-bind-TEST ()
  "Test function for `mon-write-string' w/ keyword arg :RESET-BIND.\n
Return and display results in buffer named \"*MON-WRITE-STRING-TEST*\".\n
:EXAMPLE\n\n\(mon-write-string-reset-bind-TEST\)\n
:SEE-ALSO .\n►►►"
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
:SEE-ALSO .\n"
  (with-temp-buffer 
    ;; :NOTE don't know why but this unintern is necessary to prevent `rgn-prps' from
    ;; persisting its variable outside the let binding.
    (unintern "rgn-prps" obarray) 
    (let ((rgn-tst "lowercase string aNd UPERCASE STRING")
          rgn-prps)
      (setq rgn-prps)
      (save-excursion (insert rgn-tst))
      (setplist 'rgn-prps (mon-region-capitalize (line-beginning-position 1) (line-end-position 1)))
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
:SEE-ALSO .\n►►►"
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
:SEE-ALSO .\n►►►"
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
:SEE-ALSO .\n►►►"
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
:SEE-ALSO .\n►►►"
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
:SEE-ALSO .\n►►►"
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
:SEE-ALSO .\n►►►"
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
:SEE-ALSO .\n►►►"
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
:SEE-ALSO .\n►►►"  
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
:SEE-ALSO `%mon-list-reorder'.\n►►►"
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
                        (setq with-result)))
     (setq with-result
           (apply #'mon-list-reorder (car mlrdr-D-1)))
     (push `(:test-passed ,(equal with-result (cadr mlrdr-D-1))
             :with-test   ,`(mon-list-reorder ,@(car mlrdr-D-1))
             :with-result ,with-result
             :with-expect ,(cadr mlrdr-D-1))
           mlrdr-gthr)
    (setq with-result))
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
:SEE-ALSO `mon-cln-ulan'.\n►►►"
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
                  `(:w-replace ,(buffer-substring-no-properties (buffer-end 0) (buffer-end 1))
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
:SEE-ALSO .\n►►►"
  (with-current-buffer (get-buffer-create "*DEFSETF-BUF-NO-PROPS*")
    (erase-buffer)
    (emacs-lisp-mode)
    (save-excursion (insert "was: lots of lots and lots of bubba"))
    (display-buffer (current-buffer) t)
    (sit-for 2)
    (setf (buffer-substring (buffer-end 0) (buffer-end 1)) 
          (propertize "now: lots of lots and lots of bubba" 'font-lock-face 'bold))
    (sit-for 2)
    
    (prog1 
        (when
            (equal
             (setf (buffer-substring-no-properties (buffer-end 0) (buffer-end 1)) 
                   ;; won't get inserted 
                   (propertize "Are there no bold props here?" 'font-lock-face 'bold))
             "Are there no bold props here?")
          "Good, no bold props were seen")
      (unwind-protect 
          (sit-for 2)
        (kill-buffer (current-buffer))))))

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

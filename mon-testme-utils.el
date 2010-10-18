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
;; `mon-line-strings-bq-qt-sym-bol-TEST'
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
;;
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
             (mon-line-strings-to-list (point-min) (point-max) with-cdr with-wrap)))
          (insrtp 
           (set-marker st01 (point))
           (insert t-str)
           (set-marker en01 (point))
           (goto-char st01)
           (mon-line-strings-to-list st01 en01 with-cdr with-wrap t)))))
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
        (with-current-buffer
            (get-buffer-create "*MON-LINE-DOLINES-TEST*")
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
(defun mon-string-split-TEST (&optional w-msg-usr)
  "Test function for `mon-string-split'\n
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
;;; :TEST-ME `mon-line-strings-bq-qt-sym-bol-TEST'


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
(provide 'mon-testme-utils)
;;; ==============================

 
;; Local Variables:
;; generated-autoload-file: "./mon-loaddefs.el"
;; coding: utf-8
;; End:

;;; ====================================================================
;;; mon-testme-utils.el ends here
;;; EOF

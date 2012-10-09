;;; ert-expectations.el --- The simplest unit test framework in the world

;; Filename: ert-expectations.el
;; Description: The simplest unit test framework in the world
;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Maintainer: rubikitch <rubikitch@ruby-lang.org>
;; Copyright (C) 2012, rubikitch, all rights reserved.
;; Time-stamp: <2012-10-09 16:34:22 rubikitch>
;; Created: 2012-09-24 16:20:43
;; Version: 0.2
;; URL: http://www.emacswiki.org/emacs/download/ert-expectations.el
;; Keywords: test unittest ert expectations
;; Compatibility: GNU Emacs 24.2.2
;;
;; Features that might be required by this library:
;;
;; `el-mock', `ert'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; `expectations', the simplest unit test framework using `ert'.
;; No test names! No extra typing!
;; This is aimed for a successor of el-expectations.
;; If you use el-expectations, you can simply replace.
;;
;; I love Jay Fields' expectations unit testing framework in Ruby. It
;; provides one syntax and can define various assertions. So I created
;; Emacs Lisp Expectations modeled after expectations in Ruby.
;; Testing policy is same as the original expectations in Ruby. Visit
;; expectations site in rubyforge.
;; http://expectations.rubyforge.org/
;;
;; With Emacs Lisp Mock (el-mock.el), Emacs Lisp Expectations supports
;; mock and stub, ie. behavior based testing.
;; You can get it from EmacsWiki
;; http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el
;;
;; The biggest advantage is this uses `ert' feature to display test result.
;; You can easily understand why a test failed.
;;
;;  `expectations'  vs    `ert'
;;  (expect 10            (ert-deftest erte-test-00001 ()
;;    (+ 4 6))              (should (equal 10
;;                                         (+ 4 6))))
;;
;; Example:
;;
;;   (expectations
;;     (desc "success")
;;     (expect 10
;;       (+ 4 6))
;;     (expect 5
;;       (length "abcde"))
;;     (desc "fail")
;;     (expect 11
;;       (+ 4 6))
;;     (expect 6
;;       (length "abcde")))
;;
;; Press C-M-x sexp then get the result in *ert*:
;;
;;   Selector: t
;;   Passed: 2
;;   Failed: 2 (2 unexpected)
;;   Total:  4/4
;;
;;   Started at:   2012-10-09 15:37:17+0900
;;   Finished.
;;   Finished at:  2012-10-09 15:37:17+0900
;;
;;   ..FF
;;
;;   F erte-test-00003
;;       (ert-test-failed
;;        ((should
;;          (equal 11
;;                 (mock-protect
;;                  (lambda nil
;;                    (+ 4 6)))))
;;         :form
;;         (equal 11 10)
;;         :value nil :explanation
;;         (different-atoms
;;          (11 "#xb" "?^K")
;;          (10 "#xa" "?\n"))))
;;
;;   F erte-test-00004
;;       (ert-test-failed
;;        ((should
;;          (equal 6
;;                 (mock-protect
;;                  (lambda nil
;;                    (length "abcde")))))
;;         :form
;;         (equal 6 5)
;;         :value nil :explanation
;;         (different-atoms
;;          (6 "#x6" "?^F")
;;          (5 "#x5" "?^E"))))
;;
;;
;; If you want more complex example, see (describe-function 'expectations)
;;
;;; Installation:
;;
;; Put ert-expectations.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'ert-expectations)
;;
;; No need more.
;;
;;; Usage:
;;
;; 1. Evaluate an expectations sexp.
;; 2. `M-x expectations-execute' or `M-x ert RET RET' to execute a test.
;; 3. If there are any errors, follow link to test names in *ert*
;;    to go to expect sexp in error.
;;
;; If you evaluated expectations by C-M-x, it is automatically executed.
;; If you type C-u C-M-x, execute expectations with batch-mode.
;;
;; For further information: see docstring of `expectations'.
;; [EVAL IT] (describe-function 'expectations)
;;
;;; Batch Mode:
;;
;; You can execute tests in batch mode using `ert-run-tests-batch-and-exit'.
;;
;; Example:
;; $ emacs -Q -batch -L . -l ert-expectations -l el-expectations-success-sample.el -f ert-run-tests-batch-and-exit
;;
;;; Examples:
;;
;; Example code is in the EmacsWiki.
;;
;; Success example http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations-success-sample.el
;; Failure example http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations-failure-sample.el
;;
;;; Self test:
;;
;; M-x erte-test-command: Execute successful and failed tests.
;; M-x erte-test--success: Execute successful tests.
;; M-x erte-test--fail: Execute failed tests.
;; [EVAL IT] (shell-command "emacs -Q -batch -L . -l ert-expectations -l el-expectations-success-sample.el -f ert-run-tests-batch-and-exit &")
;; [EVAL IT] (shell-command "emacs -Q -batch -L . -l ert-expectations -l el-expectations-success-sample.el -l el-expectations-failure-sample.el -f ert-run-tests-batch-and-exit &")
;;
;;; Embedded test:
;;
;; You can embed test using `fboundp' and `dont-compile'. dont-compile
;; is needed to prevent unit tests from being byte-compiled.
;;
;; (dont-compile
;;   (when (fboundp 'expectations)
;;     (expectations
;;       (expect ...)
;;       ...
;; )))
;;
;;; Customize:
;;
;; Below are customizable option list:
;;
;;  `expectations-execute-at-once'
;;    If non-nil, execute selected expectation when pressing C-M-x.
;;    default = (quote all)
;;
;;; Limitation:
;;
;; * `expectations-execute' can execute one test (sexp).
;;
;;; Bug Report:
;;
;; If you have problem, send a bug report via M-x erte-send-bug-report.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.jp")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of ert-expectations.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "ert-expectations.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x erte-send-bug-report and M-x insert-buffer *Backtrace*
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Japanese, please write in Japanese:-)
;;
;;; Change log:
;;
;; 2012/10/09
;;      * First release.
;;
;; 2012/09/24
;;      * Created.
;;

;;; Require

(require 'cl)
(require 'el-mock nil t)
(require 'ert)

;;; Code:

;;;; Macros
(defmacro erte-aif (test-form then-form &rest else-forms)
  "Like `if' but set the result of TEST-FORM in a temprary variable called `it'.
THEN-FORM and ELSE-FORMS are then excuted just like in `if'."
  (declare (indent 2) (debug t))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

;;;; The `expectations' macro
(defvar erte-test-count 0)
(defvar erte-last-filename nil)
(defvar erte-not-delete-flag nil)
(defmacro expectations (&rest body)
  "Define a expectations test case.
Use `expect' and `desc' to verify the code.

`exps-tmpbuf' creates temporary buffers and they are killed
after execute expectations.

Synopsis:
* (expect EXPECTED-VALUE BODY ...)
  Assert that the evaluation result of BODY is `equal' to EXPECTED-VALUE.
* (desc DESCRIPTION)
  Description of a test. It is treated only as a delimiter comment.

Synopsis of EXPECTED-VALUE:
* (non-nil)
* (true)
  Any non-nil value, eg. t, 1, '(1).

* (buffer BUFFER-NAME)
  Body should eq buffer object of BUFFER-NAME.

  Example:
    (expect (buffer \"*scratch*\")
      (with-current-buffer \"*scratch*\"
        (current-buffer)))
* (regexp REGEXP)
  Body should match REGEXP.

  Example:
    (expect (regexp \"o\")
      \"hoge\")
* (type TYPE-SYMBOL)
  Body should be a TYPE-SYMBOL.
  TYPE-SYMBOL may be one of symbols returned by `type-of' function.
   `symbol', `integer', `float', `string', `cons', `vector',
   `char-table', `bool-vector', `subr', `compiled-function',
   `marker', `overlay', `window', `buffer', `frame', `process',
   `window-configuration'
  Otherwise using predicate naming TYPE-SYMBOL and \"p\".
  For example, `(type sequence)' uses `sequencep' predicate.
  `(type char-or-string)' uses `char-or-string-p' predicate.

  Example:
    (expect (type buffer)
      (current-buffer))
    (expect (type sequence)
      nil)
    (expect (type char-or-string)
      \"a\")

* (error)
  Body should raise any error.

  Example:
    (expect (error)
      (/ 1 0))
* (error ERROR-SYMBOL)
  Body should raise ERROR-SYMBOL error.

  Example:
    (expect (error arith-error)
      (/ 1 0))
* (error ERROR-SYMBOL ERROR-DATA)
  Body should raise ERROR-SYMBOL error with ERROR-DATA.
  ERROR-DATA is 2nd argument of `signal' function. If ERROR-DATA
  is the special symbol `*', then it will match any error data.

  Example:
    (expect (error wrong-number-of-arguments '(= 3))
      (= 1 2 3 ))

    (expect (error error *)
      (error \"message\"))

* (error-message ERROR-MESSAGE)
  Body should raise any error with ERROR-MESSAGE.

  Example:
    (expect (error-message \"ERROR!!\")
      (error \"ERROR!!\"))

* (no-error)
  Body should not raise any error.

  Example:
    (expect (no-error)
      1)

* (mock MOCK-FUNCTION-SPEC => MOCK-RETURN-VALUE)
  Body should call MOCK-FUNCTION-SPEC and returns MOCK-RETURN-VALUE.
  Mock assertion depends on `el-mock' library.
  If available, you do not have to require it: ert-expectations detects it.

  Synopsis of MOCK-FUNCTION-SPEC:
    (FUNCTION ARGUMENT ...)
    MOCK-FUNCTION-SPEC is almost same as normal function call.
    If you should specify `*' as ARGUMENT, any value is accepted.
    Otherwise, body should call FUNCTION with specified ARGUMENTs.

  Example:
    (expect (mock (foo * 3) => nil)
      (foo 9 3))

* (not-called FUNCTION)
  Body should not call FUNCTION.
  Not-called assertion depends on `el-mock' library.
  If available, you do not have to require it: ert-expectations detects it.

  Example:
    (expect (not-called hoge)
      1)

* any other SEXP
  Body should equal (eval SEXP).

  Example:
    (expect '(1 2)
      (list 1 2))

Extending EXPECTED-VALUE is easy. See ert-expectations.el source code.

Example:
 (expectations
   (desc \"simple expectation\")
   (expect 3
     (+ 1 2))
   (expect \"hoge\"
     (concat \"ho\" \"ge\"))
   (expect \"fuga\"
     (set-buffer (get-buffer-create \"tmp\"))
     (erase-buffer)
     (insert \"fuga\")
     (buffer-string))

   (desc \"extended expectation\")
   (expect (buffer \"*scratch*\")
     (with-current-buffer \"*scratch*\"
       (current-buffer)))
   (expect (regexp \"o\")
     \"hoge\")
   (expect (type integer)
     3)

   (desc \"error expectation\")
   (expect (error arith-error)
     (/ 1 0))
   (expect (error)
     (/ 1 0))
   (desc \"mock with stub\")
   (expect (mock (foo 5 * 7) => nil)
     ;; Stub function `hoge', which accepts any arguments and returns 3.
     (stub hoge => 3)
     (foo (+ 2 (hoge 10)) 6 7))
   (desc \"tmpbuf\")
   (expect \"foo\"
     (with-current-buffer (exps-tmpbuf)
       (insert \"foo\")
       (buffer-string)))
   )"
  `(progn (setq erte-last-filename (or load-file-name buffer-file-name))
          (unless (or noninteractive erte-not-delete-flag) (ert-delete-all-tests))
          ,@body))
(put 'expectations 'lisp-indent-function 0)

;;;; The `expect' macro, expectations -> ert translator
(defmacro expect (expected &rest actual)
  "Assertion in `expectations'. See `expectations' docstring."
  (funcall (case (car-safe expected)
             (error         'erte-expect-error-sexp)
             (error-message 'erte-expect-error-message-sexp)
             (not-called    'erte-expect-not-called-sexp)
             (mock          'erte-expect-mock-sexp)
             (t             'erte-expect-normal-sexp))
           (intern (format "erte-test-%05d" (incf erte-test-count)))
           expected actual))
(put 'expect 'lisp-indent-function 1)

(defmacro erte-test (&rest body)
  `(progn
     (expectations ,@body)
     (expectations-execute)))
(put 'erte-test 'lisp-indent-function 0)

(defun erte-progn-sexp (actual)
  (if (fboundp 'mock-protect)
      `(mock-protect (lambda () ,@actual))
    `(progn ,@actual)))

(defun erte-expect-error-sexp (funcsym expected actual)
  (destructuring-bind (error &optional errsym data) expected
    (funcall (cond ((or (null errsym) (eq data '*))
                         'erte-expect-error-sexp--any)
                   (data 'erte-expect-error-sexp--data)
                   (t    'erte-expect-error-sexp--symbol))
             funcsym expected actual)))
(defun erte-expect-error-sexp--any (funcsym expected actual)
  `(ert-deftest ,funcsym ()
     (should-error ,(erte-progn-sexp actual))))
(defun erte-expect-error-sexp--data (funcsym expected actual)
  (declare (special errsym data))
  `(ert-deftest ,funcsym ()
     (should (equal '(,errsym ,@(eval data))
                    (should-error ,(erte-progn-sexp actual))))))
(defun erte-expect-error-sexp--symbol (funcsym expected actual)
  `(ert-deftest ,funcsym ()
     (should-error ,(erte-progn-sexp actual) :type ',(cadr expected))))

(defun erte-expect-error-message-sexp (funcsym expected actual)
  `(ert-deftest ,funcsym ()
     (should (equal ,(cadr expected)
                    (error-message-string (should-error ,(erte-progn-sexp actual)))))))

(defun erte-execute-with-mock-sexp (expected actual)
  `(condition-case me
      (progn
        (mock-protect
         (lambda ()
           ,expected
           ,@actual))
        nil)
    (mock-error me)))
(defun erte-expect-mock-sexp (funcsym expected actual)
  `(ert-deftest ,funcsym ()
     (let ((err ,(erte-execute-with-mock-sexp expected actual)))
       (cond ((and err (eq 'not-called (cadr err)))
              (should (equal ',expected 'not-called)))
             (err
              (destructuring-bind (_  e-args  a-args) err
                (should (equal ',expected (list 'mock a-args)))))
             (t
              (should t))))))
(defun erte-expect-not-called-sexp (funcsym expected actual)
  `(ert-deftest ,funcsym ()
     (let ((err ,(erte-execute-with-mock-sexp expected actual)))
       (cond ((eq 'called (cadr err))
              (should (equal ',expected 'called)))
             (t
              (should t))))))
(defun erte-expect-normal-sexp (funcsym expected actual)
  `(ert-deftest ,funcsym ()
     (should ,(erte-should-arg expected actual))))

(defvar erte-should-arg-alist
  '((buffer . `(equal ,expected-cadr (buffer-name ,actual-sexp)))
    (regexp . `(string-match ,expected-cadr ,actual-sexp))
    (type . `(erte-match-type-p ',expected-cadr ,actual-sexp))
    (no-error . `(or ,actual-sexp t))
    (true . actual-sexp)
    (non-nil . actual-sexp)))

(defun erte-should-arg (expected actual)
  (let* ((actual-sexp (erte-progn-sexp actual))
         (default `(equal ,expected ,actual-sexp)))
    (if (listp expected)
        (let ((expected-cadr (cadr expected)))
          (erte-aif (assoc-default (car expected) erte-should-arg-alist)
              (eval it)
            default))
      default)))

(defun erte-match-type-p (type obj)
  (or (eq (type-of obj) type)
      (let* ((name (symbol-name type))
             (pred (intern (concat name (if (string-match "-" name) "-p" "p")))))
        (when (fboundp pred)
          (funcall pred obj)))))

;;;; The `desc' function, label and pointer.
(defvar erte-desc-filename-alist nil)
(defun desc (description &rest ignore)
  "Put a label in `expectations'."
  (push (list erte-test-count
              (or load-file-name buffer-file-name)
              description)
        erte-desc-filename-alist)
  nil)

;;;; Jump to appropriate `expect' statement from *ert*
(defadvice ert-find-test-other-window (around ert-expectations (test-name) activate)
  "Call `erte-find-test-other-window' for erte-test-*."
  (if (string-match "^erte-test-\\([0-9]+\\)$" (symbol-name test-name))
      (erte-find-test-other-window
       (string-to-number (match-string 1 (symbol-name test-name))))
    ad-do-it))

(defun erte-assoc-default-cell (key alist &optional test default)
  "Like `assoc-default' but returns the cons cell found."
  (let (found (tail alist) value)
    (while (and tail (not found))
      (let ((elt (car tail)))
	(when (funcall (or test 'equal) (if (consp elt) (car elt) elt) key)
	  (setq found t value (if (consp elt) elt default))))
      (setq tail (cdr tail)))
    value))

(defun erte-find-test-other-window (n)
  (condition-case e
      (destructuring-bind (descno filename desc)
          (erte-assoc-default-cell n erte-desc-filename-alist '<)
        (find-file-other-window filename)
        (goto-char (point-min))
        (search-forward (format "(desc \"%s\")" desc) nil t)
        (re-search-forward "(expect\\b" nil t (- n descno)))
    (error (error "Don't know where `erte-test-%05d' is defined" n))))

(defadvice ert--pp-with-indentation-and-newline (around print-level activate)
  "Display full sexp in *ert* buffer."
  (let ((print-level nil)) ad-do-it))

;;;; el-expectations compatibility
(defcustom expectations-execute-at-once 'all
  "If non-nil, execute selected expectation when pressing C-M-x.
If 'all, execute all expectations blocks in current file.
If other non-nil value, execute current expectations block."
  :group 'ert-expectations)

(defun expectations-eval-defun (arg)
  "Do `eval-defun'.
If `expectations-execute-at-once' is non-nil, execute expectations if it is an expectations form."
  (interactive "P")
  (cond ((and (exps-current-form-is-expectations) (eq expectations-execute-at-once 'all))
         (ert-delete-all-tests)
         (save-excursion
           (goto-char (point-min))
           (let ((erte-not-delete-flag t))
             (while (re-search-forward "^\\s-*(expectations\n" nil t)
               (eval-defun arg))))
         (expectations-execute))
        ((exps-current-form-is-expectations)
         (eval-defun arg)
         (expectations-execute))
        (t
         (eval-defun arg))))
(defun exps-current-form-is-expectations ()
  (save-excursion
    (beginning-of-defun)
    (looking-at "(expectations\\|(.+(fboundp 'expectations)\\|(dont-compile\n.*expectations")))
(defun expectations-execute (&optional arg)
  "Execute last-defined `expectations' test.
With prefix argument, do `batch-expectations-in-emacs'."
  (interactive "P")
  (if arg
      (batch-expectations-in-emacs)
    (save-selected-window
      (ert t)
      (exps-cleanup))))

(defalias 'batch-expectations 'ert-run-tests-batch-and-exit)
(defvar expectations-result-buffer "*expectations result*")
(defun batch-expectations-in-emacs ()
  "Execute expectations in current file with batch mode."
  (interactive)
  (with-current-buffer (get-buffer-create expectations-result-buffer)
    (buffer-disable-undo)
    (erase-buffer)
    (call-process (car command-line-args) nil expectations-result-buffer t
                  "-Q" "-batch" "--eval"
                  (format "(setq load-path (cons \".\" '%S))" load-path)
                  "-l" "ert-expectations" "-l" erte-last-filename
                  "-f" "ert-run-tests-batch-and-exit")
    (display-buffer expectations-result-buffer)))

;; (shell-command "emacs -Q -batch -L . -l ert-expectations -l el-expectations-success-sample.el -f ert-run-tests-batch-and-exit &")
;; (shell-command "emacs -Q -batch -L . -l ert-expectations -l el-expectations-success-sample.el -l el-expectations-failure-sample.el -f ert-run-tests-batch-and-exit &")

(substitute-key-definition 'eval-defun 'expectations-eval-defun emacs-lisp-mode-map)
(substitute-key-definition 'eval-defun 'expectations-eval-defun lisp-interaction-mode-map)

;;;; temporary buffer
(declare-function exps-tmpbuf "ert-expectations.el")
(declare-function exps-cleanup-tmpbuf "ert-expectations.el")
(lexical-let ((count 1))
  (defun exps-tmpbuf ()
    (with-current-buffer (get-buffer-create (format " *ert-expectations tmp:%d" count))
      (prog1 (current-buffer)
        (incf count)
        (erase-buffer))))
  (defun exps-cleanup-tmpbuf ()
    (setq count 1)
    (loop for buf in (buffer-list)
          for bname = (buffer-name buf)
          when (string-match " \\*ert-expectations tmp:" bname)
          do (kill-buffer buf))))
(defun exps-cleanup ()
  (exps-cleanup-tmpbuf))

;;;; edit support
(dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (font-lock-add-keywords
   mode
   '(("\\<\\(expectations\\|expect\\)\\>" 0 font-lock-keyword-face)
     (exps-font-lock-desc 0 font-lock-warning-face prepend)
     (exps-font-lock-expected-value 0 font-lock-function-name-face prepend))))

(defun exps-font-lock-desc (limit)
  (when (re-search-forward "(desc\\s " limit t)
    (backward-up-list 1)
    (set-match-data (list (point) (progn (forward-sexp 1) (point))))
    t))

;; I think expected value is so-called function name of `expect'.
(defun exps-font-lock-expected-value (limit)
  (when (re-search-forward "(expect\\s " limit t)
    (forward-sexp 1)
    (let ((e (point)))
      (forward-sexp -1)
      (set-match-data (list (point) e))
        t)))


;;;; test code
(dont-compile
  (unless (fboundp 'evenp)
    (defun evenp (x) (zerop (% x 2)))
    (defun oddp (x) (= 1 (% x 2))))
  (put 'hoge-error 'error-conditions '(hoge-error error))
  (setq erte-test--success
        '((expect (+ 3) "a" (+ 1 2))
          (expect (type char-or-string) "a")
          (expect (buffer "*scratch*") (with-current-buffer "*scratch*" (current-buffer)))
          (expect "abce" "abce")
          (expect '(1 2) (list 1 2))
          (expect (error arith-error) (/ 1 0))
          (expect (error) (/ 1 0))
          (expect (error wrong-number-of-arguments '(= 3)) (= 1 2 3 ))
          (expect (error hoge-error '("hoge")) (signal 'hoge-error '("hoge")))
          (expect (error hoge-error `(1 (,(1+ 1) 3))) (signal 'hoge-error '(1 (2 3))))
          (expect (error error *) (error "message"))
          (expect (error-message "ERROR!!") (error "ERROR!!"))
          (expect (no-error) 1)
          (expect (no-error) nil)
          (expect 5 (stub wawa => 5) (wawa 9999))
          (expect (mock (foo 1 2)) (foo 1 2))
          (expect (mock (foo 1 2)) (foo 1 2))
          (expect (not-called hoge) 1)
          (expect (true) t)
          (expect (non-nil) 1)))
  (setq erte-test--fail
        '((expect (+ 3) "a" (+ 1 4))
          (expect (type char-or-string) 'a)
          (expect (buffer "*scrtch*") (with-current-buffer "*scratch*" (current-buffer)))
          (expect "abce" "abde")
          (expect (1 2) (list 1 2))
          (expect (error end-of-file) (/ 1 0))
          (expect (error) (/ 1 1))
          (expect (error wrong-number-of-arguments '(= 4)) (= 1 2 3 ))
          (expect (error hoge-error '("hage")) (signal 'hoge-error '("hoge")))
          (expect (error hoge-error `(1 (,(1+ 2) 3))) (signal 'hoge-error '(1 (2 3))))
          (expect (error error *) (/ 1 1))
          (expect (error-message "ERROR!!") (error "!!!"))
          (expect (no-error) (error "error!"))
          (expect (no-error) (error "error!"))
          (expect 6 (stub wawa => 5) (wawa 9999))
          (expect (mock (foo 1 4)) (foo 1 2))
          (expect (mock (foo 1 4)) 1)
          (expect (not-called hoge) (hoge))
          (expect (true) nil)
          (expect (non-nil) nil)))
  (defun erte-test-command ()
    "Self test of ert-expectations.
The right result is .F.F.F.F ..."
    (interactive)
    (eval `(erte-test
             (desc "erte-test-command")
             ,@(loop for success in erte-test--success
                     for fail in erte-test--fail
                     append (list success fail)))))
  (defun erte-test--fail ()
    "Self test of ert-expectations. Only failed tests."
    (interactive)
    (eval `(erte-test
             ,@erte-test--fail)))
  (defun erte-test--success ()
    "Self test of ert-expectations. Only successful tests."
    (interactive)
    (eval `(erte-test
             ,@erte-test--success))))

;;;; Bug report
(defvar erte-maintainer-mail-address
  (concat "rubiki" "tch@ru" "by-lang.org"))
(defvar erte-bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of ert-expectations.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"ert-expectations.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Print the result of M-x erte-test-command.
  5) Type C-c C-c to send.
# If you are a Japanese, please write in Japanese:-)")
(defun erte-send-bug-report ()
  (interactive)
  (reporter-submit-bug-report
   erte-maintainer-mail-address
   "ert-expectations.el"
   (apropos-internal "^erte-" 'boundp)
   nil nil
   erte-bug-report-salutation))

(provide 'ert-expectations)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "ert-expectations.el")
;;; ert-expectations.el ends here

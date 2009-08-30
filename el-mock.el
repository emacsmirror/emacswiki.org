;;; el-mock.el --- Tiny Mock and Stub framework in Emacs Lisp
;; $Id: el-mock.el,v 1.17 2008/08/28 19:04:48 rubikitch Exp $

;; Copyright (C) 2008  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: lisp, testing, unittest
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Emacs Lisp Mock is a library for mocking and stubbing using
;; readable syntax. Most commonly Emacs Lisp Mock is used in
;; conjunction with Emacs Lisp Expectations, but it can be used in
;; other contexts.

;; Emacs Lisp Mock provides two scope interface of mock and stub:
;; `with-mock' and `mocklet'. `with-mock' only defines a
;; scope. `mocklet' is more sophisticated interface than `with-mock':
;; `mocklet' defines local mock and stub like `let', `flet', and
;; `macrolet'.

;; Within `with-mock' body (or argument function specified in
;; `mock-protect'), you can create a mock and a stub. To create a
;; stub, use `stub' macro. To create a mock, use `mock' macro.
  
;; For further information: see docstrings.
;; [EVAL IT] (describe-function 'with-mock)
;; [EVAL IT] (describe-function 'mocklet)
;; [EVAL IT] (describe-function 'stub)
;; [EVAL IT] (describe-function 'mock)

;;; History:

;; $Log: el-mock.el,v $
;; Revision 1.17  2008/08/28 19:04:48  rubikitch
;; Implement `not-called' mock.
;;
;; Revision 1.16  2008/08/28 18:23:28  rubikitch
;; unit test: use dont-compile
;;
;; Revision 1.15  2008/04/18 18:02:24  rubikitch
;; bug fix about symbol
;;
;; Revision 1.14  2008/04/13 18:23:43  rubikitch
;; removed `message' advice.
;; mock-suppress-redefinition-message: suppress by empty message
;;
;; Revision 1.13  2008/04/12 17:36:11  rubikitch
;; raise mock-syntax-error when invalid `mock' and `stub' spec.
;;
;; Revision 1.12  2008/04/12 17:30:33  rubikitch
;; inhibit using `mock' and `stub' outside `mock-protect' function.
;;
;; Revision 1.11  2008/04/12 17:10:42  rubikitch
;; * added docstrings.
;; * `stublet' is an alias of `mocklet'.
;;
;; Revision 1.10  2008/04/12 16:14:16  rubikitch
;; * allow omission of return value
;; * (mock foo 2) and (stub foo 2) cause error now
;; * arranged test
;;
;; Revision 1.9  2008/04/12 15:10:32  rubikitch
;; changed mocklet syntax
;;
;; Revision 1.8  2008/04/12 14:54:16  rubikitch
;; added Commentary
;;
;; Revision 1.7  2008/04/10 16:14:02  rubikitch
;; fixed advice-related bug
;;
;; Revision 1.6  2008/04/10 14:08:32  rubikitch
;; *** empty log message ***
;;
;; Revision 1.5  2008/04/10 14:01:48  rubikitch
;; arranged code/test
;;
;; Revision 1.4  2008/04/10 12:57:00  rubikitch
;; mock verify
;;
;; Revision 1.3  2008/04/10 07:50:10  rubikitch
;; *** empty log message ***
;;
;; Revision 1.2  2008/04/10 07:48:04  rubikitch
;; New functions:
;; stub/setup
;; stub/teardown
;; stub/parse-spec
;;
;; refactored with-stub-function
;;
;; Revision 1.1  2008/04/10 07:37:54  rubikitch
;; Initial revision
;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'advice)

;;;; stub setup/teardown
(defun stub/setup (funcsym value)
  (mock-suppress-redefinition-message
   (lambda ()
     (when (fboundp funcsym)
       (put 'mock-original-func funcsym (symbol-function funcsym)))
     (ad-safe-fset funcsym `(lambda (&rest x) ,value)))))

(defun stub/teardown (funcsym)
  (mock-suppress-redefinition-message
   (lambda ()
     (let ((func (get 'mock-original-func funcsym)))
       (if (not func)
           (fmakunbound funcsym)
         (ad-safe-fset funcsym func)
         ;; may be unadviced
         )))))
    
;;;; mock setup/teardown
(defun mock/setup (func-spec value)
  (mock-suppress-redefinition-message
   (lambda ()
     (let ((funcsym (car func-spec)))
       (when (fboundp funcsym)
         (put 'mock-original-func funcsym (symbol-function funcsym)))
       (put 'mock-not-yet-called funcsym t)
       (ad-safe-fset funcsym
                     `(lambda (&rest actual-args)
                        (put 'mock-not-yet-called ',funcsym nil)
                        (add-to-list 'mock-verify-list
                                     (list ',funcsym ',(cdr func-spec) actual-args))
                        ,value))))))

(defun not-called/setup (funcsym)
  (mock-suppress-redefinition-message
   (lambda ()
     (let ()
       (when (fboundp funcsym)
         (put 'mock-original-func funcsym (symbol-function funcsym)))
       (ad-safe-fset funcsym
                     `(lambda (&rest actual-args)
                        (signal 'mock-error '(called))))))))

(defalias 'mock/teardown 'stub/teardown)

;;;; mock verify
(put 'mock-error 'error-conditions '(mock-error error))
(put 'mock-error 'error-message "Mock error")
(defun mock-verify ()
  (when (loop for f in -mocked-functions
              thereis (get 'mock-not-yet-called f))
    (signal 'mock-error '(not-called)))
  (loop for (funcsym expected-args actual-args) in mock-verify-list
        do
        (mock-verify-args funcsym expected-args actual-args)))

(defun mock-verify-args (funcsym expected-args actual-args)
  (loop for e in expected-args
        for a in actual-args
        do
        (unless (eq e '*)               ; `*' is wildcard argument
          (unless (equal (eval e) a)
            (signal 'mock-error (list (cons funcsym expected-args)
                                      (cons funcsym actual-args)))))))
;;;; stub/mock provider
(defvar -stubbed-functions nil)
(defvar -mocked-functions nil)
(defvar mock-verify-list nil)
(defvar in-mocking nil)
(defun mock-protect (body-fn)
  "The substance of `with-mock' macro.
Prepare for mock/stub, call BODY-FN, and teardown mock/stub.

For developer:
When you adapt Emacs Lisp Mock to a testing framework, wrap test method around this function."
  (let (mock-verify-list
        -stubbed-functions
        -mocked-functions
        (in-mocking t))
    (setplist 'mock-original-func nil)
    (setplist 'mock-not-yet-called nil)
    (unwind-protect
        (funcall body-fn)
      (mapcar #'stub/teardown -stubbed-functions)
      (unwind-protect
          (mock-verify)
        (mapcar #'mock/teardown -mocked-functions)))))

;;;; message hack
(defun mock-suppress-redefinition-message (func)
  "Erase \"ad-handle-definition: `%s' got redefined\" message."
  (prog1
      (funcall func)
    (message "")))
(put 'mock-syntax-error 'error-conditions '(mock-syntax-error error))
(put 'mock-syntax-error 'error-message "Mock syntax error")

;;;; User interface
(defmacro with-mock (&rest body)
  "Execute the forms in BODY. You can use `mock' and `stub' in BODY.
The value returned is the value of the last form in BODY.
After executing BODY, mocks and stubs are guaranteed to be released.

Example:
  (with-mock
    (stub fooz => 2)
    (fooz 9999))                  ; => 2
"
  `(mock-protect
    (lambda () ,@body)))
(defalias 'with-stub 'with-mock)

(defmacro stub (function &rest rest)
  "Create a stub for FUNCTION.
Stubs are temporary functions which accept any arguments and return constant value.
Stubs are removed outside `with-mock' (`with-stub' is an alias) and `mocklet'.

Synopsis:
* (stub FUNCTION)
  Create a FUNCTION stub which returns nil.
* (stub FUNCTION => RETURN-VALUE)
  Create a FUNCTION stub which returns RETURN-VALUE.


Example:
  (with-mock
    (stub foo)
    (stub bar => 1)
    (and (null (foo)) (= (bar 7) 1)))     ; => t
"
  (let ((value (cond ((eq '=> (car rest))
                      (cadr rest))
                     ((null rest) nil)
                     (t (signal 'mock-syntax-error '("Use `(stub FUNC)' or `(stub FUNC => RETURN-VALUE)'"))))))
    `(if (not in-mocking)
         (error "Do not use `stub' outside")
       (stub/setup ',function ',value)
       (push ',function -stubbed-functions))))

(defmacro mock (func-spec &rest rest)
    "Create a mock for function described by FUNC-SPEC.
Mocks are temporary functions which accept specified arguments and return constant value.
If mocked functions are not called or called by different arguments, an `mock-error' occurs.
Mocks are removed outside `with-mock' and `mocklet'.

Synopsis:
* (mock (FUNCTION ARGS...))
  Create a FUNCTION mock which returns nil.
* (mock (FUNCTION ARGS...) => RETURN-VALUE)
  Create a FUNCTION mock which returns RETURN-VALUE.

Wildcard:
The `*' is a special symbol: it accepts any value for that argument position.

Example:
  (with-mock
    (mock (f * 2) => 3)
    (mock (g 3))
    (and (= (f 9 2) 3) (null (g 3))))     ; => t
  (with-mock
    (mock (g 3))
    (g 7))                                ; (mock-error (g 3) (g 7))
"
  (let ((value (cond ((eq '=> (car rest))
                      (cadr rest))
                     ((null rest) nil)
                     (t (signal 'mock-syntax-error '("Use `(mock FUNC-SPEC)' or `(mock FUNC-SPEC => RETURN-VALUE)'"))))))
    `(if (not in-mocking)
         (error "Do not use `mock' outside")
       (mock/setup ',func-spec ',value)
       (push ',(car func-spec) -mocked-functions))))

(defmacro not-called (function)
  "Create a not-called mock for FUNCTION.
Not-called mocks are temporary functions which raises an error when called.
If not-called functions are called, an `mock-error' occurs.
Not-called mocks are removed outside `with-mock' and `mocklet'.

Synopsis:
* (not-called FUNCTION)
  Create a FUNCTION not-called mock.

Example:
  (with-mock
    (not-called f)
    t)     ; => t
  (with-mock
    (not-called g)
    (g 7)) ; => (mock-error called)
"
  (let ()
    `(if (not in-mocking)
         (error "Do not use `not-called' outside")
       (not-called/setup ',function)
       (push ',function -mocked-functions))))


(defun mock-parse-spec (spec)
  (cons 'progn
        (mapcar (lambda (args)
                  (if (eq (cadr args) 'not-called)
                      `(not-called ,(car args))
                    (cons (if (consp (car args)) 'mock 'stub)
                        args)))
                spec)))

(defun mocklet-function (spec body-func)
  (with-mock
    (eval (mock-parse-spec spec))
    (funcall body-func)))

(defmacro mocklet (speclist &rest body)
  "`let'-like interface of `with-mock', `mock', `not-called' and `stub'.

Create mocks and stubs described by SPECLIST then execute the forms in BODY.
SPECLIST is a list of mock/not-called/stub spec.
The value returned is the value of the last form in BODY.
After executing BODY, mocks and stubs are guaranteed to be released.

Synopsis of spec:
Spec is arguments of `mock', `not-called' or `stub'.
* ((FUNCTION ARGS...))                  : mock which returns nil
* ((FUNCTION ARGS...) => RETURN-VALUE)  ; mock which returns RETURN-VALUE
* (FUNCTION)                            : stub which returns nil
* (FUNCTION => RETURN-VALUE)            ; stub which returns RETURN-VALUE
* (FUNCTION not-called)                 ; not-called FUNCTION

Example:
  (mocklet (((mock-nil 1))
            ((mock-1 *) => 1)
            (stub-nil)
            (stub-2 => 2))
    (and (null (mock-nil 1))    (= (mock-1 4) 1)
         (null (stub-nil 'any)) (= (stub-2) 2))) ; => t
"
  `(mocklet-function ',speclist (lambda () ,@body)))

(defalias 'stublet 'mocklet)

(put 'with-mock 'lisp-indent-function 0)
(put 'with-stub 'lisp-indent-function 0)
(put 'mocklet 'lisp-indent-function 1)
(put 'stublet 'lisp-indent-function 1)

;;;; unit test
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "stub setup/teardown")
      (expect 2
        (stub/setup 'foo 2)
        (prog1
            (foo 1 2 3)
          (stub/teardown 'foo)))
      (expect nil
        (stub/setup 'foox 2)
        (foox 1 2 3)
        (stub/teardown 'foox)
        (fboundp 'foox))
      (desc "with-mock interface")
      (expect 9801
        (with-mock
          9801))
      (desc "stub macro")
      (expect nil
        (with-mock
          (stub hogehoges)
          (hogehoges 75)))
      (expect 2
        (with-mock
          (stub fooz => 2)
          (fooz 9999)))
      (expect nil
        (with-mock
          (stub fooz => 2)
          (fooz 3))
        (fboundp 'fooz))
      (expect nil
        (with-mock
          (stub hoge)                   ;omission of return value
          (hoge)))
      (expect 'hoge
        (with-mock
          (stub me => 'hoge)
          (me 1)))
      (expect 34
        (with-mock
          (stub me => (+ 3 31))
          (me 1)))
      ;; TODO defie mock-syntax-error / detect mock-syntax-error in expectations 
      (desc "abused stub macro")
      (expect (error mock-syntax-error '("Use `(stub FUNC)' or `(stub FUNC => RETURN-VALUE)'"))
        (with-mock
          (stub fooz 7)))
      (expect (error-message "Do not use `stub' outside")
        (let (in-mocking) ; while executing `expect', `in-mocking' is t.
          (stub hahahaha)))
      (desc "mock macro")
      (expect 2
        (with-mock
          (mock (foom 5) => 2)
          (foom 5)))
      (expect 3
        (with-mock
          (mock (foo 5) => 2)
          (mock (bar 7) => 1)
          (+ (foo 5) (bar 7))))
      (expect 3
        (flet ((plus () (+ (foo 5) (bar 7))))
          (with-mock
            (mock (foo 5) => 2)
            (mock (bar 7) => 1)
            (plus))))
      (expect 1
        (with-mock
          (mock (f * 2) => 1)
          (f 1 2)))
      (expect 1
        (with-mock
          (mock (f * (1+ 1)) => (+ 0 1)) ;evaluated
          (f 1 2)))
      (expect nil
        (with-mock
          (mock (f 2))                  ;omission of return value
          (f 2)))
      (expect 'hoge
        (with-mock
          (mock (me 1) => 'hoge)
          (me 1)))
      (expect 34
        (with-mock
          (mock (me 1) => (+ 3 31))
          (me 1)))

      (desc "unfulfilled mock")
      (expect (error mock-error '((foom 5) (foom 6)))
        (with-mock
          (mock (foom 5) => 2)
          (foom 6)))
      (expect (error mock-error '((bar 7) (bar 8)))
        (with-mock
          (mock (foo 5) => 2)
          (mock (bar 7) => 1)
          (+ (foo 5) (bar 8))))
      (expect (error mock-error '(not-called))
        (with-mock
          (mock (foo 5) => 2)))
      (expect (error mock-error '(not-called))
        (with-mock
          (mock (vi 5) => 2)
          (mock (foo 5) => 2)
          (vi 5)))
      (expect (error mock-error '((f 2) (f 4)))
        (with-mock
          (mock (f 2))                  ;omission of return value
          (f 4)))
      (desc "abused mock macro")
      (expect (error mock-syntax-error '("Use `(mock FUNC-SPEC)' or `(mock FUNC-SPEC => RETURN-VALUE)'"))
        (with-mock
          (mock (fooz) 7)))
      (expect (error-message "Do not use `mock' outside")
        (let (in-mocking) ; while executing `expect', `in-mocking' is t.
          (mock (hahahaha))))

      (desc "mock with stub")
      (expect 8
        (with-mock
          (mock (f 1 2) => 3)
          (stub hoge => 5)
          (+ (f 1 2) (hoge 'a))))
      (expect (error mock-error '((f 1 2) (f 3 4)))
        (with-mock
          (mock (f 1 2) => 3)
          (stub hoge => 5)
          (+ (f 3 4) (hoge 'a))))

      (desc "with-stub is an alias of with-mock")
      (expect 'with-mock
        (symbol-function 'with-stub))

      (desc "stublet is an alias of mocklet")
      (expect 'mocklet
        (symbol-function 'stublet))

      (desc "mock-parse-spec")
      (expect '(progn
                 (mock (f 1 2) => 3)
                 (stub hoge => 5))
        (mock-parse-spec
         '(((f 1 2) => 3)
           (hoge    => 5))))
      (expect '(progn
                 (not-called g))
        (mock-parse-spec
         '((g not-called))))

      (desc "mocklet")
      (expect 8
        (mocklet (((f 1 2) => 3)
                  (hoge    => 5))
          (+ (f 1 2) (hoge 'a))))
      (expect 2
        (mocklet ((foo => 2))
          (foo 1 2 3)))
      (expect 3
        (defun defined-func (x) 3)
        (prog1
            (mocklet ((defined-func => 3))
              (defined-func 3))
          (fmakunbound 'defined-func)))
      (expect nil
        (mocklet ((f))                  ;omission of return value
          (f 91)))
      (expect nil
        (mocklet (((f 76)))             ;omission of return value
          (f 76)))
      (expect 5
        (mocklet ((a => 3)
                  (b => 2))
          1                             ;multiple exprs
          (+ (a 999) (b 7))))

      (desc "stub for defined function")
      (expect "xxx"
        (defun blah (x) (* x 2))
        (prog1
            (let ((orig (symbol-function 'blah)))
              (mocklet ((blah => "xxx"))
                (blah "xx")))
          (fmakunbound 'blah)))
      (expect t
        (defun blah (x) (* x 2))
        (prog1
            (let ((orig (symbol-function 'blah)))
              (mocklet ((blah => "xx"))
                (blah "xx"))
              (equal orig (symbol-function 'blah)))
          (fmakunbound 'blah)))

      (desc "stub for adviced function")
      (expect "xxx"
        (mock-suppress-redefinition-message ;silence redefinition warning
         (lambda () 
           (defun fugaga (x) (* x 2))
           (defadvice fugaga (around test activate)
             (setq ad-return-value (concat "[" ad-return-value "]")))
           (prog1
               (let ((orig (symbol-function 'fugaga)))
                 (mocklet ((fugaga => "xxx"))
                   (fugaga "aaaaa")))
             (fmakunbound 'fugaga)))))
      (expect t
        (mock-suppress-redefinition-message
         (lambda ()
           (defun fugaga (x) (* x 2))
           (defadvice fugaga (around test activate)
             (setq ad-return-value (concat "[" ad-return-value "]")))
           (prog1
               (let ((orig (symbol-function 'fugaga)))
                 (mocklet ((fugaga => "xx"))
                   (fugaga "aaaaa"))
                 (equal orig (symbol-function 'fugaga)))
             (fmakunbound 'fugaga)))))

      (desc "mock for adviced function")
      (expect "xx"
        (mock-suppress-redefinition-message
         (lambda ()
           (defun fugaga (x) (* x 2))
           (defadvice fugaga (around test activate)
             (setq ad-return-value (concat "[" ad-return-value "]")))
           (prog1
               (let ((orig (symbol-function 'fugaga)))
                 (mocklet (((fugaga "aaaaa") => "xx"))
                   (fugaga "aaaaa")))
             (fmakunbound 'fugaga)))))
      (expect t
        (mock-suppress-redefinition-message
         (lambda ()
           (defun fugaga (x) (* x 2))
           (defadvice fugaga (around test activate)
             (setq ad-return-value (concat "[" ad-return-value "]")))
           (prog1
               (let ((orig (symbol-function 'fugaga)))
                 (mocklet (((fugaga "aaaaa") => "xx"))
                   (fugaga "aaaaa"))
                 (equal orig (symbol-function 'fugaga)))
             (fmakunbound 'fugaga)))))
      (desc "not-called macro")
      (expect 'ok
        (with-mock
          (not-called foom)
          'ok))
      (desc "mocklet/notcalled")
      (expect 'ok
        (mocklet ((foom not-called))
          'ok))
      (desc "unfulfilled not-called")
      (expect (error mock-error '(called))
        (with-mock
          (not-called hoge)
          (hoge 1)))
      (desc "abused not-called macro")
      (expect (error-message "Do not use `not-called' outside")
        (let (in-mocking) ; while executing `expect', `in-mocking' is t.
          (not-called hahahaha)))
      (desc "not-called for adviced function")
      (expect "not-called"
        (mock-suppress-redefinition-message ;silence redefinition warning
         (lambda () 
           (defun fugaga (x) (* x 2))
           (defadvice fugaga (around test activate)
             (setq ad-return-value (concat "[" ad-return-value "]")))
           (prog1
               (let ((orig (symbol-function 'fugaga)))
                 (mocklet ((fugaga not-called))
                   "not-called"))
             (fmakunbound 'fugaga)))))
      (expect t
        (mock-suppress-redefinition-message
         (lambda ()
           (defun fugaga (x) (* x 2))
           (defadvice fugaga (around test activate)
             (setq ad-return-value (concat "[" ad-return-value "]")))
           (prog1
               (let ((orig (symbol-function 'fugaga)))
                 (mocklet ((fugaga not-called))
                   "not-called")
                 (equal orig (symbol-function 'fugaga)))
             (fmakunbound 'fugaga)))))

      
      )))

(provide 'el-mock)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "el-mock.el")
;;; el-mock.el ends here


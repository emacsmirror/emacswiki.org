;;; el-expectations-success-sample.el --- success example of Emacs Lisp Expectations.
(unless (fboundp 'evenp)
  (defun evenp (x) (zerop (% x 2)))
  (defun oddp (x) (= 1 (% x 2))))
(put 'hoge-error 'error-conditions '(hoge-error error))

(expectations
  ;; `desc' is only a delimiter.
  (desc "run-hook-with-args-until-success")
  ;; Body should equal t or nil.
  (expect t
    (let ((hook '(evenp)))
      (run-hook-with-args-until-success 'hook 4)))
  (expect nil
    (let ((hook '(evenp)))
      (run-hook-with-args-until-success 'hook 5)))
  (expect t
    (let ((hook '(evenp oddp)))
      (run-hook-with-args-until-success 'hook 5)))

  (desc "buffer")
  (expect "*scratch*"
    (with-current-buffer "*scratch*"
      (buffer-name (current-buffer))))
  ;; Body should eq #<buffer *scratch*>
  (expect (buffer "*scratch*")
    (with-current-buffer "*scratch*"
      (current-buffer)))

  (desc "regexp")
  ;; Expects (string-match "o" "hoge")
  (expect (regexp "o")
    "hoge")

  (desc "type")
  ;; Body should be a integer.
  (expect (type integer)
    5)
  ;; Body should be a buffer object.
  (expect (type buffer)
    (current-buffer))
  ;; Body should be a sequence (sequencep nil).
  (expect (type sequence)
    nil)
  (expect (type char-or-string)
    "a")
  
  (desc "error")
  ;; Body should raise arith-error.
  (expect (error arith-error)
    (/ 1 0))
  ;; Body should raise any error.
  (expect (error)
    (/ 1 0))
  (expect (error wrong-number-of-arguments '(= 3))
    (= 1 2 3 ))
  (expect (error hoge-error '("hoge"))
    (signal 'hoge-error '("hoge")))
  (expect (error hoge-error `(1 (,(1+ 1) 3)))
    (signal 'hoge-error '(1 (2 3))))
  (expect (error error *)
      (error "message"))
  (desc "error-message")
  (expect (error-message "ERROR!!")
    (error "ERROR!!"))
  (desc "no-error")
  (expect (no-error)
    1)
  (desc "eval")
  (expect '(1 2)
    (list 1 2))
  (expect (get-buffer "*scratch*")
    (with-current-buffer "*scratch*"
      (current-buffer)))

  (desc "stub function")
  (expect 5
    (stub wawa => 5)
    (wawa 9999))
  (expect nil
    (fboundp 'wawa))

  (desc "mock")
  (expect (mock (foo 1 2) => 3)
    (foo 1 2))
  (expect (mock (foo * 3) => nil)
    (foo 9 3))
  (desc "mock with stub")
  (expect (mock (foo 5 * 7) => nil)
    ;; Stub function `hoge', which accepts any arguments and returns 3.
    (stub hoge => 3)
    (foo (+ 2 (hoge 10)) 6 7))
  (desc "any true value")
  (expect (true)
    t)
  (expect (true)
    1)
  (expect (non-nil)
    1)
  (expect (non-nil)
    "")
  (desc "not-called")
  (expect (not-called hoge)
    1)

  (desc "tmpbuf")
  (expect "foo"
    (with-current-buffer (exps-tmpbuf)
      (insert "foo")
      (buffer-string)))
  )
;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "el-expectations-success-sample.el")
;; Local Variables:
;; no-byte-compile:   t
;; End:
;;; el-expectations-success-sample.el ends here


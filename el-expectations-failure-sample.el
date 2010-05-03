;;; el-expectations-failure-sample.el --- failure example of Emacs Lisp Expectations.
(expectations
  (desc "error test")
  (expect 4
    (error "hoge")
    4)

  (desc "eval")
  (expect 5 4)
  (expect "hoge" "hage")
  (expect '(1) '(2))
  (expect (get-buffer-create "buf1") (get-buffer-create "buf2"))

  (desc "buffer")
  (expect (buffer "*scratch*")
    (get-buffer-create "*fail*"))

  (desc "regexp")
  (expect (regexp "o")
    "hage")

  (desc "type")
  (expect (type string)
    1)
  (desc "error")
  (expect (error arith-error)
    (/ 1 1))
  (expect (error end-of-file)
    (/ 1 0))
  (expect (error)
    (/ 1 1))
  (expect (error error *)
    (/ 1 1))
  ;; expected error data is evaluated.
  (expect (error wrong-number-of-arguments '(= 4))
    (= 1 2 3 ))
  (desc "no-error")
  (expect (no-error)
    (error "error!"))
  (desc "error-message")
  (expect (error-message "ERROR!!")
    (error "!!!"))
  (expect (error-message "ERROR!!")
    1)

  (desc "mock")
  (expect (mock (foo 1 2) => 3)
    (foo 1 4))
  (expect (mock (foo * 3) => nil)
    (foo 9 8))
  (expect (mock (foo 7) => nil)
    1)
  (desc "mock with stub")
  (expect (mock (foo * 3) => 2)
    (stub a => 4)
    (foo 3 (a 7)))
  (desc "any true value")
  (expect (true)
    nil)
  (expect (non-nil)
    nil)
  (desc "not-called")
  (expect (not-called hoge)
    (hoge))
  )

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "el-expectations-failure-sample.el")
;; Local Variables:
;; no-byte-compile:   t
;; End:
;;; el-expectations-failure-sample.el ends here

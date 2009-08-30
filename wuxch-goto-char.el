;;; quickly jump to special character.
;; wuxch-goto-char.el


(defun wuxch-go-to-char-forward()
  ""
  (interactive)
  (do-wuxch-go-to-char t)
  )

(defun wuxch-go-to-char-backward()
  ""
  (interactive)
  (do-wuxch-go-to-char nil)
  )

(defun do-wuxch-go-to-char (direction)
  "goto the point which char is same as user input. Set mark word away from point
   press up/down key to change direction
   press /control-g/ /enter/ to terminate, which /enter/ leave mark active "
  (push-mark)
  (let ((input)(end-of-input nil)(leave-mark-active t))
    (while (not end-of-input)
      (progn
        ;; 读取键盘输入，根据查找方向显示不同的提示
        (setq input (read-key-sequence (if direction "input char(forward):" "input char(backward):" ) ))
        ;; (message "input:%s" input)
        (cond
         ;; enter，退出查找，显示mark高亮
         ((or (equal input "\C-m") (equal input "\C-j"))
          (progn
            ;; (message "receive:%s" input)
            (setq leave-mark-active t)
            (setq end-of-input t)))
         ;; control-g，退出查找，不显示mark高亮
         ((equal input "\C-g")
          (progn
            ;; (message "receive:%s" input)
            (setq leave-mark-active nil)
            (setq end-of-input t)))
         ;; 上、下箭头用于改变查找的方向
         ((equal input [down])
          (progn
            ;; (message "set forward direction")
            (setq direction t)))

         ((equal input [up])
          (progn
            ;; (message "set backword direction")
            (setq direction nil)))

         ((wuxch-go-to-char-judge-legal-char input)
          (progn
            ;; (message "receive legal char:%s" input)
            (if direction
                (wuxch-search-forward-char-and-mark-word input)
              (wuxch-search-backward-char-and-mark-word input))
            ))

         (t
          (progn
            ;; (message "receive:%s" input)
            ))
         )
        )
      )
    (if (not leave-mark-active)
        (deactivate-mark))
    )

  (message "Done!")
  )

(defun wuxch-go-to-char-judge-legal-char( str )
  "Judge if the first char of str is legal"
  (let ((c (string-to-char str)))
    (or
     (and (<= c (string-to-char "z")) (>= c (string-to-char "a")))
     (and (<= c (string-to-char "Z")) (>= c (string-to-char "A")))
     (and (<= c (string-to-char "9")) (>= c (string-to-char "0")))
     (or (= c (string-to-char "_"))
         (= c (string-to-char "-"))
         (= c (string-to-char "+"))
         (= c (string-to-char "*")) )
     (or (= c (string-to-char "("))
         (= c (string-to-char ")"))
         (= c (string-to-char "="))
         (= c (string-to-char "'")) )
     (or (= c (string-to-char "["))
         (= c (string-to-char "]"))
         (= c (string-to-char "{"))
         (= c (string-to-char "}")) )
     (or (= c (string-to-char "."))
         (= c (string-to-char ","))
         (= c (string-to-char ";"))
         (= c (string-to-char ":")) )
     )
    )
  )

(defun wuxch-search-forward-char-and-mark-word( input )
  ""
  (forward-char 1)
  (if (search-forward input nil t 1)
      (progn
        ;; (message "search-forward,found char:%s" input)
        (backward-char 1)
        (mark-word)
        )
    (progn
      (message "search-forward,not found char:%s" input)
      (backward-char 1)))
  )

(defun wuxch-search-backward-char-and-mark-word( input )
  ""
  (if (search-backward input nil t 1)
      (progn
        ;; (message "search-backward,found char:%s" input)
        (mark-word)
        )
    (progn
      (message "search-backward,not found char:%s" input)))
  )


(global-set-key [(control \;)]  'ignore)
(global-set-key [(control \;)]  'wuxch-go-to-char-forward)

(global-set-key [(control \')]  'ignore)
(global-set-key [(control \')]  'wuxch-go-to-char-backward)


(provide 'wuxch-goto-char)

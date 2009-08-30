;;; functions about date and time.

(cond ((string= wuxch-computer-id "X60")
       ()
       )
      ((string= wuxch-computer-id "X31")
       (setq display-time-format "%Y/%m/%d")
       (display-time)
       )
      (t
       (setq display-time-format "%Y/%m/%d")
       (display-time))
      )


;; date命令插入当前时间
(defun date ()
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "%m/%d/%Y %H:%M:%S")))
;; 关于时间输出控制符
;; %c is the locale's date and time format.
;; %x is the locale's "preferred" date format.
;; %D is like "%m/%d/%y".
;; %R is like "%H:%M", %T is like "%H:%M:%S", %r is like "%I:%M:%S %p".
;; %X is the locale's "preferred" time format.
;; Finally, %n is a newline, %t is a tab, %% is a literal %.
;; Certain flags and modifiers are available with some format controls.
;; The flags are `_', `-', `^' and `#'.  For certain characters X,
;; %_X is like %X, but padded with blanks; %-X is like %X,
;; but without padding.  %^X is like %X, but with all textual
;; characters up-cased; %#X is like %X, but with letter-case of
;; all textual characters reversed.
;; %NX (where N stands for an integer) is like %X,
;; but takes up at least N (a number) positions.
;; The modifiers are `E' and `O'.  For certain characters X,
;; %EX is a locale's alternative version of %X;
;; %OX is like %X, but uses the locale's number symbols.


(provide 'wuxch-date)


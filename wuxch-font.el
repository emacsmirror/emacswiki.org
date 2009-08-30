;;; configuration of font setting, both english and chinese fonts.

;; 中文是英文的二倍
;; courier-13/simsun-16(windows xp)
;; 9x15/simsun-16
;; sony/simsun-16
;; bitstream vera sans mono-14/simsun-16
;; courier-13/simsun-16(windows xp)
;; dejavu sans mono 14/yahei 16

;; 查看光标位置字符信息的方法是 C-u C-x =
(global-set-key [(control =)] 'describe-char)

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

(if (not-linux)
    (progn
      (prefer-coding-system 'chinese-gbk-dos)

      (setq w32-charset-info-alist
            (cons '("gbk" w32-charset-gb2312 . 936) w32-charset-info-alist))

      ;; (setq default-frame-alist
      ;;       (append
      ;;        '((font . "fontset-gbk")) default-frame-alist))

      )
  )


(defun wuxch-set-font (en-font cn-font)
  (create-fontset-from-fontset-spec
   (replace-regexp-in-string "iso8859-1" "fontset-wuxch" en-font))
  (dolist (fnset '("fontset-default" "fontset-wuxch"))
    (set-fontset-font fnset 'gb18030 cn-font)
    (set-fontset-font fnset 'iso-8859-15 en-font))
  (set-default-font "fontset-wuxch")
  (setq default-frame-alist
        (append
         '((font . "fontset-wuxch"))
         default-frame-alist)))

(defun load-yahei ()
  (interactive)
  (if (not (frame-parameter nil 'font-backend))
      (progn
        (setq ascii-font
              "-outline-Courier New-normal-r-normal-normal-13-*-96-96-c-*-iso8859-1")
        (setq chinese-font
              "-outline-微软雅黑-normal-r-normal-normal-16-*-96-96-p-*-gb2312.1980-0"
              )
        (wuxch-set-font ascii-font chinese-font)
        )
    )
  )


(defun load-songti ()
  (interactive)
  (if (not (frame-parameter nil 'font-backend))
      (progn
        (setq ascii-font
              "-outline-Courier New-normal-r-normal-normal-13-97-96-96-c-*-iso8859-1")
        (setq chinese-font
              "-outline-宋体-normal-r-normal-normal-16-120-96-96-p-*-iso10646-1")
        (wuxch-set-font ascii-font chinese-font)
        )
    )
  )


(defun load-songti-linux ()
  (interactive)
  (set-face-font 'default "-Adobe-Courier-Medium-R-Normal--18-180-75-75-M-110-ISO8859-1")
  )

(defun load-songti-2009 ()
  "load-songti-2009:"
  (set-default-font "Courier New-11")
  (set-fontset-font (frame-parameter nil 'font)
                    'han (font-spec :family "SimSun" :size 18))
  (set-fontset-font (frame-parameter nil 'font)
                    'symbol (font-spec :family "SimSun" :size 18))
  (set-fontset-font (frame-parameter nil 'font)
                    'cjk-misc (font-spec :family "SimSun" :size 18))
  (set-fontset-font (frame-parameter nil 'font)
                    'bopomofo (font-spec :family "SimSun" :size 18))
  )


(cond ((string= wuxch-computer-id "X60")
       (load-songti-2009)
       )
      ((string= wuxch-computer-id "X31")
       (load-songti)
       )
      ((string= wuxch-computer-id "linux")
       (load-songti-linux)
       )
      (t (load-songti))
      )

(provide 'wuxch-font)

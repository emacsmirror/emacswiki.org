;;; customize of fill column command, especially for chinese characters.

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
;; 不要在 fill 时在句号后加空格
(setq sentence-end-double-space nil)

;; 直接设定fill-column好像没有用，必须用 custom-set-variables方式
;; (setq fill-column 120)
(custom-set-variables
 '(fill-column 100)
 )

(defun wuxch-fill-buffer ()
  "fill the whole buffer"
  (interactive)
  (fill-region (point-min) (point-max))
  )

(defun wuxch-fill-dwim ()
  "fill current paragraph and move to next or mark region is there is a region"
  (interactive)
  (if mark-active
      (progn
        (fill-region (region-beginning) (region-end))
        )
    (progn
      (fill-paragraph t)
      ;; (push-mark)
      ;; (forward-paragraph)
      )
    )
  )

(global-set-key [(meta q)] 'wuxch-fill-dwim)

;; (setq paragraph-start "\f\\|[ \t]*$")
;; (setq paragraph-separate "[^ \t\f]*$")

(provide 'wuxch-fill-column)

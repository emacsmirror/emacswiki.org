;;; show hide label on fringe.

(require 'hideshow)

(setq hs-hide-comments-when-hiding-all nil)
(setq hs-isearch-open t)

(define-fringe-bitmap 'hs-marker [0 24 24 126 126 24 24 0])

(defface hs-fringe-face
  '((t (:foreground "#888" :box (:line-width 2 :color "gray75" :style released-button))))
  "Face used to highlight the fringe on folded regions"
  :group 'hideshow)

(custom-set-faces
 '(hs-fringe-face ((t (:foreground "yellow"))) t)
 )

(defun display-code-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((marker-string "*fringe-dummy*")
           (marker-length (length marker-string))
           (display-string (format " ...(%d)" (count-lines (overlay-start ov) (overlay-end ov))))
           )
      (overlay-put ov 'help-echo "Hiddent text. C-c,= to show")
      (put-text-property 0 marker-length 'display
                         (list 'left-fringe 'hs-marker 'hs-fringe-face) marker-string)
      (overlay-put ov 'before-string marker-string)
      (put-text-property 0 (length display-string) 'face 'hs-face display-string)
      (overlay-put ov 'display display-string)
      )))
(setq hs-set-up-overlay 'display-code-line-counts)

;; 此命令定制代码折叠后的显示，使用font-lock-warning-face
;; (setq hs-set-up-overlay
;;       (defun wuxch-display-code-line-counts (ov)
;;         (when (eq 'code (overlay-get ov 'hs))
;;           (overlay-put ov 'display
;;                        (propertize
;;                         (format " ... [hide %d line(s)]"
;;                                 (- (count-lines (overlay-start ov)
;;                                                 (overlay-end ov)) 1))
;;                         'face 'font-lock-warning-face)))))



(defun wuxch-c-java-hs-toggle-hiding ()
  "如果在大括号之后，或者是在已经折叠的位置，那么就执行hs-toggle-hiding"
  (interactive)
  (wuxch-do-hs-toggle-hiding ?{ )
  )

(defun wuxch-elisp-hs-toggle-hiding ()
  ""
  (wuxch-do-hs-toggle-hiding ?\( )
  )

(defun wuxch-do-hs-toggle-hiding (c)
  "如果c之后，或者是在已经折叠的位置，那么就执行hs-toggle-hiding"
  ;; 使用line-end-position保证在有折叠信息的任何一行双击都有作用
  (if (hs-overlay-at (line-end-position))
      (progn
        ;; 在折叠信息
        (hs-show-block))
    (progn
      (if (char-equal (char-after (point)) c )
          (progn
            ;; 在 c字符 后面
            (hs-hide-block)
            )
        (progn
          ;; 执行正常双击的操作
          ;; (mouse-drag-region event)
          ;; (wuxch-copy-word)
          )
        )
      )
    )
  )

(defun wuxch-hs-toggle-hideing (event)
  ""
  (interactive "e")
  (let ((prev-point))
    (mouse-set-point event)
    (setq prev-point (point))
    (back-to-indentation)
    (when (string= major-mode "emacs-lisp-mode")
      (wuxch-elisp-hs-toggle-hiding)
      )
    (when (or (string= major-mode "cc-mode")
              (string= major-mode "c++-mode")
              (string= major-mode "java-mode")
              (string= major-mode "c-mode")
              )
      (wuxch-c-java-hs-toggle-hiding)
      )
    (goto-char prev-point)
    )
  )

(global-set-key [C-down-mouse-1] 'ignore)
(global-set-key [C-down-mouse-1] 'wuxch-hs-toggle-hideing)


(provide 'wuxch-hideshow)

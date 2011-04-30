;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 字体设置 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar emacs-english-font "Droid Sans Mono"
  "The font name of English.")
(defvar emacs-cjk-font "Droid Sans Fallback"
  "The font name of CJK.")
(defvar emacs-font-size 10
  "The default font size.")

;; 英文字体
(set-frame-font (format "%s-%s" emacs-english-font emacs-font-size))
;; 中文字体
(set-fontset-font (frame-parameter nil 'font)
                  'unicode emacs-cjk-font)

(provide 'LazyCatFont)

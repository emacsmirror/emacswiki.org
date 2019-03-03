;;; smart-orange-theme --- A color theme that emphasizes oranges, reds, and yellows

;;; Commentary:
;;; inspired by https://medium.com/@caseorganic/why-the-color-of-technology-must-change-caae7f1ca061

;;; Code:

(deftheme smart-orange "Smart Orange")

(custom-theme-set-faces
 'smart-orange
 '(default ((t (:foreground "DarkOrange3" :background "black"))))
 '(cursor ((t (:background "Gray80"))))
 '(highlight ((t (:foreground "Orange2"))))
 '(hl-line ((t (:background "#303030"))))
 '(font-lock-keyword-face ((t (:foreground "LightGoldenrod" :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "OrangeRed3"))))
 '(font-lock-comment-face ((t (:foreground "YellowGreen" :slant italic))))
 '(font-lock-comment-delimiter-face ((t (:foreground "YellowGreen" :weight bold))))
 '(eshell-prompt ((t :foreground "orange")))
 '(comint-highlight-prompt ((t :foreground "orange" :weight bold)))
 '(link ((t :foreground "yellow")))
 '(minibuffer-prompt ((t :foreground "orange")))
 '(rainbow-delimiters-depth-1-face ((t :foreground "LightGoldenrod" :weight bold)))
 '(rainbow-delimiters-depth-2-face ((t :foreground "Red")))
 '(rainbow-delimiters-depth-3-face ((t :foreground "Orange")))
 '(rainbow-delimiters-depth-4-face ((t :foreground "Yellow")))
 '(rainbow-delimiters-depth-5-face ((t :foreground "Green")))
 '(rainbow-delimiters-depth-6-face ((t :foreground "Blue")))
 '(rainbow-delimiters-depth-7-face ((t :foreground "Purple"))))


(provide-theme 'smart-orange)

;;; smart-orange-theme.el ends here

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
 '(link ((t (:foreground "yellow"))))
 '(font-lock-builtin-face ((t (:weight bold))))
 '(font-lock-comment-delimiter-face ((t (:foreground "YellowGreen" :weight bold :slant italic))))
 '(font-lock-comment-face ((t (:foreground "YellowGreen" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "Orange" :underline t))))
 '(font-lock-doc-face ((t (:foreground "YellowGreen" :slant italic))))
 '(font-lock-function-name-face ((t (:foreground "Orange"))))
 '(font-lock-keyword-face ((t (:foreground "gold" :weight regular))))
 '(font-lock-negation-char-face ((t (:foreground "RedOrange" :weight bold))))
 '(font-lock-preprocessor-face ((t (:foreground "gold" :weight bold))))
 '(font-lock-string-face ((t (:foreground "Orange"))))
 '(font-lock-type-face ((t (:foreground "Orange" :slant italic))))
 '(font-lock-variable-name-face ((t (:foreground "Orange"))))
 '(font-lock-warning-face ((t (:background "DarkRed"))))
 '(show-paren-match ((t (:foreground  "Gray"))))
 '(show-paren-mismatch ((t (:foreground "Red"))))
 '(eshell-prompt ((t :foreground "orange")))
 '(comint-highlight-prompt ((t :foreground "orange" :weight bold)))
 '(minibuffer-prompt ((t :foreground "orange")))
 '(rainbow-delimiters-depth-1-face ((t :foreground "RedOrange" :weight bold)))
 '(rainbow-delimiters-depth-2-face ((t :foreground "Orange")))
 '(rainbow-delimiters-depth-3-face ((t :foreground "Yellow")))
 '(rainbow-delimiters-depth-4-face ((t :foreground "Green")))
 '(rainbow-delimiters-depth-5-face ((t :foreground "Blue")))
 '(rainbow-delimiters-depth-6-face ((t :foreground "Purple")))
 '(rainbow-delimiters-depth-7-face ((t :foreground "Grey"))))


(provide-theme 'smart-orange)

;;; smart-orange-theme.el ends here

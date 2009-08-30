;;; claire-mode.el --- for claire, an object oriented language

;; last update 15/10/2003 [dd/mm/yyyy]

;; claire-mode.el is free software use at your own risk

;;; Commentary:

;; for claire, an object oriented language [http://www.claire-language.com]
;;
;; I warn you this is a really big hack, it won't run too fast, 
;; but hopefully it won't break. 
;;
;; to use this file add the following to your 
;; ~/.emacs file
;;
;;     (load-library "~/.emacs.d/claire-mode")
;;
;; next time you edit a file with the .cl extension 
;; it will start in claire-mode mode (hopefully)

;;; Code:

;;keywords in claire-mode
(defvar claire-mode-keywords
'(
  "as" "assert" "any"
  "break"
  "case" "catch"
  "else" "exists"
  "for"
  "if" "in"
  "let" 
  "make_list" 
  "none" 
  "printf" 
  "return" 
  "trace" "try" 
  "until"
  "when" "while"  
  "Zif" 
  ))



;; regexp to find a function name
(defvar claire-mode-functions
    '(("\\[\\(\\(\\sw\\|\\s_\\)*\\)\(" 1 font-lock-function-name-face)
      ("\t" . 'show-paren-mismatch-face)))

;; types
(defvar claire-mode-types
    '((":\\(\\( \\)*\\sw*\\)" 1 font-lock-type-face))
)

;;parameters
(defvar claire-mode-parameters
    '(("[(,]\\(\\(\\sw\\|\\s_\\)*\\):" 1 font-lock-variable-name-face))
)

;;newline doesn't work!
(defvar claire-mode-comment
    '(("\\(//\\(.\\)*\\)" 1 font-lock-comment-face))
)

(defvar claire-mode-objects
    '(("\\(\\sw\\(\\sw\\|\\s_\\)*\\) <:" 1 font-lock-type-face))
)

(defvar claire-mode-constructor
    '(("<: \\(\\(\\sw\\|\\s_\\)*\\)\(" 1 font-lock-function-name-face))
)



(defvar claire-mode-operators
  '((" \\(:=\\|=\\|:\\|=>\\|>=\\|<=\\|!=\\|=\\|%\\|exists\\|forall\\|U\\|not\\) " 1 font-lock-builtin-face)
    )
 )

(defvar claire-mode-operators-strong
  '(("\\(->\\|::\\|<:\\)" 1 font-lock-warning-face)
    )
 )



;___________________________________________________
;
; THE MAGIC! defining claire-mode as a major mode 
;___________________________________________________



;; define as a sub class of generic mode.
(define-generic-mode 'claire-mode
  '("//" ";;")               ;; comments
  claire-mode-keywords       ;; key words 
  nil                        ;; variable font regexp [done below]
  '("\\.cl\\'")              ;; works with files called
  nil                        ;; not sure
  "Major mode for editing claire files ");;documentation





;___________________________________________________
;
; adding in the previously defined regexp's
;___________________________________________________

(font-lock-add-keywords 'claire-mode claire-mode-functions)
;(font-lock-add-keywords 'claire-mode claire-mode-operators)
(font-lock-add-keywords 'claire-mode claire-mode-operators-strong)
;(font-lock-add-keywords 'claire-mode claire-mode-parameters)
(font-lock-add-keywords 'claire-mode claire-mode-types)
(font-lock-add-keywords 'claire-mode claire-mode-comment)
(font-lock-add-keywords 'claire-mode claire-mode-objects)
(font-lock-add-keywords 'claire-mode claire-mode-constructor)

(provide 'claire-mode)
;;; claire-mode.el ends here


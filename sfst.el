;;; sfst-mode.el -- major mode for editing SFST files

; For Stuttgart Finite State Transducer Tools (SFST), see
; http://www.ims.uni-stuttgart.de/projekte/gramotron/SOFTWARE/SFST.html

;; Copyright (C) 2006 Sebastian Nagel ;;

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


(provide 'sfst)

(defvar sfst-mode-hook nil)

(defconst sfst-version "0.02" "Version of sfst-mode")
;; 0.01  --  most functionalities
;; 0.02  --  all operators are now highlighted, support for symbol variables, escaped [!|] etc. not highlighted

(setq auto-mode-alist ; files for which sfst-mode will be invoked.
      (append '(("\\.s?fst$" . sfst-mode)
                ) auto-mode-alist))


(defvar sfst-mode-syntax-table
  (let ((sfst-mode-syntax-table (make-syntax-table)))
    ; comments are start with %
    (modify-syntax-entry ?%  "<" sfst-mode-syntax-table)
    ; until end-of-line
    (modify-syntax-entry ?\n ">" sfst-mode-syntax-table)
    (modify-syntax-entry ?\\ "\\" sfst-mode-syntax-table)
    (modify-syntax-entry ?\" "." sfst-mode-syntax-table)
;;    (modify-syntax-entry ?\" "." sfst-mode-syntax-table)
    (modify-syntax-entry ?\' "$" sfst-mode-syntax-table)
;;    (modify-syntax-entry ?< "\"" sfst-mode-syntax-table)
    (modify-syntax-entry ?< "." sfst-mode-syntax-table)
    (modify-syntax-entry ?> "." sfst-mode-syntax-table)
    (modify-syntax-entry ?{ "(}" sfst-mode-syntax-table)
    (modify-syntax-entry ?} "){" sfst-mode-syntax-table)
    (modify-syntax-entry ?( "()" sfst-mode-syntax-table)
    (modify-syntax-entry ?) ")(" sfst-mode-syntax-table)
    (modify-syntax-entry ?[ "(]" sfst-mode-syntax-table)
    (modify-syntax-entry ?] ")[" sfst-mode-syntax-table)
    sfst-mode-syntax-table)
  "Syntax table for sfst-mode")


(defconst sfst-font-lock-keywords
  (list
   ;; escaped characters
   (list "\\(\\\\->\\)\\|\\(\\\\.\\)"
         (list 1 'font-lock-function-name-face nil t)
         (list 2 'font-lock-unfontify-region-function nil t))
   ;; keywords
   (list "\\(\\<ALPHABET\\>\\)"
         (list 1 'font-lock-keyword-face nil t))
   ;; include operator
   (list "\\(?:\\(#include\\)[ \t]+\\)?\\(\"<?.+?>?\"\\)"
         (list 1 'font-lock-keyword-face nil t)
         (list 2 'font-lock-constant-face nil t))
   ;; (symbol) variables
   (list "\\(\\([$#]\\)=?\\(?:\\\\\\$\\|\\\\\\#\\|[^ \n\t]\\)+?\\2\\)"
         (list 1 'font-lock-variable-name-face nil t))
   ;; operators (must of high precedence because of <=> is an op not a multi-character symbols)
;;    (list "\\(!\\)"
;;          (list 1 font-lock-negation-char-face nil t))
   (list "\\([_/\\\\^]->\\|[\\^_]?\\(?:<=>?\\|<?=>\\)\\|[:|&!?*+^_]\\|^_\\|||\\|<<\\|__\\)"
         (list 1 'font-lock-function-name-face nil t))
   ;; multi-character symbols
   (list "\\(<.*?>\\)"
         (list 1 'font-lock-type-face nil t))
   )
  "Expressions to highlight in sfst-mode.")

(defun sfst-font ()
  "Set font-lock variables for sfst mode."
  (make-local-variable 'font-lock-keywords-case-fold-search) ; For GNU Emacs.
  (setq font-lock-keywords-case-fold-search nil)
  (put major-mode 'font-lock-keywords-case-fold-search nil) ; For XEmacs.
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(sfst-font-lock-keywords nil nil)))


(defun sfst-mode ()
  "Major mode for editing files describing
finite state transducers to be used with sfst-tools
(Stuttgart Finite State Transducer Tools, see
http://www.ims.uni-stuttgart.de/projekte/gramotron/SOFTWARE/SFST.html)."
  (interactive)
  (kill-all-local-variables)
;;  (use-local-map sfst-mode-map)
  (setq major-mode 'sfst-mode)
  (setq mode-name "SFST")
  (setq parse-sexp-ignore-comments t)
  (set-syntax-table sfst-mode-syntax-table)
;;   (make-local-variable 'indent-line-function)
;;   (setq indent-line-function 'sfst-indent-line)
  (make-local-variable 'comment-start)  
  (setq comment-start "% ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "% *")
  (make-local-variable 'completion-ignore-case)
  (setq completion-ignore-case nil)
  (sfst-font)
  (run-hooks 'sfst-mode-hook))





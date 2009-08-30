;;; xs-mode.el --- A simple major mode for write perl XS code

;; Copyright 2007 Ye Wenbin
;;
;; Author: wenbinye@gmail.com
;; Version: $Id: xs-mode.el,v 0.0 2007/09/05 02:25:20 ywb Exp $
;; Keywords: 
;; X-URL: not distributed yet

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

;;; Commentary:
;; This elisp is for fix font-lock highlighting when use c-mode as
;; major mode in XS files. The indentation of XS code is too difficult
;; for me. I have to give it up.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'xs-mode)
;; Or for autoload:
;;   (autoload 'xs-mode "xs-mode" "Major mode for XS files" t)
;;   (add-to-list 'auto-mode-alist '("\\.xs$" . xs-mode))

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'cc-mode)

(defvar xs-imenu-generic-expression
  '(nil "XS\\s-*(\\s-*\\([[:alnum:]_]+\\)\\s-*)" 1)
  "imenu expression for xsubpp output file.")

(defvar xs-font-lock-syntactic-keywords
  '(("^\\(=\\)\\sw"
     (1 "< b"))
    ("^=cut[ \t]*\\(\n\\)"
     (1 "> b")))
  "Make pod syntax as comments.")

(defvar xs-font-lock-keywords
  (append
   c-font-lock-keywords
   c-font-lock-keywords-1
   c-font-lock-keywords-2
   c-font-lock-keywords-3   
   ;; Reset char ' in syntax table in case it is in pod. I don't know
   ;; why the `xs-font-lock-syntactic-keywords' only take effect in
   ;; the first line, not like `perl-mode'
   '(("'[\\]?.'" . font-lock-string-face)))
  "Extra Keyword for `xs-mode'.")

(define-derived-mode xs-mode c-mode "XS"
  "Major mode to edit xs code."
  (add-to-list 'imenu-generic-expression  xs-imenu-generic-expression)
  (setq font-lock-defaults
        '((c-font-lock-keywords
           c-font-lock-keywords-1 c-font-lock-keywords-2
           c-font-lock-keywords-3 xs-font-lock-keywords)
          nil nil 
          ((?\_ . "w")
           (?\# . "w")
           (?\' . "."))
          c-beginning-of-syntax
          (font-lock-syntactic-keywords
           . xs-font-lock-syntactic-keywords))))

(add-to-list 'magic-mode-alist
             (cons (regexp-quote "#include \"perl.h\"") 'xs-mode))
(add-to-list 'auto-mode-alist '("\\.xs$" . xs-mode))

(provide 'xs-mode)
;;; xs-mode.el ends here

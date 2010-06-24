;;; fixme-mode.el --- Makes FIXME, TODO, etc. appear in big, angry letters
;; Filename: fixme-mode.el
;; Description: Makes source code warnings (FIXME, TODO, etc.) stand out
;; in a big way.
;; Author: Bryan Waite, based on some code found at 
;; http://c2.com/cgi/wiki?FixmeComment
;; Copyright (C) 2009-2010, Bryan Waite 
;; License: MIT License (not reproduced for space reasons)
;; Compatibility: Only tested under Emacs 23.1 on Ubuntu Linux.
;; Usage: Just add (require 'fixme-mode) to your .emacs file
;; and then type M-x fixme-mode to try it out.

(require 'cl)

(defconst fixme-mode-version 0.2)

(defgroup fixme-mode nil
  "Highlights FIXME, TODO, and other warnings in source code"
  :prefix "fixme-"
  :link   '(url-link "http://www.thesiteiwillonedayhave.com"))

(defcustom fixme-modes '(erlang-mode java-mode c-mode emacs-lisp-mode jde-mode
                                  scheme-mode python-mode ruby-mode cperl-mode
                                  slime-mode common-lisp-mode c++-mode d-mode
                                  js2-mode haskell-mode tuareg-mode lua-mode
                                  pascal-mode fortran-mode prolog-mode asm-mode
                                  csharp-mode sml-mode)
  "The modes which fixme should apply to"
  :group 'fixme-mode)

(defcustom fixme-highlighted-words '("FIXME" "TODO" "BUG" "KLUDGE")
  "Words to highlight"
  :group 'fixme-mode)

(defcustom fixme-foreground-color "Red"
  "Font foreground colour"
  :group 'fixme-mode)

(defcustom fixme-background-color  "Yellow"
  "Font background color"
  :group 'fixme-mode)

(defvar fixme-keyword-re-string "" 
  "The regular expression to use for searching for fixme words. Generated with fixme-register-keyword-re")

(defvar fixme-keyword-font-lock '()
  "Font lock keywords. Generated from fixme-register-font-lock-keywords")

(make-face 'font-lock-fixme-face)

(defun fixme-next ()
  "Goto the next fixme highlighted word"
  (interactive)
  (lexical-let ((old-case case-fold-search))
    (setq case-fold-search nil)
    (search-forward-regexp fixme-keyword-re-string)
    (setq case-fold-search old-case)))

(defun fixme-prev ()
  "Goto the previous fixme highlighted word"
  (interactive)
  (lexical-let ((old-case case-fold-search))
     (setq case-fold-search nil)
     (search-backward-regexp fixme-keyword-re-string)
     (setq case-fold-search old-case)))

(defun fixme-show-all-fixmes ()
  "Show all fixme strings in the current file"
  (interactive)
  (let ((buf (buffer-file-name)))
    (when buf
      ;;(grep (concat "grep -nH -e " (concat "\"" fixme-keyword-re-string "\" " buf)))
      (occur fixme-keyword-re-string)
)))

(defun fixme-register-keyword-re ()
  "Generate the regular expression string from fixme-highlighted-words
and store the result in fixme-keyword-re-string"
  (lexical-let ((num-words (length fixme-highlighted-words))
                (word-count 0))
    (setq fixme-keyword-re-string "")
    (dolist (word fixme-highlighted-words)
      (incf word-count)
      (setq fixme-keyword-re-string (concat fixme-keyword-re-string word))
      (when (< word-count num-words) ;;only add the OR in if we're not at the end
        (setq fixme-keyword-re-string (concat fixme-keyword-re-string "\\|"))))))

(defun fixme-register-font-lock-keywords ()
  "Generate the font-lock keywords from fixme-highlighted-words
and store the result in fixme-keyword-font-lock"
  (lexical-let ((stuff '()))
    (dolist (word fixme-highlighted-words)
      (setq stuff (append stuff `((,(concat "\\<\\(" word "\\)") 1 'font-lock-fixme-face t)))))
    (setq fixme-keyword-font-lock stuff)))

(defun fixme-register-keywords ()
  "Register the font-lock keywords from fixme-keyword-font-lock with the modes
listed in fixme-modes"
  (mapc (lambda (mode)
          (font-lock-add-keywords
           mode
           fixme-keyword-font-lock))
        fixme-modes)
  (make-face 'font-lock-fixme-face)
  (modify-face 'font-lock-fixme-face fixme-foreground-color
                fixme-background-color nil t nil t nil nil))

(defun fixme-remove-keywords ()
  "Remove the font-lock keywords from fixme-keyword-font-lock with the modes
listed in fixme-modes"
  (mapc (lambda (mode)
          (font-lock-remove-keywords
           mode
           fixme-keyword-font-lock))
        fixme-modes))

(defun fixme-reload-keywords ()
  "Run this if you change the fixme-modes or fixme-highlighted-words variables
to update the font-lock and searching variables"
  (interactive)
  (fixme-register-keyword-re)
  (fixme-register-font-lock-keywords)
  (fixme-register-keywords)
  )

;;;###autoload(defvar fixme-mode nil)
(define-minor-mode fixme-mode  
  "A minor mode for making FIXME and other warnings stand out"
  nil
  " Fixme"
  nil
  :global
  :group fixme-mode
  :version fixme-mode-version
  (if fixme-mode
      (fixme-reload-keywords)
  (fixme-remove-keywords))
)

(provide 'fixme-mode)

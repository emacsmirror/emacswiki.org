;;; .emacs --- Minimal Emacs initialization file

;; Copyright (C) 2008, 2009 Vinicius Jose Latorre

;; Author: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Maintainer: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Time-stamp: <2009/08/08 09:41:44 vinicius>
;; Keywords: environment, initialization
;; Version: 1.1
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/ViniciusJoseLatorre


;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete]    'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; Turn on font-lock mode for Emacs
(global-font-lock-mode t)

;; Visual feedback on selections
(setq-default transient-mark-mode t)

;; Always end a file with a newline
(setq require-final-newline t)

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;; Enable wheelmouse support by default
(when window-system
  (mwheel-install))

;; don't make backup files, please
(setq make-backup-files nil)


;; my settings ;-)

(column-number-mode 1)			; turn on column number

(setq inhibit-startup-message t)	; don't display splash screen
(setq visible-bell t)			; turn on visible bell
(setq dired-listing-switches "-laoFv")	; dired-mode switches

(global-set-key [f9]    'goto-line)	; F9
(global-set-key "\C-cg" 'goto-line)

;; colors for all frames but the initial one
(setq default-frame-alist
      (append '((background-color . "gray14")
		(foreground-color . "white")
		(pointer-color    . "firebrick")
		(cursor-color     . "Yellow"))
	      default-frame-alist))

;; colors only for the initial frame
(setq initial-frame-alist
      (append '((background-color . "gray14")
		(foreground-color . "white")
		(pointer-color    . "firebrick")
		(cursor-color     . "Yellow"))
	      initial-frame-alist))

;; C++ setting
(add-hook 'c++-mode-hook
	  '(lambda ()
	     ;; set Kernighan & Ritchie style
	     (c-set-style           "k&r")
	     ;; 3 for basic indentation
	     (setq c-basic-offset   3)
	     ;; use spaces instead of tabs
	     (setq indent-tabs-mode nil)))

;;; .emacs ends here

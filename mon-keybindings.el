;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; This is mon-keybindings.el 
;;; ================================================================
;;; DESCRIPTION:
;;; This file defines standard keybindings used across MON site systems.
;;;
;;; TODO:
;;; Need a keybinding for minibuffer completion in fundamental-mode etc.
;;; on <S-iso-lefttab> <backtab> w/ 'hippie-expand
;;; e.g. (add-hook 'hippie-expand
;;; (minibuffer-complete)
;;; Right now there is one for M-/ but I would like others as well.
;;; Esp. for `eval-expression' minibuffer where I need standard elisp completion.
;;; See; `minibuffer-complete' `minibuffer-completion-help',
;;; `minibuffer-completing-symbol' `lisp-complete-symbol'
;;;
;;; NOTES:
;;; --- IF/WHEN YOU FUCKUP:
;;; i.e. Forget to escape "C-c\M-" (yet again) you wont' be able to
;;; type "C" in a buffer using the modes keymap. 
;;; reset that with (define-key emacs-lisp-mode-map "C" nil) 
;;;                              ^- change mode-map accordingly
;;; 
;;; SNIPPETS:
;;; --- W32 SPECIFIC:
;;; w32-lwindow-modifier
;;; w32-rwindow-modifier
;;; w32-pass-alt-to-system
;;; w32-pass-lwindow-to-system 
;;; w32-pass-rwindow-to-system
;;; w32-pass-multimedia-buttons-to-system 
;;; "To bind the key s-TAB, use [?\\s-\\t], not [s-TAB]")
;;;
;;; --- KEYS THAT ARE EASY TO _NOT_ FIND:
;;; <rwindow>
;;; <lwindow>
;;; <C-backspace>
;;; <S-backspace>
;;; <tab> <backtab> <S-iso-lefttab> 
;;; 
;;; --- FUNCTIONS THAT OPERATE OND KEYS/KEYMAPS/BINDINGS:
;;; (info "(elisp)Keymaps")
;;;
;;; `listify-key-sequence'
;;; `lookup-key' (lookup-key keymap key &optional accept-default)
;;; `substitute-key-definition' (olddef newdef keymap &optional oldmap prefix)
;;; `read-key-sequence' (`kbd' "<some-key>")
;;; `define-key', `make-sparse-keymap' 
;;; `substitute-key-definition', `suppress-keymap', `command-remapping'
;;; `local-unset-key', `local-set-key', 
;;; `global-set-key', `global-unset-key'
;;; `function-key-map' (var) 
;;;
;;; --- KEYBINDING HOOK IDIOM:
;;; (add-hook 'some-hook-to-add|(remove-hook 'some-hook-to-remove
;;;              ;(function  ;note using function/#'here makes it possible to remove the
;;;                          ;lambda (annonymous) function.
;;;               ;(lambda () (define-key 
;;;                            ;some-mode-map 
;;;                            ;(kbd "<KEY>")|[vector]|"\KEY-key*\KEY-key*\\")
;;;                            ;'function-name)
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/mon-keybindings.el')
;;; FIRST-PUBLISHED:
;;;
;;; FILE-CREATED: Autumn 2008
;;; HEADER-ADDED: <Timestamp: #{2009-09-14T14:59:44-04:00Z}#{09381} - by MON KEY>
;;; ================================================================
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;;; Floor, Boston, MA 02110-1301, USA.
;;; ================================================================
;;; Copyright (C) 2009 MON KEY 
;;; ==========================

;;; ==============================
;;; !NOT C-c bound
(global-set-key "\M-n" 'scroll-up-in-place)
(global-set-key "\M-p" 'scroll-down-in-place)
(global-set-key (kbd "<C-backspace>") 'backward-kill-word)
(global-set-key (kbd "<S-backspace>") (kbd "DEL"))

;;; ==============================
;;; Removed this binding to accomodate `dired-efap'.
;;; MODIFICATIONS: <Timestamp: #{2009-09-07T14:45:06-04:00Z}#{09371} - by MON KEY>
;;;(global-set-key [(f2)] 'doremi-window-width)

(when win32p (global-set-key [(f3)] 'w32-maximize-frame))

;;; ==============================
;;; NOTE: Keyindings marked ;!! are bound in naf-mode.el but use a different global-key.
;;; C-c BOUND KEYS:
;;; ==============================
(global-set-key "\C-c!"       'shell-command)  ; "M-!" doesn't like windows :(
;; (global-set-key "\C-cc"     'comment-region) moving this into emacs-lisp binding only
(global-set-key "\C-cwou"     'mon-wrap-one-url)
(global-set-key "\C-cflr"     'fill-region)
(global-set-key "\C-cvm"      'view-mode)                        
;;
;; C-c C-*
(global-set-key "\C-c\C-cp"   'mon-put-copyright)               ;!! "\C-c\M-pc" 
(global-set-key "\C-c\C-di"   'comment-divider)                  ;!! "\C-c\M-di"
(global-set-key "\C-c\C-dn"   'comment-divider-to-col-four)      ;!! "\C-c\M-dn" 
;; (global-set-key "\C-c\C-in" 'mon-incr)                       ;!! "\C-c\M-in" 
(global-set-key "\C-x\C-k1"   'split-designator)                 ;!! "\C-C\M-k1"
(global-set-key "\C-c\C-kc"   'mon-kill-completions)
(global-set-key "\C-c\C-gg"   'google-define)                    ;!! "\C-c\M-gg" 
(global-set-key "\C-c\C-na"   'naf-drive-dired-artist-letter)    ;!! "\C-c\M-na" - w32 only. path is MON local. 
(global-set-key "\C-c\C-nb"   'naf-drive-dired-brand-letter)     ;!! "\C-c\M-na" - w32 only. path is MON local
;; (global-set-key "\C-x\C-u" 'describe-char) ; when it gets rebound
;;
;; C-c M-*
(global-set-key "\C-c\M-/"    'hippie-expand)
(global-set-key "\C-c\M-ab"   'mon-append-to-buffer)
(global-set-key "\C-c\M-af"   'append-to-file)
(global-set-key "\C-c\M-ar"   'mon-append-to-register)
(global-set-key "\C-c\M-f"    'mon-flip-windows) 
(global-set-key "\C-c\M-r"    'capitalize-region)                ;!! "\C-c\M-cr"
(global-set-key "\C-c\M-php"  'mon-insert-php-comment-divider)
(global-set-key "\C-c\M-wd"   'wdired-change-to-wdired-mode)

;; CREATED: <Timestamp: #{2009-08-24T16:33:22-04:00Z}#{09351} - by MON KEY>
(global-set-key "\C-cfst" 'mon-file-stamp) 

;;; ==============================

;;; ==============================
;;; ADDED: `dired-efap' keybinding.
;;; MODIFICATIONS: <Timestamp: #{2009-09-07T14:48:26-04:00Z}#{09371} - by MON KEY>
(add-hook 'dired-mode-hook 
          (function 
           (lambda () 
             (define-key dired-mode-map "\C-c\M-dwt" 'mon-toggle-dired-dwim-target)
             (define-key dired-mode-map "\M-^" 'dired-up-directory-this-buffer)
	     (define-key dired-mode-map [M-f2] 'dired-efap))))

(add-hook 'Info-mode-hook  
	  (function 
           (lambda () 
             (define-key Info-mode-map "\M-n" 'scroll-up-in-place))))

(add-hook 'ido-minibuffer-setup-hook
          (function 
           (lambda ()
             (define-key ido-completion-map (kbd "<backtab>") 'ido-complete))))

(cond (IS-MON-P 
       (when (boundp 'Tex-mode-map)  
         (define-key TeX-mode-map (kbd "<S-iso-lefttab>")  
           'TeX-complete-symbol))))

;;; ==============================
;;; emacs-lisp-mode-map 
;;; (global-set-key "\C-c\M-;"   'mon-user-evald)
;;; ==============================

;;; ==============================
(when IS-MON-P 
  (add-hook 'emacs-lisp-mode-hook  
            (function 
             (lambda () 
               (progn
                 (define-key emacs-lisp-mode-map "\C-c\M-:"    'mon-eval-expression)
                 (define-key emacs-lisp-mode-map "\C-cc"       'comment-region)
                 (define-key emacs-lisp-mode-map "\C-c\C-uc"   'uncomment-region)
                 (define-key emacs-lisp-mode-map "\C-c\C-di"   'comment-divider)
                 (define-key emacs-lisp-mode-map "\C-c\C-dn"   'comment-divider-to-col-four)
                 (define-key emacs-lisp-mode-map "\C-cst"      'mon-insert-lisp-stamp)
                 (define-key emacs-lisp-mode-map "\C-ctm"      'mon-insert-lisp-testme)
                 ;;!NOTE: C-c C-b bound to slime-interupt in Slime-mode
                 (define-key emacs-lisp-mode-map "\C-c\C-b"    'mon-eval-sexp-at-point)
                 (define-key emacs-lisp-mode-map "\C-c\C-j"    'mon-eval-print-last-sexp)
                 (define-key emacs-lisp-mode-map "\C-c\C-cb"   'mon-princ-cb)
                 (define-key emacs-lisp-mode-map "\C-cel"      'mon-escape-lisp-string-region)
                 (define-key emacs-lisp-mode-map "\C-cul"      'mon-unescape-lisp-string-region)
		 (define-key emacs-lisp-mode-map "\C-clsl"     'mon-line-strings-to-list)
                 (define-key emacs-lisp-mode-map "\C-c\t"      'lisp-complete-symbol)
                 (cond (IS-MON-P-GNU  (define-key emacs-lisp-mode-map (kbd "<S-iso-lefttab>")  'lisp-complete-symbol))
                       (IS-MON-P-W32 (define-key emacs-lisp-mode-map (kbd "<backtab>")  'lisp-complete-symbol))))))))

;;; ==============================
;;; lisp-interaction-mode-map:
;;; ==============================
(when IS-MON-P
  (add-hook 'lisp-interaction-mode-hook 
            (function	
             (lambda () 
               (progn
                 (local-unset-key "\C-j")
                 (define-key lisp-interaction-mode-map "\C-j" 'newline-and-indent)
                 (define-key lisp-interaction-mode-map "\C-c\C-j" 'mon-eval-print-last-sexp)
                 (cond (IS-MON-P-GNU
                        (define-key lisp-interaction-mode-map (kbd "<S-iso-lefttab>")  'lisp-complete-symbol))
                       (IS-MON-P-W32
                        (define-key lisp-interaction-mode-map  (kbd "<backtab>") 'lisp-complete-symbol))))))))

;;; ==============================
(add-hook 'message-mode-hook
          (function 
	   (lambda ()
	     (progn
	       (local-unset-key "\C-c\C-d")
	       (define-key message-mode-map "\C-c\C-dd" 'message-dont-send)))))

;;; ==============================
;;; hg's revert isn't usually what is wanted and ?U is to close to ?u which
;;; normally unmarks in dired... This is/was a HORRIBLE design decision in DVC.
;;; Lets fix it.  
;;
;;; (add-hook 'dvc-diff-mode-hook #'(lambda () (local-unset-key [?U]))) ;; WAS: dvc-revert-files
;;
;;; (add-hook 'xhg-log-mode-hook  #'(lambda () (local-unset-key [?R]))) ;; WAS: xhg-rollback

;;; ==============================
(provide 'mon-keybindings)
;;; ==============================

;;; ================================================================
;;; mon-keybindings.el ends here
;;; EOF

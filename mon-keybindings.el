;;; mon-keybindings.el --- MON keybindings for fncns used across sites
;; -*- mode: EMACS-LISP; no-byte-compile: t -*-

;;; ================================================================
;; Copyright © 2008-2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-keybindings.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2008-09
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: local, keybouard, environment, convenience, emacs, installation

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; This file defines standard keybindings used across MON site systems.
;;
;; TODO:
;; Need a keybinding for minibuffer completion in fundamental-mode etc.
;; on <S-iso-lefttab> <backtab> w/ 'hippie-expand
;; e.g. (add-hook 'hippie-expand (minibuffer-complete))
;; 
;;
;; Right now there is one for M-/ but I would like others as well.
;; Esp. for `eval-expression' minibuffer where I need standard elisp completion.
;; :SEE `minibuffer-complete' `minibuffer-completion-help',
;; `minibuffer-completing-symbol' `lisp-complete-symbol'
;;
;; NOTES:
;; :IF/WHEN-YOU-FUCKUP
;; i.e. Forget to escape "C-c\M-" (yet again) you wont' be able to
;; type "C" in a buffer using the modes keymap. 
;; reset that with (define-key emacs-lisp-mode-map "C" nil) 
;;                              ^- change mode-map accordingly
;; 
;; SNIPPETS:
;; :INTERRUPT-FLOW-META-QUIT
;; (current-input-mode) => (nil nil t 7)
;; (set-input-mode)
;; (recent-keys)
;; (open-termscript  "<FILE>")
;; (open-dribble-file "<FILE>")
;; (open-dribble-file nil) 
;; single-key-description
;; kbd
;; event-modifiers
;; system-key-alist
;; input-decode-map
;; function-key-map
;; local-function-key-map
;; keymap                       -> 24  -> "\x18"
;; keymap                       -> 64  -> "@"
;; event-apply-control-modifier -> 99  -> "c"
;; event-apply-shift-modifier   -> 83  -> "S"
;; event-apply-alt-modifier     -> 97  -> "a"
;; event-apply-meta-modifier    -> 109 -> "m"
;; event-apply-hyper-modifier   -> 115 -> "s"
;; (24 keymap (64 keymap 
;;                (99 . event-apply-control-modifier)
;;                (83 . event-apply-shift-modifier)
;;                (97 . event-apply-alt-modifier)
;;                (109 . event-apply-hyper-modifier)
;;                (115 . event-apply-super-modifier)
;;                (104 . event-apply-hyper-modifier)))
;; 
;; x-alt-keysym
;; x-meta-keysym
;; x-hyper-keysym
;; x-super-keysym
;;
;; :W32-SPECIFIC
;; w32-phantom-key-code -> 255
;; w32-lwindow-modifier
;; w32-rwindow-modifier
;; w32-pass-alt-to-system
;; w32-pass-lwindow-to-system 
;; w32-pass-rwindow-to-system
;; w32-pass-multimedia-buttons-to-system 
;; "To bind the key s-TAB, use [?\\s-\\t], not [s-TAB]")
;;
;; :KEYS-THAT-ARE-EASY-TO-_NOT_-FIND
;; <rwindow>
;; <lwindow>
;; <C-backspace>
;; <S-backspace>
;; <tab> <backtab> <S-iso-lefttab> 
;; 
;; :KEYS/KEYMAPS/KEY-BINDING-FUNCTIONS
;; :SEE (info "(elisp)Keymaps")
;;
;; `listify-key-sequence'
;; `lookup-key' (lookup-key keymap key &optional accept-default)
;; `substitute-key-definition' (olddef newdef keymap &optional oldmap prefix)
;; `read-key-sequence' (`kbd' "<some-key>")
;; `define-key', `make-sparse-keymap' 
;; `substitute-key-definition', `suppress-keymap', `command-remapping'
;; `local-unset-key', `local-set-key', 
;; `global-set-key', `global-unset-key'
;; `function-key-map' (var) 
;;
;; :KEYBINDING-HOOK-IDIOM
;; ([add-hook|remove-hook] 
;;  'some-hook-to-add/remove
;;  (function   ;; :NOTE Using `function' or "#'" (sharpquote) allows later
;;   (lambda () ;; removal of anonymous lambda forms.
;;    (define-key 'some-mode-map 
;;        [kbd "<KEY>")|[vector-of-key-chars]|"\KEY-key*\KEY-key*\\"]
;;    'some-function-name)))
;;
;; URL: http://www.emacswiki.org/emacs/mon-keybindings.el
;; FIRST-PUBLISHED:
;;
;; HEADER-ADDED: <Timestamp: #{2009-09-14T14:59:44-04:00Z}#{09381} - by MON KEY>
;; FILE-CREATED: 2008-09
;;
;; =================================================================

;;; LICENSE:

;; =================================================================
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; =================================================================
;; Permission is granted to copy, distribute and/or modify this
;; document under the terms of the GNU Free Documentation License,
;; Version 1.3 or any later version published by the Free Software
;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;; and no Back-Cover Texts. A copy of the license is included in
;; the section entitled ``GNU Free Documentation License''.
;; 
;; A copy of the license is also available from the Free Software
;; Foundation Web site at:
;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ==============================
;; Copyright © 2008-2010 MON KEY 
;;; ==============================

;;; CODE:

(eval-when-compile (require 'cl))

;;; ==============================
;; :NOT-BOUND-W-C-c
(global-set-key "\M-n" 'mon-scroll-up-in-place)
(global-set-key "\M-p" 'mon-scroll-down-in-place)
(global-set-key (kbd "<C-backspace>") 'backward-kill-word)
(global-set-key (kbd "<S-backspace>") (kbd "DEL"))
(global-set-key "\C-c\C-xd" 'mon-dired-other-window)
;;
(when IS-W32-P 
  (global-set-key [(f3)] 'w32-maximize-frame))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2009-09-07T14:45:06-04:00Z}#{09371} - by MON
;;; :REMOVED (global-set-key [(f2)] 'doremi-window-width) 
;;; To accomodate `dired-efap'.

;;; ==============================
;; :BOUND-W-C-c
;;; :NOTE Keyindings marked ;!! are bound in naf-mode.el but use a different global-key.

(global-set-key "\C-c!"       'shell-command)  ;; "M-!" doesn't like it when IS-W32-P :(
(global-set-key "\C-cwou"     'mon-wrap-one-url)
(global-set-key "\C-cflr"     'fill-region)
(global-set-key "\C-cvm"      'view-mode)                        
(global-set-key "\C-cu:"      'mon-cln-up-colon)
;;
;; C-c C-*
(global-set-key "\C-c\C-cp"   'mon-put-copyright)                ;!! "\C-c\M-pc" 
(global-set-key "\C-c\C-di"   'comment-divider)                  ;!! "\C-c\M-di"
(global-set-key "\C-c\C-dn"   'mon-comment-divider-to-col-four)      ;!! "\C-c\M-dn" 
;; (global-set-key "\C-c\C-in" 'mon-incr)                        ;!! "\C-c\M-in" 
(global-set-key "\C-x\C-k1"   'mon-split-designator)             ;!! "\C-C\M-k1"
(global-set-key "\C-c\C-kc"   'mon-kill-completions)
(global-set-key "\C-c\C-gg"   'google-define)                    ;!! "\C-c\M-gg" 
(global-set-key "\C-c\C-na"   'naf-drive-dired-artist-letter)    ;!! "\C-c\M-na" :NOTE IS-W32-P only. Path is MON local. 
(global-set-key "\C-c\C-nb"   'naf-drive-dired-brand-letter)     ;!! "\C-c\M-nb" :NOTE IS-W32-P only. Path is MON local.
;; (global-set-key "\C-x\C-u" 'describe-char) ;; When it gets rebound.
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

;; :CREATED <Timestamp: #{2009-08-24T16:33:22-04:00Z}#{09351} - by MON>
(global-set-key "\C-cfst" 'mon-file-stamp) 

;;; ==============================
;;; :ADDED `dired-efap' keybinding.
;;; :MODIFICATIONS <Timestamp: #{2009-09-07T14:48:26-04:00Z}#{09371} - by MON>
;;; :ADDED `mon-w3m-dired-file' 
;;; :CHANGESET 1338 <Timestamp: #{2009-12-17T13:21:29-05:00Z}#{09514} - by MON>
(add-hook 'dired-mode-hook 
         (function 
          (lambda () 
              (define-key dired-mode-map "\C-c\M-dwt" 'mon-toggle-dired-dwim-target)
              (define-key dired-mode-map "\M-^"       'dired-up-directory-this-buffer)
              (define-key dired-mode-map [M-f2]       'dired-efap)
              (define-key dired-mode-map "\C-c\M-wl"  'mon-copy-file-dired-as-list)   ;; Args: AS-LIST FULL-PATH
              (define-key dired-mode-map "\C-c\M-ws"  'mon-copy-file-dired-as-string) ;; Args: UNQOUTED FULL-PATH
              (define-key dired-mode-map "\C-cw3"     'mon-w3m-dired-file))))

;;; ==============================
;; :COMPLETION-KEYBINDINGS
;;; :TODO Add bindings for `f' to select-next `b' select-previous
;;;
;;; :NOTE Make `n' and `p' move up and down in *Completions* buffer.
;;;       Make SPC scroll the page.
;;;
;;; :CREATED <Timestamp: #{2009-12-18T21:48:03-05:00Z}#{09515} - by MON KEY>
(add-hook 'completion-list-mode-hook
          (function 
           (lambda ()
            (define-key completion-list-mode-map "n" #'(lambda () (interactive) (line-move 1 t)))
            (define-key completion-list-mode-map "p"  #'(lambda () (interactive) (line-move -1 t)))
            (define-key completion-list-mode-map (kbd "SPC")  'scroll-up))))

(add-hook 'Info-mode-hook  
	  (function 
           (lambda () 
             (define-key Info-mode-map "\M-n" 'mon-scroll-up-in-place))))

(add-hook 'ido-minibuffer-setup-hook
          (function 
           (lambda ()
             (define-key ido-completion-map (kbd "<backtab>") 'ido-complete))))

(cond (IS-MON-P 
       (when (boundp 'Tex-mode-map)  
         (define-key TeX-mode-map (kbd "<S-iso-lefttab>")  
           'TeX-complete-symbol))))

;;; ==============================
;; :EMACS-LISP-MODE-KEYMAP
;;; (symbol-value 'emacs-lisp-mode-map)
;;; (global-set-key "\C-c\M-;"   'mon-user-evald)

(when IS-MON-SYSTEM-P
  (add-hook 'emacs-lisp-mode-hook  
            (function 
             (lambda () 
               (progn
                 (define-key emacs-lisp-mode-map "\M-i"        'indent-according-to-mode)
                 (define-key emacs-lisp-mode-map "\C-c\M-:"    'mon-eval-expression)
                 (define-key emacs-lisp-mode-map "\C-cc"       'comment-region)
                 (define-key emacs-lisp-mode-map "\C-c\C-uc"   'uncomment-region)
                 (define-key emacs-lisp-mode-map "\C-c\C-di"   'comment-divider)
                 (define-key emacs-lisp-mode-map "\C-c\C-dn"   'mon-comment-divider-to-col-four)
                 (define-key emacs-lisp-mode-map "\C-cst"      'mon-insert-lisp-stamp)
                 (define-key emacs-lisp-mode-map "\C-ctm"      'mon-insert-lisp-testme)
                 ;; :NOTE  C-c C-b bound to `slime-interrupt' in Slime-mode!!
                 (define-key emacs-lisp-mode-map "\C-c\C-b"    'mon-eval-sexp-at-point)
                 (define-key emacs-lisp-mode-map "\C-c\C-j"    'mon-eval-print-last-sexp)
                 (define-key emacs-lisp-mode-map "\C-c\C-cb"   'mon-princ-cb)
                 (define-key emacs-lisp-mode-map "\C-cal"      'align-let) ;; :WAS [?\C-c ?\C-a] in align-let.el
                 (define-key emacs-lisp-mode-map "\C-cel"      'mon-escape-lisp-string-region)
                 (define-key emacs-lisp-mode-map "\C-cexs"     'mon-insert-lisp-doc-eg-xref)
                 (define-key emacs-lisp-mode-map "\C-cul"      'mon-unescape-lisp-string-region)
		 (define-key emacs-lisp-mode-map "\C-clsl"     'mon-line-strings-to-list)
                 (define-key emacs-lisp-mode-map "\C-c\t"      'lisp-complete-symbol)
                 (cond (IS-MON-P-GNU  (define-key emacs-lisp-mode-map (kbd "<S-iso-lefttab>")  'lisp-complete-symbol))
                       (IS-MON-P-W32  (define-key emacs-lisp-mode-map (kbd "<backtab>")  'lisp-complete-symbol))))))))

;;; ==============================
;; :LISP-INTERACTION-MODE-KEYMAP
;;; (symbol-value 'lisp-interaction-mode-map
(when IS-MON-P
  (add-hook 'lisp-interaction-mode-hook 
            (function (lambda () 
              (progn
                (local-unset-key "\C-j")
                (define-key lisp-interaction-mode-map "\C-j" 'newline-and-indent)
                (define-key lisp-interaction-mode-map "\C-c\C-j" 'mon-eval-print-last-sexp)
                (cond (IS-MON-P-GNU
                       (define-key lisp-interaction-mode-map (kbd "<S-iso-lefttab>")  'lisp-complete-symbol))
                      (IS-MON-P-W32
                       (define-key lisp-interaction-mode-map  (kbd "<backtab>") 'lisp-complete-symbol)
                       (define-key lisp-interaction-mode-map  (kbd "<backtab>") 'lisp-complete-symbol)
                       )))))))

;;; ==============================
;; :SLIME-KEYBINDINGS
;;; `slime-prefix-bindings'
;;; `slime-editing-keys'
;;; `slime-keys'
;;; `slime-doc-bindings'
;;; `slime-who-bindings'
;;; :SEE-ALSO `mon-help-slime-keys', `slime-cheat-sheet'
;;;
;; :SLIME-KEYMAPS
;;; `slime-parent-map'
;;; `slime-parent-bindings'
;;; `slime-prefix-map'
;;; `slime-who-map'
;;; ==============================
(add-hook 'slime-mode-hook
          (function (lambda ()
            (when IS-MON-P 
              (local-unset-key "\C-c\C-c")
              (define-key slime-mode-map "\C-cc"    'comment-region)
              (define-key slime-mode-map "\C-c\C-o"  'slime-compile-defun)
              ;;
              (local-unset-key "\M-n")
              (define-key slime-mode-map "\M-n" 'mon-scroll-up-in-place)
              (define-key slime-mode-map "\C-c\M-n" 'slime-next-note)
              ;;
              (local-unset-key "\M-p")
              (define-key slime-mode-map "\M-p" 'mon-scroll-down-in-place)
              (define-key slime-mode-map "\C-c\M-p" 'slime-previous-note)
              ;;
              (define-key slime-mode-map "\C-ctm" 'mon-insert-lisp-testme)
              (define-key slime-mode-map "\C-cst" 'mon-insert-lisp-stamp)
              (define-key slime-mode-map "\C-c\C-j"  'slime-eval-print-last-expression))
            (when IS-MON-P-GNU
              (define-key slime-mode-map (kbd "<S-iso-lefttab>") 'slime-complete-symbol)
              (define-key slime-repl-mode-map (kbd "<S-iso-lefttab>") 'slime-complete-symbol)
              (define-key slime-mode-map  (kbd "<backtab>") 'slime-complete-symbol))
            (when IS-MON-P-W32
              (define-key slime-mode-map  (kbd "<backtab>") 'slime-complete-symbol)
              (define-key slime-mode-map  (kbd "<S-tab>")  'slime-complete-symbol)
              ;; :NOTE I don't think this is getting used. Can it hurt?
              (define-key slime-mode-map (kbd "<S-iso-lefttab>") 'slime-complete-symbol))
            )))

;;; ==============================
;; :MESSAGE-MODE-KEYBINDINGS
(add-hook 'message-mode-hook
          (function 
	   (lambda ()
	     (progn
	       (local-unset-key "\C-c\C-d")
	       (define-key message-mode-map "\C-c\C-dd" 'message-dont-send)))))

;;; ==============================
;; :EMACS-SERVER-KEYBINDINGS
;;; :CREATED <Timestamp: #{2010-01-27T22:48:03-05:00Z}#{10043} - by MON KEY>
(when IS-MON-P-GNU
  (add-hook 'server-switch-hook  
            (function 
             (lambda () (local-set-key (kbd "C-x #") 'server-edit)))))

;;; ==============================
;; :DVC-KEYBINDINGS
;;;
;;; :NOTE hg's revert isn't usually what is wanted and ?U is to close to ?u
;;;       which normally unmarks in dired... This is/was a _HORRIBLE_ design
;;;       decision in DVC.  Lets fix it.
;;
;;; (add-hook 'dvc-diff-mode-hook #'(lambda () (local-unset-key [?U]))) ;; :WAS dvc-revert-files
;;
;;; (add-hook 'xhg-log-mode-hook  #'(lambda () (local-unset-key [?R]))) ;; :WAS xhg-rollback
;;; (add-hook 'dvc-diff-mode-hook (function (lambda () )

(add-hook 'dvc-status-mode-hook 
          (function 
           (lambda () 
            (define-key dvc-status-mode-map dvc-keyvec-revert nil))))

;;; :VARIABLE `dvc-keyvec-revert' <- (char-to-string 85) -> U 
;;;
;;; Whomever, chose to hide _anything_ that reverts or is related to reverting
;;; in a dedicated symbol vector buried in DVC's package spaghetti can FUCK OFF!
;;; How is this variable transparent to the user??? 
;;; Find a better way to make your code portable across DVC's... and document it!
;;;
;;; :SEE-ALSO `dvc-fileinfo-revert-files' `dvc-status-mode-map'

;;; ==============================
(provide 'mon-keybindings)
;;; ==============================

;;; ================================================================
;;; mon-keybindings.el ends here
;;; EOF

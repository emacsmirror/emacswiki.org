;;; mon-keybindings.el --- MON keybindings for fncns used across sites
;; -*- mode: EMACS-LISP; no-byte-compile: t -*-

;;; ================================================================
;; Copyright © 2008-2011 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-keybindings.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2008-09
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: local, keyboard, environment, installation, convenience, emacs, 

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; This file defines standard keybindings used across MON site systems.
;;
;; FUNCTIONS:►►►
;; `mon-keybind-globally', `mon-keybind-w3m', `mon-keybind-emacs-lisp-mode',
;; `mon-keybind-slime', `mon-keybind-lisp-interaction-mode',
;; `mon-keybind-dired-mode', `mon-keybind-conf-mode', 
;; 
;; FUNCTIONS:◄◄◄
;; 
;; VARIABLES:
;; `*mon-keybindings-xrefs*'
;;
;; GROUPS:
;; `mon-keybindings'
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
;; For a list of common Emacs keybingings :SEE :FILE lisp/bindings.el
;;
;; SNIPPETS:
;;
;; substitute-key-definition <OLDDEF> <NEWDEF> <KEYMAP>
;; (current-input-mode) => (nil nil t 7)
;; (set-input-mode)
;; (recent-keys)
;; (open-termscript  "<FILE>")
;; (open-dribble-file "<FILE>")
;; (open-dribble-file nil) 
;; where-is
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
;;                (99  . event-apply-control-modifier)
;;                (83  . event-apply-shift-modifier)
;;                (97  . event-apply-alt-modifier)
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
;; <TAB> <RET> <BS> <LFD> <ESC> and <DEL>
;; <rwindow>
;; <lwindow>
;; <C-backspace>
;; <S-backspace>
;; <tab> <backtab> <S-iso-lefttab>  
;; [(shift tab)]
;; [backtab]
;;  (kbd "<S-SPC>") 
;;     [mouse-1]     [mouse-2]      [mouse-2]
;; [C-M-mouse-1] [C-S-mouse-2]  [M-S-mouse-3] 
;; [C-M-mouse-1] [C-S-mouse-2]  [M-S-mouse-3]  
;; [C-M-mouse-1] [C-S-mouse-2]  [M-S-mouse-3]  <M-S-mouse-3>
;; [C-down-mouse-3] [C-down-mouse-2] [C-down-mouse-1] <C-down-mouse-1>
;; [follow-link]
;; (event-modifiers ((kbd "<S-SPC>")) (listify-key-sequence (kbd "<S-SPC>"))) 
;; (event-modifiers (event-convert-list (listify-key-sequence "<S-SPC>")(kbd "<S-SPC>"))))
;; (listify-key-sequence (kbd "<S-SPC>")) 
;; (kbd "<S-SPC>") => [33554464]
;; (event-basic-type 33554464) => 32
;; (event-modifiers 33554464) => shift
;; (event-convert-list '(shift 32))
;; (event-convert-list `(,@(event-modifiers 33554464) ,(event-basic-type 33554464)))
;; (event-apply-modifier (read-event) 'shift 25 "S-")
;; event-apply-shift-modifier
;; event-apply-modifier
;;
;; :EXTRACT-KBD-EVENT-VECTORS
;; (let ((kbd-event (elt (kbd "<s-SPC>") 0))
;;       frob-evnt)
;;   (setq frob-evnt
;;         `(,@(event-modifiers kbd-event)
;;           ,(event-basic-type kbd-event)))
;;   (and (eq (event-convert-list frob-evnt) kbd-event)
;;        (list frob-evnt (vector kbd-event))))
;;
;; :KEYBINDING-HOOK-IDIOM
;; ([add-hook|remove-hook] 
;;  'some-hook-to-add/remove
;;  (function   ;; :NOTE Using `function' or "#'" (sharpquote) allows later
;;   (lambda () ;; removal of anonymous lambda forms.
;;    (define-key 'some-mode-map 
;;        [kbd "<KEY>")|[vector-of-key-chars]|"\KEY-key*\KEY-key*\"]
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
;; Copyright © 2008-2011 MON KEY 
;;; ==============================

;;; CODE:

 
(eval-when-compile (require 'cl))

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))

;;; ==============================
;;; :CHANGESET 2389
;;; :CREATED <Timestamp: #{2011-01-12T13:31:53-05:00Z}#{11023} - by MON KEY>
(defgroup mon-keybindings nil
  "Customization group for variables and functions of :FILE mon-keybindings.el\n
:SEE-ALSO .\n►►►"
  ;; :prefix "<PREFIX>"
  :link '(url-link 
          :tag ":EMACSWIKI-FILE http://www.emacswiki.org/emacs/mon-keybindings.el" 
          "http://www.emacswiki.org/emacs/mon-keybindings.el")
  :link '(emacs-library-link 
          :tag ":FILE mon-keybindings.el"
          "mon-keybindings.el")
  :group 'mon-base)

;;; ==============================
;;; :CHANGESET 2389
;;; :CREATED <Timestamp: #{2011-01-12T13:32:43-05:00Z}#{11023} - by MON KEY>
(defcustom *mon-keybindings-xrefs* 
  '(mon-keybind-globally mon-keybind-dired-mode mon-keybind-completions
    mon-keybind-w3m mon-keybind-emacs-lisp-mode
    mon-keybind-lisp-interaction-mode mon-keybind-slime mon-keybind-conf-mode
    *mon-keybindings-xrefs*)
  "Xrefing list of `mon-keybind-*' symbols, functions constants, and variables.\n
The symbols contained of this list are defined in :FILE mon-keybindings.el\n
:SEE-ALSO `*mon-default-loads-xrefs*', `*mon-default-start-loads-xrefs*',
`*mon-dir-locals-alist-xrefs*', `*mon-testme-utils-xrefs*',
`*mon-button-utils-xrefs*', `*mon-buffer-utils-xrefs*',
`*mon-line-utils-xrefs*', `*mon-plist-utils-xrefs*'
`*mon-seq-utils-xrefs*', `*mon-string-utils-xrefs*', 
`*mon-type-utils-xrefs*',
`*mon-window-utils-xrefs*', `*naf-mode-xref-of-xrefs*',
`*naf-mode-faces-xrefs*', `*naf-mode-date-xrefs*', `*mon-ulan-utils-xrefs*',
`*mon-xrefs-xrefs'.\n►►►"
  :type '(repeat symbol)
  :group 'mon-keybindings
  :group 'mon-xrefs)

;;; ==============================
;;; :TODO This function should read from a list of consed pairs of the form:
;;;  ((kbd "<KEYSTRING>") . <COMMAND>)
;;; And check if the binding exists and if not bind it, but first record the old
;;; binding so we can reset to default if/as needed.
;;; In the its current configuration thre is no clean way to undo the binding of
;;; these global definitions...
;;; :CHANGESET 2389
;;; :CREATED <Timestamp: #{2011-01-12T13:30:46-05:00Z}#{11023} - by MON KEY>
(defun mon-keybind-globally ()
  "Initializes mon preferred global keybindings.\n
When `IS-MON-SYSTEM-P' evaluated at init by `mon-keybind-put-hooks-init' which
establishes an `eval-after-load' form for :FILE mon-keybindings.el\n
:SEE-ALSO `mon-keybind-w3m', `mon-keybind-dired-mode', `mon-keybind-w32-init',
`mon-keybind-lisp-interaction-mode', `mon-keybind-emacs-lisp-mode',
`mon-help-key-functions', `mon-help-keys'.\n►►►"
  (interactive)
  (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-SYSTEM-P))
  ;; :NOT-BOUND-W-C-c
  (global-set-key "\M-n" 'mon-scroll-up-in-place)
  (global-set-key "\M-p" 'mon-scroll-down-in-place)
  (global-set-key (kbd "<C-backspace>") 'backward-kill-word)
  (global-set-key (kbd "<S-backspace>") (kbd "DEL"))
  ;;
  (when (and (intern-soft "IS-W32-P" obarray) ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-W32-P))
    (global-set-key [(f3)] 'w32-maximize-frame)
    ;; "M-!" doesn't like it when IS-W32-P :(
    (global-set-key "\C-c!"       'shell-command))
  ;; ==============================
  ;; :BOUND-W-C-c
  ;; :NOTE Keyindings marked ;!! are bound in naf-mode.el but use a different global-key.
  (global-set-key "\C-cwou"     'mon-wrap-one-url)
  (global-set-key "\C-cflr"     'fill-region)
  (global-set-key "\C-cvm"      'view-mode)                        
  (global-set-key "\C-cu:"      'mon-cln-up-colon)
  ;;
  ;; C-c C-*
  (global-set-key "\C-c\C-di"   'mon-comment-divider)                  ;!! "\C-c\M-di"
  (global-set-key "\C-c\C-gg"   'google-define)                    ;!! "\C-c\M-gg" 
  ;;
  ;; C-c M-*
  (global-set-key "\C-c\M-/"    'hippie-expand)
  (global-set-key "\C-c\M-ar"   'mon-append-to-register)
  (global-set-key "\C-c\M-f"    'mon-flip-windows) 
  ;;
  ;; :CREATED <Timestamp: #{2009-08-24T16:33:22-04:00Z}#{09351} - by MON>
  (global-set-key "\C-cfst"     'mon-file-stamp) 
  (global-set-key "\C-c\C-xd"   'mon-dired-other-window)
  ;;
  ;; ==============================
  ;; :MINIBUFFER
  (define-key minibuffer-local-map "\C-cfs" 'mon-file-stamp-minibuffer)
  ;; ==============================
  ;; :NARROW-MAP
  (define-key narrow-map "r" 'narrow-to-region)
  ;; ==============================
  ;; :TODO `just-one-space' needs a diffent binding M-SPC conflits with
  ;; `IS-MON-P-GNU' fluxbox settings.
  (when (and (intern-soft "IS-MON-P-GNU"  obarray) ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-P-GNU))
    (global-unset-key (kbd "M-<SPC>"))
    (global-set-key   (kbd "s-<SPC>") 'just-one-space))
  ))

;; ==============================
;; (defun mon-keybind-globally ()
;; ;; (assq :check-keys
;; `((,(kbd "<C-backspace>")  . backward-kill-word)
;;   (,(kbd "<S-backspace>")  . ,(kbd "<DEL>"))
;;   (:check-keys
;;    (,(kbd "C-c vm")         . view-mode)
;;    (,(kbd "C-c C-x d")      . mon-dired-other-window)
;;    (,(kbd "C-c !")          . shell-command) ;; "M-!" doesn't like it when IS-W32-P :(
;;    (,(kbd "C-c wou")        . mon-wrap-one-url)
;;    (,(kbd "C-c flr")        . fill-region)
;;    (,(kbd "C-c u:")         . mon-cln-up-colon))
;;   (:check-keys-w32
;;    ;;(when (and (intern-soft "IS-W32-P") (bound-and-true-p IS-W32-P))
;;    (,(kbd "M-<f3>") . w32-maximize-frame)) ;; :WAS [(f3)]
;;   )
;; ==============================
;; "\C-c\C-di"   'mon-comment-divider)                  ;!! "\C-c\M-di"
;; "\C-c\C-dn"   'mon-comment-divider-to-col-four)      ;!! "\C-c\M-dn" 
;; ;; (global-set-key "\C-c\C-in" 'mon-incr)                        ;!! "\C-c\M-in" 
;; "\C-x\C-k1"   'mon-split-designator)             ;!! "\C-C\M-k1"
;; "\C-c\C-kc"   'mon-kill-completions)
;; "\C-c\C-gg"   'google-define)                    ;!! "\C-c\M-gg" 
;; "\C-c\C-na"   'naf-drive-dired-artist-letter)    ;!! "\C-c\M-na"
;; "\C-c\C-nb"   'naf-drive-dired-brand-letter)     ;!! "\C-c\M-nb"
;; ;; (global-set-key "\C-x\C-u" 'describe-char) ;; When it gets rebound.
;;
;; ;; C-c M-*
;; "\C-c\M-/"    'hippie-expand)
;; "\C-c\M-ab"   'mon-append-to-buffer)
;; "\C-c\M-af"   'append-to-file)`\9`
;; "\C-c\M-ar"   'mon-append-to-register)
;; "\C-c\M-f"    'mon-flip-windows) 
;; "\C-c\M-r"    'capitalize-region)                ;!! "\C-c\M-cr"
;; "\C-c\M-php"  'mon-insert-php-comment-divider)
;; "\C-c\M-wd"   'wdired-change-to-wdired-mode)
;;
;; ;; :CREATED <Timestamp: #{2009-08-24T16:33:22-04:00Z}#{09351} - by MON>
;; "\C-cfst" 'mon-file-stamp)

;;; ==============================
;; Commands that are easy to forget to use:
;;
;; `backward-kill-word'      <C-backspace>
;; `what-cursor-position'    C-x=
;; `delete-blank-lines'      C-x C-o
;; `exchange-point-and-mark' C-x C-x  
;; `set-fill-prefix'         C-x.
;; `comment-set-column'      C-x;   
;; `set-fill-column'         C-xf
;; `set-goal-column'         C-x C-n
;; `just-one-space'          M-SPC  
;; `comment-dwim'            M-;
;; `indent-new-comment-line' M-j
;; `delete-indentation'      M-^
;; `delete-horizontal-space' M-\
;; `count-lines-region'      M-=
;; `back-to-indentation'     M-m
;; `async-shell-command'     M-&
;; `yank-pop'                M-y
;; `kill-word'               M-d
;; `backward-kill-word'      M-<delete>
;; `append-next-kill'        M-Cw
;; `goto-line'               M-gg
;; `occur'                   M-so
;; `highlight-regexp'        M-shr 
;; `highlight-phrase'        M-shp
;; `highlight-lines-matching-regexp' M-shl
;; `unhighlight-regexp'              M-shu
;; `hi-lock-find-patterns'           M-shf 

 
;;; ==============================
;;; :ADDED `mon-w3m-dired-file' 
;;; :CHANGESET 1338 <Timestamp: #{2009-12-17T13:21:29-05:00Z}#{09514} - by MON>
(defun mon-keybind-dired-mode ()
  "Adjust `dired-mode-map' keybindings to MON preferences.\n
Added to the `dired-mode-hook' at intit with `mon-keybind-put-hooks-init'.\n
:NOTE can be removed with:\n
\(remove-hook 'dired-mode-hook 'mon-keybind-dired-mode\)\n
:SEE-ALSO `mon-keybind-w3m', `mon-keybind-dired-mode', `mon-keybind-w32-init',
`mon-keybind-lisp-interaction-mode', `mon-keybind-emacs-lisp-mode',
`mon-help-key-functions', `mon-help-keys'.\n►►►"
  (set (make-local-variable 'minor-mode-overriding-map-alist)
       `(ido-mode keymap (remap ,@(remove '(find-file-other-frame . ido-find-file-other-frame)
                                   (cdaddr ido-minor-mode-map-entry)))))
  ;; find-file-other-frame
  ;; (define-key map [remap find-file-other-frame]
  ;; 'ido-find-file-other-frame)
  ;; (local-unset-key "\C-x5\C-f")
  ;; (local-unset-key "\C-x5f")
  (define-key dired-mode-map "\C-x5f"     'mon-dired-find-file-other-frame)
  (define-key dired-mode-map "\C-x5\C-f"  'mon-dired-find-file-other-frame)
  (define-key dired-mode-map "\C-c\M-dwt" 'mon-toggle-dired-dwim-target)
  (define-key dired-mode-map "\M-^"       'mon-dired-up-directory-this-buffer)
  (define-key dired-mode-map "\C-ccl"     'mon-copy-file-dired-as-list) ;; Args: AS-LIST FULL-PATH
  (define-key dired-mode-map "\C-ccf"     'mon-copy-file-dired-as-string) ;; Args: UNQOUTED FULL-PATH
  (define-key dired-mode-map "\C-cw3"     'mon-w3m-dired-file)
  (define-key dired-mode-map "\C-cui"     'mon-dired-uninsert-subdir)
  (define-key dired-mode-map "\C-cua"     'mon-dired-uninsert-subdir-all)
  (define-key dired-mode-map "\C-c\M-wd"  'wdired-change-to-wdired-mode)
  (define-key dired-mode-map "\C-c\M-u"   'mon-dired-unmark-elc)
  ;; :ADDED `dired-efap' keybinding.
  ;; :MODIFICATIONS <Timestamp: #{2009-09-07T14:48:26-04:00Z}#{09371} - by MON>
  (when (featurep 'dired-efap)
    (if (and (intern-soft "IS-MON-P-W32" obarray)
             (bound-and-true-p IS-MON-P-W32))
        (define-key dired-mode-map  [M-f2] 'dired-efap)
      (define-key dired-mode-map  [f2] 'dired-efap))))
;;
;; (remove-hook 'dired-mode-hook 'mon-keybind-dired-mode)
;; (add-hook 'dired-mode-hook 'mon-keybind-dired-mode))

;;; ==============================
;; :COMPLETION-KEYBINDINGS
;;; :TODO Add bindings for `f' to select-next `b' select-previous
;;;
;;; :NOTE Make `n' and `p' move up and down in *Completions* buffer.
;;;       Make SPC scroll the page.
;;;
;;; :CHANGESET 1895
;;; :CREATED <Timestamp: #{2009-12-18T21:48:03-05:00Z}#{09515} - by MON KEY>
(defun mon-keybind-completions ()
  "Add keybindings to `completion-list-mode-map'.\n
Binds `mon-line-move-next', `mon-line-move-prev', and `scroll-up'.\n
:SEE-ALSO `mon-keybind-put-hooks-init', `mon-keybind-dired-mode',
`mon-keybind-completions', `mon-keybind-emacs-lisp-mode',
`mon-keybind-lisp-interaction-mode', `mon-keybind-slime', `mon-keybind-w3m',
`mon-keybind-w32-init', `mon-help-key-functions', `mon-help-keys'.\n►►►"
  (define-key completion-list-mode-map "n"          'mon-line-move-next)
  (define-key completion-list-mode-map "p"          'mon-line-move-prev)
  (define-key completion-list-mode-map (kbd "SPC")  'scroll-up)
  (local-unset-key "q")
  (define-key completion-list-mode-map "q" 'delete-completion-window)
  (define-key completion-list-mode-map "Q" 'quit-window)
  ;; (define-key completion-list-mode-map (kbd "<S-SPC>")  'scroll-down)
  )

;; (remove-hook 'completion-list-mode-hook 'mon-keybind-completions)
;; (add-hook 'completion-list-mode-hook 'mon-keybind-completions)

;; (delete-completion-window)

;; let-bind the current slime buffer the set that to the local
;; `completion-reference-buffer' on entry to buffer
;; `slime-completions-buffer-name'
;; `completion-setup-hook'
;; `completion-setup-function'
;; `completion-annotate-function'
;; (set (make-local-variable 'completion-reference-buffer) )
;; minibuffer-completion-help
;; minibuffer-hide-completions
;; display-buffer-mark-dedicated

;; (let ((comp-ref-bfr
;;        (or (minibufferp completion-reference-buffer)
;;            (window-minibuffer-p (selected-window))
;;            (eq (current-buffer) completion-reference-buffer)
;;            (eq (help-buffer) completion-reference-buffer)
;;            (string-match-p "^ " (buffer-name completion-reference-buffer))
;;            (null (buffer-live-p (get-buffer completion-reference-buffer)))
;;            ;; fall thru
;;            (other-buffer (get-buffer-window "*Completions*" 0) 
;;                          completion-reference-buffer) nil t)
;;        ))


;; (buffer-live-p completion-reference-buffer)
;; bury-buffer
;; 
;; completion-list-mode-map
;; 
;; slime-complete-symbol-function
;; slime-complete-symbol
;; completion-reference-buffer
;; deletei

;; (when (get-buffer slime-completions-buffer-name)

;; slime-completions-window
;; slime-completions-buffer-name
;; display-completion-list

;;; ==============================
;;(remove-hook'Info-mode-hook  
(add-hook 'Info-mode-hook  
	  (function 
           (lambda () 
             (define-key Info-mode-map "\M-n" 'mon-scroll-up-in-place)
             ;; :NOTE follwing synch w/ MON binding for `help-go-forward' and `help-go-back'
             ;; Move back in history to the last node you were at.
             (define-key Info-mode-map "\C-c\C-b" 'Info-history-back)
             ;; Move forward in history to the node you returned from after using l.
             (define-key Info-mode-map "\C-c\C-f" 'Info-history-forward)
             (define-key Info-mode-map "\C-cia" 'info-apropos))))


;;; ==============================
;; (remove-hook 'ido-minibuffer-setup-hook
(add-hook 'ido-minibuffer-setup-hook
          (function 
           (lambda ()
             (define-key ido-completion-map (kbd "<backtab>") 'ido-complete))))

(cond ((and (intern-soft "IS-MON-P" obarray) ;; *IS-MON-OBARRAY*
            (bound-and-true-p IS-MON-P))
       (when (bound-and-true-p Tex-mode-map)  
         (define-key TeX-mode-map (kbd "<S-iso-lefttab>") 'TeX-complete-symbol))))

;;; ==============================
;; :W3M-MODE-MAP
;; `w3m-mode-map', `w3m-mode-hook'
;; ==============================
;; :NOTE Following is some unfinished work-notes regarding pending customization
;; of w3m keymaps/hooks
;; `w3m-mode-map'
;; `w3m-mode-menu'
;; No, This is M-n 
;; w3m-copy-buffer2 
;; (14 . w3m-next-buffer)
;; (16 . w3m-previous-buffer)
;; (20 . w3m-copy-buffer)
;; (22 . w3m-history-restore-position)
;; (67108896 . w3m-history-store-position)
;; (0 . w3m-history-store-position))
;; (browse-url-generic-program  'w3m-browse-url)
;; (browse-url-browser-function 'common-lisp-hyperspec)
;;

;; ==============================
;; :WAS (add-hook 'w3m-mode-hook
;;       (function (lambda ()
;;          (progn { ... } nil t)
;;; :CHANGESET 1869
;;; :CREATED <Timestamp: #{2010-06-15T11:33:31-04:00Z}#{10242} - by MON KEY>
(defun mon-keybind-w3m ()
  "Adjust `w3m-mode-map' keybindings to MON preferences.\n
 :BIND   `<up>'   <- `mon-scroll-down-in-place'
 :BIND   `n'      <- `mon-scroll-down-in-place'
 :BIND   `<down>' <- `mon-scroll-up-in-place'
 :BIND   `p'      <- `mon-scroll-up-in-place'
 :BIND   `q'      <- `w3m-delete-buffer'
 :BIND   `v'      <- `w3m-view-source'
 :BIND   `c'      <- `mon-w3m-kill-url-at-point'
 :BIND   `M-C'    <- `w3m-print-current-url'
 :BIND   `u'      <- `w3m-print-this-url'
 :BIND   `f'      <- `w3m-goto-url'
 :BIND   `r'      <- `w3m-reload-this-page' 
 :BIND   `R'      <- `w3m-redisplay-this-page'
 :BIND   `M-p'    <- `w3m-previous-buffer'
 :BIND   `M-n'    <- `w3m-next-buffer'
 :BIND   `C-c b'  <- `w3m-view-previous-page'
 :BIND   `C-c f'  <- `w3m-view-next-page'\n
Hook added with `mon-keybind-put-hooks-init'.
Can be manually removed later with:
 \(remove-hook 'w3m-mode-hook 'mon-keybind-w3m\)\n
:SEE-ALSO `mon-keybind-dired-mode', `mon-keybind-w32-init',
`mon-keybind-lisp-interaction-mode', `mon-keybind-emacs-lisp-mode',
`mon-help-key-functions', `mon-help-keys'.\n►►►"
  (let ((mkw3-msg (concat
                    ":FUNCTION `mon-keybind-w3m' "
                    "-- evaluated on `w3m-mode-hook")))
  ;; :LOCAL-UNSET
  (local-unset-key  (kbd "<up>"))
  (local-unset-key  (kbd "<down>"))
  (local-unset-key  (kbd "M-C"))
  (local-unset-key  (kbd "M-p")) ;;; (kbd "M-p") [134217840]
  ;; `report-emacs-w3m-bug' to clost to our C-c b binding on `w3m-view-previous-page'
  (local-unset-key (kbd "C-c C-b")) 
  (local-unset-key  "n")
  (local-unset-key  "p")
  (local-unset-key  "c")
  (local-unset-key  "G")
  (local-unset-key  "U")
  (local-unset-key  "r")
  (local-unset-key  "R")
  ;; `w3m-close-window' Close all emacs-w3m windows, without deleteing buffers. 
  (local-unset-key  "q")
  ;; `w3m-copy-buffer' This was a really bad UI design decision. 
  (local-unset-key (kbd "M-n"))
  ;; `w3m-view-source' Display the html source of the current page.   
  (local-unset-key [92]) ;; (string-to-char "\\") 92
  ;;
  ;; :LOCAL-SET
  ;;
  ;; Or, `next-line' 
  ;; (local-set-key    "n" 'mon-scroll-down-in-place) ;; :NOTE also bound `j'.
  (define-key w3m-mode-map "n" 'mon-scroll-down-in-place) ;; :NOTE also bound `j'.
  ;;
  ;; Or, `previous-line' 
  ;; (local-set-key    "p"           'mon-scroll-up-in-place)   ;; :NOTE also bound `k'.
  (define-key w3m-mode-map "p" 'mon-scroll-up-in-place) ;; :NOTE also bound `k'.
  ;;
  ;; `w3m-delete-buffer' Delete the current emacs-w3m buffer.
  ;;(local-set-key     "q"           'w3m-delete-buffer) ;; :NOTE Also bound to `C-c C-w'
  (define-key w3m-mode-map "q" 'w3m-delete-buffer) ;; :NOTE Also bound to `C-c C-w'
  ;;
  ;; `w3m-next-buffer' Turn the page of emacs-w3m buffers ahead. 
  ;; (local-set-key   (kbd "M-n")     'w3m-next-buffer)
  (define-key w3m-mode-map (kbd "M-n")     'w3m-next-buffer)
  ;;
  ;; `w3m-view-source' Display the html source of the current page.   
  ;;(local-set-key     "v"           'w3m-view-source)
  (define-key w3m-mode-map "v" 'w3m-view-source)
  ;;
  ;; `w3m-print-current-url' Display the url of the current page and put it into `kill-ring'.
  ;; :NOTE `w3m-copy-current-url-as-kill' aliases `w3m-print-current-url' in mon-url-utils.el
  ;; (local-set-key    (kbd "M-C")    'w3m-print-current-url) 
  (define-key w3m-mode-map (kbd "M-C") (or (intern-soft "w3m-copy-current-url-as-kill" obarray) 
                                           (intern-soft "w3m-print-current-url" obarray)))
  ;;
  ;;(local-set-key    "c"            'mon-w3m-kill-url-at-point)
  (define-key w3m-mode-map "c" 'mon-w3m-kill-url-at-point)
  ;;
  ;; `w3m-print-this-url' Display the url under point and put it into `kill-ring'.
  ;; `w3m-copy-this-url-as-kill' aliases `w3m-print-this-url' in mon-url-utils.el
  ;;(local-set-key    "u"            'w3m-print-this-url)
  (define-key w3m-mode-map "u" (or (intern-soft "w3m-copy-this-url-as-kill" obarray)
                                   (intern-soft "w3m-print-this-url" obarray)))
  ;;
  ;; `w3m-goto-url-new-session' Visit the web page in a new session.
  ;;(local-set-key   (kbd "C-u f")        'w3m-goto-url-new-session)
  (define-key w3m-mode-map (kbd "C-u f") 'w3m-goto-url-new-session)
  ;;
  ;; `w3m-goto-url' Visit the web page.
  ;;(local-set-key "f" 'w3m-goto-url)
  (define-key w3m-mode-map "f" 'w3m-goto-url)
  ;;
  ;;(local-set-key "r" 'w3m-reload-this-page)
  (define-key w3m-mode-map  "r" 'w3m-reload-this-page)
  ;;
  ;;(local-set-key "R" 'w3m-redisplay-this-page)
  (define-key w3m-mode-map "R" 'w3m-redisplay-this-page)
  ;;
  ;; `w3m-previous-buffer' Turn the page of emacs-w3m buffers behind.
  ;;(local-set-key (kbd "M-p")  'w3m-previous-buffer) 
  (define-key w3m-mode-map (kbd "M-p")     'w3m-previous-buffer)
  (define-key w3m-mode-map (kbd "M-n")     'w3m-next-buffer)
  (define-key w3m-mode-map (kbd "C-c b")   'w3m-view-previous-page)
  (define-key w3m-mode-map (kbd "C-c f")   'w3m-view-next-page)
  ;;(local-set-key          (kbd "<up>")   'mon-scroll-down-in-place)
  (define-key w3m-mode-map (kbd "<up>")    'mon-scroll-down-in-place)
  ;; (local-set-key    (kbd "<down>")      'mon-scroll-up-in-place)
  (define-key w3m-mode-map  (kbd "<down>") 'mon-scroll-up-in-place)
  (minibuffer-message mkw3-msg)))
;;
;; (remove-hook 'w3m-mode-hook 'mon-keybind-w3m)
;; (add-hook 'w3m-mode-hook 'mon-keybind-w3m)

;;; ==============================
;; :EMACS-LISP-MODE-KEYMAP
;;; (symbol-value 'emacs-lisp-mode-map)
;;; (global-set-key "\C-c\M-;"   'mon-user-evald)

;;; ==============================
;;; :CHANGESET 1869
;;; :CREATED <Timestamp: #{2010-06-16T11:22:27-04:00Z}#{10243} - by MON KEY>
(defun mon-keybind-emacs-lisp-mode ()
  "Bind keys on the `emacs-lisp-mode-map'.\n
Run on the `emacs-lisp-mode-hook'\n
:EXAMPLE\n\n\(symbol-function 'mon-keybind-emacs-lisp-mode\)\n
:SEE-ALSO `mon-keybind-w3m', `mon-keybind-dired-mode', `mon-keybind-w32-init'
`mon-keybind-lisp-interaction-mode', `mon-help-key-functions', `mon-help-keys'.\n►►►"
  ;; `recenter-top-bottom' was changed significantly w/ v23, I can't deal.
  ;; :SEE :FILE lisp/window.el 
  ;; :SEE :VARIABLE `resize-mini-windows'
  ;; :SEE (URL `http://debbugs.gnu.org/cgi/bugreport.cgi?bug=6192')
  ;; :FIXME This needs to be globally unbound :(
  (local-unset-key [?\C-l])
  (define-key emacs-lisp-mode-map "\M-i"         'indent-according-to-mode)
  (define-key emacs-lisp-mode-map "\C-cc"        'comment-region)
  (define-key emacs-lisp-mode-map "\C-c\C-uc"    'uncomment-region)
  (define-key emacs-lisp-mode-map "\C-c\C-di"    'mon-comment-divider)
  (define-key emacs-lisp-mode-map "\C-c\C-d4"    'mon-comment-divider-to-col-four)
  (define-key emacs-lisp-mode-map "\C-cst"       'mon-insert-lisp-stamp)
  (define-key emacs-lisp-mode-map "\C-ctm"       'mon-insert-lisp-testme)
  ;; :NOTE  C-c C-b bound to `slime-interrupt' in Slime-mode!!
  (define-key emacs-lisp-mode-map "\C-cBr"       'boxquote-region)
  (define-key emacs-lisp-mode-map "\C-cBu"       'boxquote-unbox)
  (define-key emacs-lisp-mode-map "\C-c\C-x\C-e" 'mon-eval-sexp-at-point)
  (define-key emacs-lisp-mode-map "\C-c\M-:"     'mon-eval-expression)
  (define-key emacs-lisp-mode-map "\C-c\C-j"     'mon-eval-print-last-sexp)
  (define-key emacs-lisp-mode-map "\C-c\C-cb"    'mon-princ-cb)
  (define-key emacs-lisp-mode-map "\C-cal"       'align-let) ;; :WAS [?\C-c ?\C-a] in align-let.el
  (define-key emacs-lisp-mode-map "\C-cel"       'mon-escape-lisp-string-region)
  ;; (define-key emacs-lisp-mode-map "\C-cexs"     'mon-insert-lisp-doc-eg-xref)
  (define-key emacs-lisp-mode-map "\C-c\C-dc"    'mon-insert-lisp-doc-eg-xref)
  (define-key emacs-lisp-mode-map "\C-cul"       'mon-unescape-lisp-string-region)
  (define-key emacs-lisp-mode-map "\C-clsl"      'mon-line-strings-to-list)
  (define-key emacs-lisp-mode-map "\C-clb"       'mon-line-strings-bq-qt-sym-bol)
  (define-key emacs-lisp-mode-map "\C-c\t"       'lisp-complete-symbol)
  (define-key emacs-lisp-mode-map (kbd "<S-iso-lefttab>")  'lisp-complete-symbol)
  (define-key emacs-lisp-mode-map (kbd "<backtab>")  'lisp-complete-symbol)
  (define-key emacs-lisp-mode-map (kbd "<S-tab>")  'lisp-complete-symbol)
  ;; :NOTE `slime-compile-and-load-file' is on C-c C-k
  (define-key emacs-lisp-mode-map "\C-c\C-k"  'emacs-lisp-byte-compile-and-load)
  )
;;
;; (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
;;            (bound-and-true-p IS-MON-SYSTEM-P))
;;   ;; (remove-hook 'emacs-lisp-mode-hook 'mon-keybind-emacs-lisp-mode)
;;   (add-hook 'emacs-lisp-mode-hook  'mon-keybind-emacs-lisp-mode))

;;; ==============================
;;; :PREFIX "mklim-"
;; :LISP-INTERACTION-MODE-KEYMAP
(defun mon-keybind-lisp-interaction-mode (&optional w-msg)
  "Bind keys on the `lisp-interaction-mode-map'.\n
When optional arg W-MSG is non-nil message that new keybinding were made on
buffer-local entry to `lisp-interaction-mode'.\n
Run on the `lisp-interaction-mode-hook'\n
Added to the `lisp-interaction-mode-hook' at init with `mon-keybind-put-hooks-init'.\n
:EXAMPLE\n\n\(symbol-function 'mon-keybind-emacs-lisp-mode\)\n
\(symbol-value 'lisp-interaction-mode-map\)\n
:SEE-ALSO `mon-keybind-emacs-lisp-mode', `mon-keybind-dired-mode',
`mon-keybind-w3m', `mon-keybind-w32-init', `mon-help-key-functions',
`mon-help-keys'.\n►►►"
  (let ((mklim-msg (and w-msg
                        (cons 
                         (concat ":FUNCTION `mon-keybind-lisp-interaction-mode' "
                                 "-- evaluated on `lisp-interaction-mode-hook' on entry in buffer %S")
                         (get-buffer (current-buffer))))))
    ;; :NOTE Slime binds C-j to `slime-eval-print-last-expression'
    (local-unset-key "\C-j")
    (define-key lisp-interaction-mode-map "\C-j" 'newline-and-indent)
    (define-key lisp-interaction-mode-map "\C-c\C-j" 'mon-eval-print-last-sexp)
    (define-key lisp-interaction-mode-map (kbd "<S-iso-lefttab>")  'lisp-complete-symbol)
    (define-key lisp-interaction-mode-map (kbd "<S-tab>") 'lisp-complete-symbol)
    (define-key lisp-interaction-mode-map (kbd "<backtab>") 'lisp-complete-symbol)
    ;; (slime-log-event (format (car mklim-msg) (cdr mklim-msg)))
    ;;(minibuffer-message (car mklim-msg) (cdr mklim-msg))
    (and mklim-msg (message (car mkb-slm-msg) (cdr mkb-slm-msg)))))
;; 
(when (and (intern-soft "IS-MON-P" obarray) ;; *IS-MON-OBARRAY*
           (bound-and-true-p IS-MON-P))
  ;; (remove-hook 'lisp-interaction-mode-hook 'mon-keybind-lisp-interaction-mode)
  (add-hook 'lisp-interaction-mode-hook 'mon-keybind-lisp-interaction-mode))

;;; ==============================
;;; :PREFIX "mkb-slm-"
;;; :CHANGESET 1895
;;; :CREATED <Timestamp: #{2010-06-17T15:04:09-04:00Z}#{10244} - by MON KEY>
(defun mon-keybind-slime (&optional w-msg)
  "Bind keys on the `slime-mode-map'.\n
When optional arg W-MSG is non-nil message that new keybinding were made on
buffer-local entry to `slime-mode'.\n
Added to the `slime-mode-hook' at init with `mon-keybind-put-hooks-init'.\n
:EXAMPLE\n\n\(symbol-function 'mon-keybind-slime\)\n
\(symbol-value 'slime-mode-map\)\n
:NOTE Can be removed with:\n
 \(remove-hook 'slime-mode-hook 'mon-keybind-slime\)\n
;; :SLIME-KEYMAPS                     :SLIME-KEY-BINDINGS 
`slime-mode-map'                      `slime-keys'
`slime-mode-indirect-map'             
`slime-parent-map'                    `slime-parent-bindings'
`slime-prefix-map'                    `slime-prefix-bindings'
`slime-editing-map'                   `slime-editing-keys'
`slime-doc-map'                       `slime-doc-bindings'
`slime-who-map'                       `slime-who-bindings'
`slime-popup-buffer-mode-map'         `slime-popup-buffer-mode'  
`slime-xref-mode-map'                 `slime-xref-mode'
`slime-inspector-mode-map'            `slime-inspector-mode'
`slime-connection-list-mode-map'      `slime-connection-list-mode'
`slime-thread-control-mode-map'       `slime-thread-control-mode'
`sldb-mode-map'                       `sldb-mode'
`slime-macroexpansion-minor-mode-map' `slime-macroexpansion-minor-mode'\n
`slime-fuzzy-completions-map'         `slime-fuzzy-completions-mode'
;; :SLIME-MENUS
`slime-cheat-sheet-table'
`slime-sldb-easy-menu'
`slime-easy-menu'\n
;; :SLIME-KEYMAP-FUNCTIONS
`slime-init-keymaps'
`slime-init-keymap'
`slime-bind-keys'
`slime-define-keys'
`slime-add-easy-menu'
`slime-sldb-add-easy-menu'
`slime-cheat-sheet-table'\n
:NOTE Slime doesn't always directly map commands to bindings and many maps are
bound inside forms wrapped with `slime-init-keymaps', `slime-init-keymap',
`slime-bind-keys', `slime-define-keys', etc. As such, finding keybindings isn't
always easy.\n As a friendly reminder, here is how it is done:\n
\(lookup-key slime-mode-map \"<SOME><KEYS>\"\)\n
\(lookup-key slime-mode-map \"\\C-cB\"\)\n
\(where-is '<SOME-SLIME-COMMAND>\)\n
\(where-is 'slime-interrupt\)\n
;; :SLIME-PARENT-MAP-KEYS
\\{slime-parent-map}\n
;; :SLIME-PREFIX-MAP-KEYS
\\{slime-prefix-map}\n
;; :SLIME-EDITING-MAP-KEYS
\\{slime-editing-map}\n
;; :SLIME-DOC-MAP-KEYS
\\{slime-doc-map}\n
;; :SLIME-WHO-MAP-KEYS
\\{slime-who-map}\n
;; :SLIME-MODE-MAP-KEYS
\\{slime-mode-map}\n
:SEE-ALSO `mon-help-CL-slime-keys', `slime-cheat-sheet', `mon-slime-setup-init',
`mon-keybind-lisp-interaction-mode', `mon-keybind-emacs-lisp-mode',
`mon-keybind-dired-mode', `mon-keybind-w3m', `mon-keybind-w32-init',
`mon-help-key-functions', `mon-help-keys'.\n►►►"
  (let ((mkb-slm-msg (and w-msg 
                          (cons 
                           (concat ":FUNCTION `mon-keybind-slime' "
                                   "-- evaluated on `slime-mode-hook' on entry in to buffer %S")
                           (get-buffer (current-buffer))))))
    (when (and (intern-soft "IS-MON-P" obarray) ;; *IS-MON-OBARRAY*
               (bound-and-true-p IS-MON-P))
      (local-unset-key "\C-c\C-b")
      (define-key slime-mode-map "\C-cB"      'slime-interrupt)
      (local-unset-key "\C-c\C-c")
      (define-key slime-mode-map "\C-cc"      'comment-region)
      (local-unset-key "\C-c\C-uc") ;; :WAS `slime-undefine-function'
      (define-key slime-mode-map "\C-c\C-uc"  'uncomment-region)
      (define-key slime-mode-map "\C-c\C-ud"  'slime-undefine-function)
      ;;
      ;; FU slime-package-fu.el not only does it not work well but... 
      ;; the default binding for `slime-export-symbol-at-point' to "C-cx" really sucks!
      (when (featurep 'slime-package-fu)
        (local-unset-key  "\C-cx") ;;  :WAS `slime-export-symbol-at-point'
        (define-key slime-mode-map "\C-cex" 'slime-export-symbol-at-point))
      ;;
      (define-key slime-mode-map "\C-c\C-o"  'slime-compile-defun)
      ;;
      (local-unset-key "\M-n")
      (define-key slime-mode-map "\M-n"     'mon-scroll-up-in-place)
      (define-key slime-mode-map "\C-c\M-n" 'slime-next-note)
      ;;
      (local-unset-key "\M-p")
      (define-key slime-mode-map "\M-p"     'mon-scroll-down-in-place)
      (define-key slime-mode-map "\C-c\M-p" 'slime-previous-note)
      ;;
      (define-key slime-mode-map "\C-cel"    'mon-escape-lisp-string-region)
      (define-key slime-mode-map "\C-cul"    'mon-unescape-lisp-string-region)
      (define-key slime-mode-map "\C-c\C-dr" 'mon-insert-slime-arglist)
      (define-key slime-mode-map "\C-c\C-dc" 'mon-insert-lisp-doc-eg-xref)
      (define-key slime-mode-map "\C-c\C-dj" 'mon-insert-lisp-CL-jump-doc)
      ;; :TODO Add keybinding:
      ;; (define-key slime-mode-map           'mon-insert-lisp-CL-debug)
      ;; (define-key slime-mode-map           'mon-insert-lisp-CL-eval-when)
      (define-key slime-mode-map "\C-ctm"    'mon-insert-lisp-testme)
      (define-key slime-mode-map "\C-cst"    'mon-insert-lisp-stamp)
      (define-key slime-mode-map "\C-c\C-j"  'slime-eval-print-last-expression)
      ;; 
      ;; `slime-compile-file'
      ;; `slime-compile-and-load-file' is on C-c C-k
      ;; 
      ;; (define-key 'slime-mode-map 'mon-quit-slime-description-window)
      ;; 'mon-next-xref-slime)
      ;; 'mon-prev-xref-slime)
      )
    ;; (when IS-MON-P-W32
    ;;   (define-key slime-mode-map  (kbd "<backtab>") 'slime-complete-symbol)
    ;;   (define-key slime-mode-map  (kbd "<S-tab>")  'slime-complete-symbol)
    ;;   (define-key slime-repl-mode-map  (kbd "<backtab>") 'slime-complete-symbol)
    ;;   (define-key slime-repl-mode-map  (kbd "<S-tab>")  'slime-complete-symbol)
    ;;   ;; :NOTE I don't think this is getting used. Can it hurt?
    ;;   (define-key slime-mode-map (kbd "<S-iso-lefttab>") 'slime-complete-symbol))
    (when (and (intern-soft "IS-MON-P-GNU" obarray) ;; *IS-MON-OBARRAY*
               (bound-and-true-p IS-MON-P-GNU))
      (define-key slime-mode-map (kbd "<S-tab>")         'slime-complete-symbol)
      (define-key slime-mode-map (kbd "<S-iso-lefttab>") 'slime-complete-symbol)
      (define-key slime-mode-map (kbd "<backtab>")       'slime-complete-symbol)
      (define-key slime-repl-mode-map (kbd "<S-tab>")    'slime-complete-symbol)
      (define-key slime-repl-mode-map (kbd "<backtab>")  'slime-complete-symbol)
      (define-key slime-repl-mode-map (kbd "<S-iso-lefttab>") 'slime-complete-symbol))
    ;; (and mkb-slm-msg (minibuffer-message (car mkb-slm-msg) (cdr mkb-slm-msg)))
    (and mkb-slm-msg (message (car mkb-slm-msg) (cdr mkb-slm-msg)))
    ))

;; `slime-fuzzy-target-buffer'
;; (listify-key-sequence )
;; (event-modifiers (kbd "<super>f"))

;;; ==============================
;; :MESSAGE-MODE-KEYBINDINGS
;; (remove-hook 'message-mode-hook
(add-hook 'message-mode-hook
          (function 
	   (lambda ()
	     (progn
	       (local-unset-key "\C-c\C-d")
	       (define-key message-mode-map "\C-c\C-dd" 'message-dont-send)))))


;;; ==============================
;;; :CHANGESET 2142 <Timestamp: #{2010-09-27T13:51:19-04:00Z}#{10391} - by MON KEY>
;;; :CREATED <Timestamp: #{2010-09-26T12:59:09-04:00Z}#{10387} - by MON>
(defun mon-keybind-conf-mode ()
  "Add local keybindings to `conf-mode-map'.\n
Change C-c C-c mapping from `conf-colon-mode' to `comment-region'.\n
Add binding for C-cst for `mon-stamp-in-context'.\n
Run on the `conf-mode-hook' when `IS-MON-P'.\n
:SEE-ALSO `conf-mode'.\n►►►"
  (local-unset-key "\C-c\C-c") ;; '`conf-colon-mode' :NOTE still bound to "\C-c:"
  (define-key conf-mode-map "\C-cc"     'comment-region)
  (define-key conf-mode-map "\C-cst" #'(lambda (w-changed)
                                         (interactive "P")
                                         (mon-stamp-in-context nil t 
                                                               :w-changed t 
                                                               :w-divider nil 
                                                               :w-newline t))))
;;
(when (and (intern-soft "IS-MON-SYSTEM-P" obarray)
           (bound-and-true-p IS-MON-SYSTEM-P)
           (intern-soft "IS-MON-P" obarray)
           (bound-and-true-p IS-MON-P))
  ;;(remove-hook 'conf-mode-hook 'mon-keybind-conf-mode)
  (add-hook 'conf-mode-hook 'mon-keybind-conf-mode))

;;; ==============================
;; :EMACS-SERVER-KEYBINDINGS
;;; :CREATED <Timestamp: #{2010-01-27T22:48:03-05:00Z}#{10043} - by MON KEY>
(when (and (intern-soft "IS-MON-P-GNU" obarray)
           (bound-and-true-p IS-MON-P-GNU))
  ;; (remove-hook 'server-switch-hook  
  (add-hook 'server-switch-hook  
            (function 
             (lambda () 
               (local-set-key (kbd "C-x #") 'server-edit)))
            ))

;;; ==============================
;;; Don't bind `ibuffer-do-revert' -- the key is too easy to confuse or touch
;;; arbitrarily
;;; :CREATED <Timestamp: #{2010-12-02T21:30:07-05:00Z}#{10484} - by MON KEY>
(add-hook 'ibuffer-mode-hook 
          (function (lambda () 
                      ;; :WAS `ibuffer-do-revert'
                      (local-set-key (kbd "V") #'ignore))))


;;; :CREATED <Timestamp: #{2011-01-04T12:58:58-05:00Z}#{11012} - by MON KEY>
;; `nxml-mode-map'
(add-hook 'nxml-mode-hook 
          (function (lambda () 
                      (unless (key-binding (kbd "\C-cc"))
                        (local-set-key (kbd "\C-cc") #'comment-region)))))

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

;;; (buffer-local-variables (get-buffer "*bzr-status*"))
;;; (buffer-local-variables (get-buffer "*bzr-status"))
;;;
;;; dvc-fileinfo-revert-files

;; (remove-hook 'dvc-status-mode-hook
(add-hook 'dvc-status-mode-hook
          (function 
           (lambda () 
             (local-unset-key dvc-status-mode-map "U") ;;; dvc-fileinfo-revert-files)
             (define-key dvc-status-mode-map dvc-keyvec-revert nil)
             ;; dvc-fileinfo-revert-files
             )))

;; (add-hook 'dvc-diff-mode-hook
;;           (function (lambda ()
;;                       (local-unset-key 
;;                        ;; dvc-fileinfo-revert-files [85]

;;; (buffer-local-value 'dvc-show-active-dvc-string (current-buffer))
;;; dvc-show-active-dvc-string

;;; :VARIABLE `dvc-keyvec-revert' <- (char-to-string 85) -> U 
;;;
;;; Whomever, chose to hide _anything_ that reverts or is related to reverting
;;; in a dedicated symbol vector buried in DVC's package spaghetti can FUCK OFF!
;;; How is this variable transparent to the user??? 
;;; Find a better way to make your code portable across DVC's... and document it!
;;;
;;; :SEE-ALSO `dvc-fileinfo-revert-files' `dvc-status-mode-map'

;; `vc-revert' C-x v u
;; dvc-status-mode-map
;; dvc-keyvec-revert
;; U dvc-fileinfo-revert-files

 
;;; ==============================
(provide 'mon-keybindings)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; generated-autoload-file: "./mon-loaddefs.el"
;; coding: utf-8
;; no-byte-compile: t
;; End:

;;; ================================================================
;;; mon-keybindings.el ends here
;;; EOF

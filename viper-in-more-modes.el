;;; viper-in-more-modes.el --- vi-like keybindings for various Emacs modes

;; Copyright (C) 2007 Alessandro Piras
;; Copyright (C) 2007 Brad Beveridge
;; Copyright (C) 2007 Jason Spiro
;; Copyright (C) 2010 Alexey Romanov
;;
;; Author: Alessandro Piras <laynor at gmail.com>
;;      Brad Beveridge <brad.beveridge at gmail.com>
;;      Alexey Romanov <alexey.v.romanov at gmail.com>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>
;; Version: 0.1.3+git
;; URL: http://gitorious.org/viper-in-more-modes/
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file is an unofficial add-on to Michael Kifer's Viper-mode.
;; It provides vi-like keymaps for various major modes, including
;; emacs-lisp-mode, lisp-mode, lisp-interaction-mode, slime-repl-mode,
;; LaTeX-mode, haskell-mode, prolog-mode and ruby-mode. If you have
;; any questions or comments, please e-mail the authors and the
;; maintainer.
;;
;; Load the package with:
;;
;; (require 'viper-in-more-modes)
;;
;; This is alpha-quality code. If it works for you, let us know.

;;; TODO:

;; * Rename viper-in-more-modes to something much shorter. This will
;;   not only give viper-in-more-modes a shorter name, but will also
;;   instantly provide us with a better prefix.
;;
;; * Clean up the code in general: for example, the error messages
;;   shouldn't include exclamation marks. And the grammar and
;;   capitalization in the comments should be improved. Also, the
;;   boxed comments probably don't have to be in boxes.
;;
;; * Submit it to M-x report-emacs-bug and ask them to please include
;;   viper-in-more-modes as part of Viper. If we can't reach some
;;   contributors for copyright assignment, we'll probably have to
;;   discard their contributions at this time, so we should probably
;;   try to get all contributors' mailing addresses and phone numbers
;;   as soon as they've contributed fifteen lines or more, in case
;;   they later disappear.

;;; Change Log:

;; Version 0.1.3: Added bindings for Haskell, Ruby, Prolog and
;;   LaTeX (from AUCTeX) modes. Thank you, Alexey Romanov
;;   <alexey.v.romanov at gmail.com>. Changed prefix from "vimper"
;;   to "viper-imm" and renamed `viper-leader-char' to
;;   `viper-imm-leader-char'. Created a customization group.
;; Version 0.1.2: Removed some duplicate keybinding code.
;;   Also, slime-list-callees is now on the ">" key instead of
;;   the "<" key, which was already taken. Thank you, Stephen Bach
;;   <sjbach at comcast.net>.
;; Version 0.1.1: Made `viper-leader-char' a var, not a const.
;;   Thank you, John J Foerch <jjfoerch at earthlink.net>.
;; Version 0.1: Initial upload to EmacsWiki.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Code:

(defgroup viper-in-more-modes nil
  "vi-like keybindings for various modes."
  :group  'emulations
  :link   '(custom-group-link "viper")
  :prefix 'viper-imm)

;;; Begin utility code {{{

;; Macro workaround to make some commands work on the character
;; the cursor is on too
(defmacro do-one-char-forward (&rest body)
  "Wraps the body between `forward-char' and `backward-char' to make commands
work on closed parens like one can expect in vi."
  `(progn
     (forward-char)
     ,@body
     (backward-char)))

(defmacro def-simple-viper-imm-wrapper-ocf (name args &rest body)
  "Define a wrapper for a command to execute it as if the cursor was one
   char forward the current position. Uses `do-one-char-forward'. Use it
   like a defun without lambda-list.  See examples below."
  `(defun ,name (,@args)
     (interactive)
     (do-one-char-forward
      ,@body)))

;;; }}} End utility code

;;; Begin major mode keybinding code {{{

;; Major mode keybindings and functions used by viper-in-more-modes
(defcustom viper-imm-leader-char " "
  "Leader char for viper-in-more-modes keymaps."
  :group 'viper-in-more-modes)

(defmacro viper-imm-defkey-l (map key func)
  `(define-key ,map (concat viper-imm-leader-char ,key) ,func))

;;;; Emacs Lisp Mode

(defcustom viper-imm-lisp-bindings t
  "Lisp bindings."
  :type  'boolean
  :group 'viper-in-more-modes)

;;; Commands definitions

;; Almost all are workarounds due to the fact that Emacs wants the
;; cursor to be AFTER the ")" to execute functions on S-expressions.
;; We use the `do-one-char-forward' utility macro here (see above for
;; details on that macro).
(def-simple-viper-imm-wrapper-ocf viper-imm-eval-last-sexp
  (&optional eval-last-sexp-arg-internal)
  (eval-last-sexp eval-last-sexp-arg-internal))

(defun viper-imm-eval-region (&optional arg)
  (interactive "P")
  (if (and (boundp 'vimpulse-visual-mode)
           (not vimpulse-visual-mode))
      (error "Select the region in Visual Mode.")
    (eval-region (min (mark) (point)) (max (mark) (point)))
    (vimpulse-visual-mode 'toggle)))

(defun viper-imm-pp-eval-region ()
  (interactive)
  (message (pp-to-string (viper-imm-eval-region))))

(def-simple-viper-imm-wrapper-ocf viper-imm-pp-eval-last-sexp
  (&optional eval-last-sexp-arg-internal)
  (pp-eval-last-sexp eval-last-sexp-arg-internal))

;; Macroexpand command (macroexpands last S-expression)
(def-simple-viper-imm-wrapper-ocf viper-imm-macroexpand ()
  (pp-macroexpand-expression (sexp-at-point)))

;; Macroexpand-all command (macroexpands-all last S-expression)
(def-simple-viper-imm-wrapper-ocf viper-imm-macroexpand-all ()
  (pp-macroexpand-expression (macroexpand-all (sexp-at-point))))

;;; Bindings

(defvar viper-imm-emacs-lisp-mode-vi-map
  (let ((map (make-sparse-keymap)))
    (viper-imm-defkey-l map "pe" 'viper-imm-pp-eval-last-sexp)
    (viper-imm-defkey-l map "pE" 'pp-eval-expression)
    (viper-imm-defkey-l map "pr" 'viper-imm-pp-eval-region)
    (viper-imm-defkey-l map "e" 'viper-imm-eval-last-sexp)
    (viper-imm-defkey-l map "r" 'viper-imm-eval-region)
    (viper-imm-defkey-l map "k" 'eval-buffer)
    (viper-imm-defkey-l map "K" 'eval-buffer)
    (viper-imm-defkey-l map "da" 'apropos)
    (viper-imm-defkey-l map "df" 'describe-function)
    (viper-imm-defkey-l map "dv" 'describe-variable)
    (viper-imm-defkey-l map "E"  'eval-expression)
    (viper-imm-defkey-l map "m" 'viper-imm-macroexpand)
    (viper-imm-defkey-l map "M" 'viper-imm-macroexpand-all)
    (viper-imm-defkey-l map "B" 'byte-compile-file)
    map))

(when viper-imm-lisp-bindings
  (viper-modify-major-mode 'emacs-lisp-mode
                           'vi-state
                           viper-imm-emacs-lisp-mode-vi-map)
  (viper-modify-major-mode 'lisp-interaction-mode
                           'vi-state
                           viper-imm-emacs-lisp-mode-vi-map))

;;;; Common Lisp Mode (Slime)

(defcustom viper-imm-slime-bindings t
  "Common Lisp Slime bindings."
  :type  'boolean
  :group 'viper-in-more-modes)

;;; Commands

(def-simple-viper-imm-wrapper-ocf viper-imm-slime-compile-defun ()
  (slime-compile-defun))
(def-simple-viper-imm-wrapper-ocf viper-imm-slime-eval-defun ()
  (slime-eval-defun))
(def-simple-viper-imm-wrapper-ocf viper-imm-slime-eval-last-expression ()
  (slime-eval-last-expression))
(def-simple-viper-imm-wrapper-ocf viper-imm-slime-pprint-eval-last-expression
  ()
  (slime-pprint-eval-last-expression))

(defun viper-imm-slime-eval-region ()
  (interactive)
  (if (and (boundp 'vimpulse-visual-mode)
           (not vimpulse-visual-mode))
      (error "Select the region in Visual mode.")
    (slime-eval-region (min (mark) (point)) (max (mark) (point)))
    (vimpulse-visual-mode 'toggle)))

;;; Bindings

;; In general, the Viper Slime mappings are much the same as regular
;; Slime bindings. The C-c and C-x prefixes are dropped. When commands
;; are similar, we use a lower case letter for the C-<key> case and an
;; upper case letter for the M-<key>, such as:
;;
;; C-c C-k : slime-compile-and-load-file : (vip-slime-leader k)
;; C-c M-k : slime-compile-file          : (vip-slime-leader K)
;;
;; All commands begin with vip-slime-leader, which defaults to
;; <space>. The M-x commands are not mapped, as they are presumably
;; rare. Some keys are a triple key sequence. The second key is a
;; marker for a category the third key is the activation key.
(defvar viper-imm-lisp-mode-vi-map
  (let ((map (make-sparse-keymap)))
    ;; Compilation commands
    (viper-imm-defkey-l map "k" 'slime-compile-and-load-file)
    (viper-imm-defkey-l map "K" 'slime-compile-file)
    (viper-imm-defkey-l map "c" 'viper-imm-slime-compile-defun)
    (viper-imm-defkey-l map "C" 'slime-remove-notes)
    ;; Finding definitions
    (viper-imm-defkey-l map "." 'slime-edit-definition)
    (viper-imm-defkey-l map "," 'slime-pop-find-definition-stack)
    ;; Note handling has the same binding as Slime defaults
    (viper-imm-defkey-l map "\M-n" 'slime-next-note)
    (viper-imm-defkey-l map "\M-p" 'slime-previous-note)
    ;; Lisp evaluation
    (viper-imm-defkey-l map "x" 'viper-imm-slime-eval-defun)
    (viper-imm-defkey-l map "e" 'viper-imm-slime-eval-last-expression)
    (viper-imm-defkey-l map "p" 'viper-imm-slime-pprint-eval-last-expression)
    (viper-imm-defkey-l             ; watch for visual mode!
     map "r" 'viper-imm-slime-eval-region)
    ;; Lisp documentation: 3 key sequences
    (viper-imm-defkey-l map "dd" 'slime-describe-symbol)
    (viper-imm-defkey-l map "da" 'slime-apropos)
    (viper-imm-defkey-l map "dz" 'slime-apropos-all)
    (viper-imm-defkey-l map "dp" 'slime-apropos-package)
    (viper-imm-defkey-l map "dh" 'slime-hyperspec-lookup)
    (viper-imm-defkey-l map "d~" 'common-lisp-hyperspec-format)
    ;; Macro expansion
    (viper-imm-defkey-l map "m" 'slime-macroexpand-1)
    (viper-imm-defkey-l map "M" 'slime-macroexpand-all)
    (viper-imm-defkey-l map "t" 'slime-toggle-trace-fdefinition)
    ;; Disassembly
    (viper-imm-defkey-l map "D" 'slime-disassemble-symbol)
    ;; Abort/recovery
    (viper-imm-defkey-l map "b" 'slime-interrupt)
    (viper-imm-defkey-l map "~" 'slime-sync-package-and-default-directory)
    (viper-imm-defkey-l map "P" 'slime-repl-set-package)
    ;; Cross-reference: 3 key sequences
    (viper-imm-defkey-l map "wc" 'slime-who-calls)
    (viper-imm-defkey-l map "wr" 'slime-who-references)
    (viper-imm-defkey-l map "wb" 'slime-who-binds)
    (viper-imm-defkey-l map "ws" 'slime-who-sets)
    (viper-imm-defkey-l map "wm" 'slime-who-macroexpands)
    (viper-imm-defkey-l map "<" 'slime-list-callers)
    (viper-imm-defkey-l map ">" 'slime-list-callees)
    ;; Inspector
    (viper-imm-defkey-l map "i" 'slime-inspect)
    ;; Repl!
    (viper-imm-defkey-l map "R" 'slime-switch-to-output-buffer)
    (viper-imm-defkey-l map "z" 'slime-switch-to-output-buffer)
    (viper-imm-defkey-l map "s" 'slime-scratch)
    ;; Profiler. "p" is already taken as a key, so we use "f" to
    ;; access the profiler functions
    (viper-imm-defkey-l map "ft" 'slime-toggle-profile-fdefinition)
    (viper-imm-defkey-l map "fp" 'slime-profile-package)
    (viper-imm-defkey-l map "fu" 'slime-unprofile-all)
    (viper-imm-defkey-l map "fr" 'slime-profile-report)
    (viper-imm-defkey-l map "fR" 'slime-profile-reset)
    map))

;;;; Slime REPL Mode (Slime)

;;; Commands

;; In normal mode, take search regex starting from the correct point.
(def-simple-viper-imm-wrapper-ocf viper-imm-ocf-slime-repl-next-input ()
  (interactive)
  (slime-repl-next-input))
(def-simple-viper-imm-wrapper-ocf viper-imm-ocf-slime-repl-previous-input ()
  (interactive)
  (slime-repl-previous-input))

;;; Bindings

(defvar viper-imm-slime-repl-mode-vi-map
  (let ((map (copy-keymap viper-imm-lisp-mode-vi-map)))
    (viper-imm-defkey-l map "n" 'viper-imm-ocf-slime-repl-next-input)
    (viper-imm-defkey-l map "p" 'viper-imm-ocf-slime-repl-previous-input)
    (viper-imm-defkey-l map "dG" 'slime-repl-clear-buffer)
    (define-key map "\C-n" 'viper-imm-ocf-slime-repl-next-input)
    (define-key map "\C-p" 'viper-imm-ocf-slime-repl-previous-input)
    (define-key map (kbd "RET") 'slime-repl-closing-return)
    map))

(defvar viper-imm-slime-repl-mode-insert-map
  (let ((map (make-sparse-keymap)))
    ;; When Emacs is running from the console, the basic keys below
    ;; don't seem to work in insert-mode unless redefined here.
    (define-key map (kbd "RET") 'slime-repl-closing-return)
    (define-key map "\C-n" 'slime-repl-next-input)
    (define-key map "\C-p" 'slime-repl-previous-input)
    map))

;;;; Slime REPL Mode (Slime)

(defvar viper-imm-sldb-mode-vi-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'sldb-default-action)
    (define-key map "q" 'sldb-quit)
    (viper-imm-defkey-l map "q" 'sldb-quit)
    (viper-imm-defkey-l map "0" 'sldb-invoke-restart-0)
    (viper-imm-defkey-l map "1" 'sldb-invoke-restart-1)
    (viper-imm-defkey-l map "2" 'sldb-invoke-restart-2)
    (viper-imm-defkey-l map "3" 'sldb-invoke-restart-3)
    (viper-imm-defkey-l map "4" 'sldb-invoke-restart-4)
    (viper-imm-defkey-l map "5" 'sldb-invoke-restart-5)
    (viper-imm-defkey-l map "6" 'sldb-invoke-restart-6)
    (viper-imm-defkey-l map "7" 'sldb-invoke-restart-7)
    (viper-imm-defkey-l map "8" 'sldb-invoke-restart-8)
    (viper-imm-defkey-l map "9" 'sldb-invoke-restart-9)
    (viper-imm-defkey-l map "a" 'sldb-abort)
    (viper-imm-defkey-l map "c" 'sldb-continue)
    (viper-imm-defkey-l map "v" 'sldb-show-source)
    map))

(when viper-imm-slime-bindings
  (viper-modify-major-mode 'lisp-mode
                           'vi-state
                           viper-imm-lisp-mode-vi-map)
  (viper-modify-major-mode 'slime-repl-mode
                           'vi-state
                           viper-imm-slime-repl-mode-vi-map)
  (viper-modify-major-mode 'slime-repl-mode
                           'insert-state
                           viper-imm-slime-repl-mode-insert-map)
  (viper-modify-major-mode 'sldb-mode
                           'vi-state
                           viper-imm-sldb-mode-vi-map))

;;;; LaTeX Mode (AUCTeX)

(defcustom viper-imm-latex-bindings t
  "LaTeX bindings."
  :type  'boolean
  :group 'viper-in-more-modes)


(defvar viper-imm-LaTeX-mode-vi-map
  (let ((map (make-sparse-keymap)))
    (viper-imm-defkey-l map "pp" 'preview-at-point)
    (viper-imm-defkey-l map "pw" 'preview-copy-region-as-mml)
    (viper-imm-defkey-l map "pe" 'preview-environment)
    (viper-imm-defkey-l map "ps" 'preview-section)
    (viper-imm-defkey-l map "pr" 'preview-region)
    (viper-imm-defkey-l map "pb" 'preview-buffer)
    (viper-imm-defkey-l map "pd" 'preview-document)
    (viper-imm-defkey-l map "Pp" 'preview-clearout-at-point)
    (viper-imm-defkey-l map "Ps" 'preview-clearout-section)
    (viper-imm-defkey-l map "Pr" 'preview-clearout-region)
    (viper-imm-defkey-l map "Pb" 'preview-clearout-buffer)
    (viper-imm-defkey-l map "Pd" 'preview-clearout-document)
    (viper-imm-defkey-l map "pf" 'preview-cache-preamble)
    (viper-imm-defkey-l map "Pf" 'preview-cache-preamble-off)
    (viper-imm-defkey-l map "pi" 'preview-goto-info-page)
    (viper-imm-defkey-l map "p?" 'preview-goto-info-page)
    (viper-imm-defkey-l map "%" 'TeX-comment-or-uncomment-paragraph)
    (viper-imm-defkey-l map ";" 'TeX-comment-or-uncomment-region)
    (viper-imm-defkey-l map "?" 'TeX-doc)
    (viper-imm-defkey-l map "e" 'LaTeX-environment)
    (viper-imm-defkey-l map "]" 'LaTeX-close-environment)
    (viper-imm-defkey-l map "k" 'TeX-kill-job)
    (viper-imm-defkey-l map "l" 'TeX-recenter-output-buffer)
    (viper-imm-defkey-l map "^" 'TeX-home-buffer)
    (viper-imm-defkey-l map "_" 'TeX-master-file-ask)
    (viper-imm-defkey-l map "d" 'TeX-save-document)
    (viper-imm-defkey-l map "`" 'TeX-next-error)
    (viper-imm-defkey-l map "tb" 'TeX-toggle-debug-bad-boxes)
    (viper-imm-defkey-l map "tw" 'TeX-toggle-debug-warnings)
    (viper-imm-defkey-l map "c" 'TeX-command-master)
    (viper-imm-defkey-l map "r" 'TeX-command-region)
    (viper-imm-defkey-l map "tr" 'TeX-pin-region)
    (viper-imm-defkey-l map "b" 'TeX-command-buffer)
    (viper-imm-defkey-l map "tp" 'TeX-PDF-mode)
    (viper-imm-defkey-l map "ti" 'TeX-interactive-mode)
    (viper-imm-defkey-l map "ts" 'TeX-source-specials-mode)
    (viper-imm-defkey-l map "to" 'TeX-Omega-mode)
    ;; TODO: try to find a way to define "fb" and similar
    (viper-imm-defkey-l map "f" 'TeX-font)
    (viper-imm-defkey-l map "N" 'TeX-normal-mode)
    (viper-imm-defkey-l map "ob" 'TeX-fold-buffer)
    (viper-imm-defkey-l map "Ob" 'TeX-fold-clearout-buffer)
    (viper-imm-defkey-l map "or" 'TeX-fold-region)
    (viper-imm-defkey-l map "Or" 'TeX-fold-clearout-region)
    (viper-imm-defkey-l map "op" 'TeX-fold-paragraph)
    (viper-imm-defkey-l map "Op" 'TeX-fold-clearout-paragraph)
    (viper-imm-defkey-l map "om" 'TeX-fold-macro)
    (viper-imm-defkey-l map "oe" 'TeX-fold-env)
    (viper-imm-defkey-l map "oc" 'TeX-fold-comment)
    (viper-imm-defkey-l map "Oi" 'TeX-fold-clearout-item)
    (viper-imm-defkey-l map "oo" 'TeX-fold-dwim)
    (viper-imm-defkey-l map "qe" 'TeX-fill-environment)
    (viper-imm-defkey-l map "qp" 'TeX-fill-paragraph)
    (viper-imm-defkey-l map "qr" 'TeX-fill-region)
    (viper-imm-defkey-l map "qs" 'TeX-fill-section)
    (viper-imm-defkey-l map "Q" 'TeX-fill-paragraph)
    (viper-imm-defkey-l map "s" 'LaTeX-section)
    (viper-imm-defkey-l map "v" 'TeX-view)
    (viper-imm-defkey-l map "i" 'LaTeX-insert-item)
    (viper-imm-defkey-l map "qr" 'TeX-fill-region)
    (viper-imm-defkey-l map "qs" 'TeX-fill-section)
    (viper-imm-defkey-l map "Q" 'TeX-fill-paragraph)
    (viper-imm-defkey-l map "s" 'LaTeX-section)
    (viper-imm-defkey-l map "~" 'LaTeX-math-mode)
    (viper-imm-defkey-l map "&" 'reftex-view-crossref)
    (viper-imm-defkey-l map "(" 'reftex-label)
    (viper-imm-defkey-l map ")" 'reftex-reference)
    (viper-imm-defkey-l map "-" 'reftex-toc-recenter)
    (viper-imm-defkey-l map "/" 'reftex-index-selection-or-word)
    (viper-imm-defkey-l map "<" 'reftex-index)
    (viper-imm-defkey-l map "=" 'reftex-toc)
    (viper-imm-defkey-l map ">" 'reftex-display-index)
    (viper-imm-defkey-l map "[" 'reftex-citation)
    (viper-imm-defkey-l map "\\" 'reftex-index-phrase)
    (viper-imm-defkey-l map "|" 'reftex-index-visit-phrases-buffer)
    map))

(when viper-imm-latex-bindings
  (viper-modify-major-mode 'LaTeX-mode
                           'vi-state
                           viper-imm-LaTeX-mode-vi-map)
  (viper-modify-major-mode 'latex-mode
                           'vi-state
                           viper-imm-LaTeX-mode-vi-map))

;;;; Haskell Mode

(defcustom viper-imm-haskell-bindings t
  "Haskell bindings."
  :type  'boolean
  :group 'viper-in-more-modes)

(defvar viper-imm-haskell-mode-vi-map
  (let ((map (make-sparse-keymap)))
    (viper-imm-defkey-l map "t" 'inferior-haskell-type)
    ;; (viper-imm-defkey-l map "T" 'inferior-haskell-insert-type)
    (viper-imm-defkey-l map "i" 'inferior-haskell-info)
    (viper-imm-defkey-l map "h" 'haskell-hoogle)
    (viper-imm-defkey-l map "l" 'inferior-haskell-load-file)
    (viper-imm-defkey-l map "d" 'inferior-haskell-find-haddock)
    (viper-imm-defkey-l map "?" 'inferior-haskell-find-haddock)
    (viper-imm-defkey-l map "=" 'haskell-indent-insert-equal)
    (viper-imm-defkey-l map "|" 'haskell-indent-insert-guard)
    (viper-imm-defkey-l map "o" 'haskell-indent-insert-otherwise)
    (viper-imm-defkey-l map "w" 'haskell-indent-insert-where)
    (viper-imm-defkey-l map "." 'haskell-indent-align-guards-and-rhs)
    (viper-imm-defkey-l map "z" 'switch-to-haskell)
    (viper-imm-defkey-l map ">" 'haskell-indent-put-region-in-literate)
    (viper-imm-defkey-l map "\M-." 'inferior-haskell-find-definition)
    map))

(when viper-imm-haskell-bindings
  (viper-modify-major-mode 'haskell-mode
                           'vi-state
                           viper-imm-haskell-mode-vi-map))

;;;; Prolog Mode

(defcustom viper-imm-prolog-bindings t
  "Prolog bindings."
  :type  'boolean
  :group 'viper-in-more-modes)

(defvar viper-imm-prolog-mode-vi-map
  (let ((map (make-sparse-keymap)))
    (viper-imm-defkey-l map "l" 'inferior-prolog-load-file)
    (viper-imm-defkey-l map "c" 'inferior-prolog-load-file)
    (viper-imm-defkey-l map "z" 'switch-to-prolog)
    map))

(when viper-imm-prolog-bindings
  (viper-modify-major-mode 'prolog-mode
                           'vi-state
                           viper-imm-prolog-mode-vi-map))

;;;; Ruby Mode

(defcustom viper-imm-ruby-bindings t
  "Ruby bindings."
  :type  'boolean
  :group 'viper-in-more-modes)

(defvar viper-imm-ruby-mode-vi-map
  (let ((map (make-sparse-keymap)))
    (viper-imm-defkey-l map "l" 'ruby-load-file)
    (viper-imm-defkey-l map "r" 'ruby-send-region)
    (viper-imm-defkey-l map "R" 'ruby-send-region-and-go)
    (viper-imm-defkey-l map "b" 'ruby-send-block)
    (viper-imm-defkey-l map "B" 'ruby-send-block-and-go)
    (viper-imm-defkey-l map "x" 'ruby-send-definition)
    (viper-imm-defkey-l map "X" 'ruby-send-definition-and-go)
    (viper-imm-defkey-l map "z" 'switch-to-ruby)
    (viper-imm-defkey-l map "l" 'ruby-load-file)
    (viper-imm-defkey-l map "s" 'run-ruby)
    map))

(defvar viper-imm-inferior-ruby-mode-vi-map
  (let ((map (make-sparse-keymap)))
    (viper-imm-defkey-l map "l" 'ruby-load-file)
    map))

(when viper-imm-ruby-bindings
  (viper-modify-major-mode 'ruby-mode
                           'vi-state
                           viper-imm-ruby-mode-vi-map)
  (viper-modify-major-mode 'inferior-ruby-mode
                           'vi-state
                           viper-imm-inferior-ruby-mode-vi-map))

;;; }}} End major mode keybinding code

(provide 'viper-in-more-modes)

;;; viper-in-more-modes.el ends here

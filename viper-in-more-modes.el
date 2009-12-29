;;; viper-in-more-modes.el --- vi-like keybindings for various Emacs modes

;; Copyright (C) 2007 Alessandro Piras, Brad Beveridge, Jason Spiro
;; 
;; Version: 0.1.2
;; Authors: Alessandro Piras <laynor@gmail.com>,
;;          Brad Beveridge <brad.beveridge@gmail.com>
;; Previous maintainer: Jason Spiro <http://www.jspiro.com>
;; Maintainer: No maintainer anymore. Got a patch? See below.
;; URL: http://emacswiki.org/elisp/viper-in-more-modes.el
;; 
;; This file is not part of GNU Emacs.

;; This code is unmaintained.  Subscribe to 
;; http://lists.ourproject.org/cgi-bin/mailman/listinfo/implementations-list
;; and if you're lucky, a committer will commit your patch for you.
;; If you would like to step up as new maintainer, email implementations-list and a committer will gladly anoint you.

;;; Commentary:
;; 
;; This file is an unofficial add-on to viper-mode.  It currently
;; provides vi-like keymaps for emacs-lisp-mode, lisp-interaction-mode,
;; slime-repl-mode, and lisp-mode.  If you have any questions or
;; comments, please email the authors and maintainer.  We provide no
;; guarantee of help with such early code.  If you extended this file to
;; cover additional modes, we would be very grateful; please contact us.
;; 
;; There are no installation instructions or usage instructions, but
;; we might be able to help you out if you contact us.  If you wrote
;; such instructions and added them to this wiki page, we would be very
;; appreciative.
;; 
;; This is alpha-quality code.  If it works for you, we would
;; appreciate it if you let us know.

;;; TODO:
;;
;; * We shouldn't use the "vimper" function-name prefix anywhere in
;; this file.  The word "vimper" is a leftover from back when
;; viper-in-more-modes was part of Vimpulse, and Vimpulse was
;; unreleased and didn't have a good name yet.  But
;; viper-in-more-modes is no longer part of Vimpulse.  It doesn't
;; require Vimpulse to load.  It does provide access to some Vimpulse
;; features like visual mode, but viper-in-more-modes does something
;; different than Vimpulse and so isn't shipped with Vimpulse.  So we
;; should pick a new prefix.
;; 
;; * Rename viper-in-more-modes to something much shorter.  This will
;; not only give viper-in-more-modes a shorter name, but will also
;; instantly provide us with a better prefix.
;; 
;; * Clean up the code in general: for example, the error messages
;; shouldn't include exclamation marks.  And the grammar and
;; capitalization in the comments should be improved.  Also, the boxed
;; comments probably don't have to be in boxes.
;; 
;; * Submit it to M-x report-emacs-bug and ask them to please include
;; viper-in-more-modes as part of Viper.  If we can't reach some
;; contributors for copyright assignment, we'll probably have to
;; discard their contributions at this time, so we should probably
;; try to get all contributors' mailing addresses and phone numbers
;; as soon as they've contributed fifteen lines or more, in case
;; they later disappear.

;;; Change Log:
;; 
;; Version 0.1.2:  Removed some duplicate keybinding code.  Also, 
;; slime-list-callees is now on the ">" key instead of on the "<" key
;; which was already taken. Thank you Stephen Bach <sjbach at comcast.net>.
;; Version 0.1.1:  Made viper-leader-char a var, not a const.  Thank you
;; John J Foerch <jjfoerch at earthlink.net>.
;; Version 0.1:  Initial upload to wiki.

;;; License:
;; 
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place - Suite 330, Boston, MA 02111-1307, USA.



;; Begin utility code {{{

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macro workaround to make some commands ;;;
;;; work on the character the cursor is on ;;;
;;; too (eg. in visual mode pressing "d"   ;;;
;;; deletes also the char under the cursor ;;;
;;; (like in vim)			   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro do-one-char-forward (&rest body)
  "Wraps the body between `forward-char' and `backward-char' to make commands
work on closed parens like one can expect in vi."
  `(progn
     (forward-char)
     ,@body
     (backward-char)))

(defmacro def-simple-vimper-wrapper-ocf (name &rest body)
  ";     Define a wrapper for a command to execute it as if the cursor was one
   ; char forward the current position. Uses `do-one-char-forward'. Use it
   ; like a defun without lambdalist.
   ;
   ;     For example, this:
   ;
   ; (def-simple-vimper-wrapper-ocf my-eval-last-sexp (eval-last-sexp))
   ;
   ;     expands to this:
   ;
   ; (defun my-eval-last-sexp ()
   ;   (interactive)
   ;   (do-one-char-forward
   ;    (eval-last-sexp)))"
  `(defun ,name ()
     (interactive)
     (do-one-char-forward
      ,@body)))

;; }}} End utility code



;; Begin major mode keybinding code {{{

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Major mode keybindings and functions used by vimper  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar viper-leader-char " ")
(defmacro vimper-defkey-l (map key func)
  `(define-key ,map (concat viper-leader-char ,key) ,func))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     Emacs Lisp Mode - Viper Mappings       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commands definitions (Almost all are work-arounds due to the fact that
;;; 	    Emacs wants the cursor to be _after_ the ")" to execute functions
;;;	    on sexps. We use the `do-one-char-forward' utility macro here;
;;;	    the macro is defined above.
(defun vimper-eval-last-sexp (&optional eval-last-sexp-arg-internal)
  (interactive)
  (do-one-char-forward (eval-last-sexp eval-last-sexp-arg-internal)))

(defun vimper-eval-region (&optional arg)
  (interactive "P")
  (if (not viper-visual-mode)
      (message "Select the region in Visual Mode!")
    (eval-region (min (mark) (point)) (max (mark) (point)))
    (viper-visual-mode 'toggle)))
(defun vimper-pp-eval-region ()
  (interactive)
  (message (pp-to-string (vimper-eval-region))))
(defun vimper-pp-eval-last-sexp (&optional eval-last-sexp-arg-internal)
  (interactive)
  (do-one-char-forward (pp-eval-last-sexp eval-last-sexp-arg-internal)))
;; macroexpand command (macroexpands last sexp)
(def-simple-vimper-wrapper-ocf vimper-macroexpand
  (message (pp-to-string (macroexpand (sexp-at-point)))))
;; macroexpand-all command (macroexpands-all last sexp)
(def-simple-vimper-wrapper-ocf vimper-macroexpand-all
  (message (pp-to-string (macroexpand-all (sexp-at-point)))))
		    
;;; bindings
(setq my-elisp-modified-vi-map
      (let ((map (make-sparse-keymap)))
	(vimper-defkey-l map "p" (make-sparse-keymap))
	(vimper-defkey-l map "pe" 'vimper-pp-eval-last-sexp)
	(vimper-defkey-l map "pE" 'pp-eval-expression)
	(vimper-defkey-l map "pr" 'vimper-pp-eval-region)
	(vimper-defkey-l map "e" 'vimper-eval-last-sexp)
	(vimper-defkey-l map "r" 'vimper-eval-region)
	(vimper-defkey-l map "da" 'apropos)
	(vimper-defkey-l map "df" 'describe-function)
	(vimper-defkey-l map "dv" 'describe-variable)
	(vimper-defkey-l map "E"  'eval-expression)
	(vimper-defkey-l map "m" 'vimper-macroexpand)
	(vimper-defkey-l map "M" 'vimper-macroexpand-all)
	(vimper-defkey-l map "B" 'byte-compile-file)
	map))
(viper-modify-major-mode 'emacs-lisp-mode 'vi-state my-elisp-modified-vi-map)
(viper-modify-major-mode 'lisp-interaction-mode 'vi-state my-elisp-modified-vi-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     Common Lisp Mode - Viper Mappings      ;;;
;;;                    Slime                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Commands
(def-simple-vimper-wrapper-ocf vimper-slime-compile-defun 
   (slime-compile-defun))
(def-simple-vimper-wrapper-ocf vimper-slime-eval-defun 
   (slime-eval-defun))
(def-simple-vimper-wrapper-ocf vimper-slime-eval-last-expression 
   (slime-eval-last-expression))
(def-simple-vimper-wrapper-ocf vimper-slime-pprint-eval-last-expression
   (slime-pprint-eval-last-expression))
(defun vimper-slime-eval-region ()
  (interactive)
  (if (not viper-visual-mode)
      (message "Select the region in Visual Mode!")
    (slime-eval-region (min (mark) (point)) (max (mark) (point)))
    (viper-visual-mode 'toggle)))
(defun vimper-slime-macroexpand-1 (&optional REPEATEDLY)
  (interactive "P")
  (do-one-char-forward
   (slime-macroexpand-1 REPEATEDLY)))
(def-simple-vimper-wrapper-ocf vimper-slime-macroexpand-all
  (slime-macroexpand-all))
;; Bindings

;; In general the Viper Slime mappings are much the same as regular Slime bindings,
;; The C-c and C-x prefixes are dropped.
;; When commands are similar, we use a lower case letter for the C-<key> case and an upper
;; case letter for the M-<key>, such as:
;; C-c C-k : slime-compile-and-load-file  : (vip-slime-leader k)
;; C-c M-k : slime-compile-file           : (vip-slime-leader K)
;; All commands begin with vip-slime-leader, which defaults to <space>
;; the M-x commands are not mapped, as they are presumably rare
;; Some keys are a triple key sequence.  The second key is a marker for a category
;; the third key is the activation key.

(setq my-lisp-modified-vi-map (let ((map (make-sparse-keymap)))
				;; Compilation Commands
				(vimper-defkey-l map "k" 'slime-compile-and-load-file)
				(vimper-defkey-l map "K" 'slime-compile-file)
				(vimper-defkey-l map "c" 'vimper-slime-compile-defun)
				(vimper-defkey-l map "C" 'slime-remove-notes)

				;; Do I want to change those??? Meta key -_-''
				;; TODO: of course I want to change theese!! we're using
				;;       leader char here, so it definately sucks!!
				;; Finding definitions (they are same as Slime default)
				(vimper-defkey-l map "M-." 'slime-edit-definition)   
				(vimper-defkey-l map "M-," 'slime-pop-find-definition-stack)
				;; Note handling has the same binding as Slime defaults
				(vimper-defkey-l map "M-n" 'slime-next-note)
				(vimper-defkey-l map "M-p" 'slime-previous-note)

				;; Lisp Evaluation
				(vimper-defkey-l map "x" 'vimper-slime-eval-defun)
				(vimper-defkey-l map "e" 'vimper-slime-eval-last-expression)
				(vimper-defkey-l map "p" 'vimper-slime-pprint-eval-last-expression)
				(vimper-defkey-l map "r" 'vimper-slime-eval-region) ; watch for visual mode!!
				;; Lisp Documentation
				;; 3 key sequences
				(vimper-defkey-l map "dd" 'slime-describe-symbol) 
				(vimper-defkey-l map "da" 'slime-apropos) 
				(vimper-defkey-l map "dz" 'slime-apropos-all)
				(vimper-defkey-l map "dp" 'slime-apropos-package)
				(vimper-defkey-l map "dh" 'slime-hyperspec-lookup)
				(vimper-defkey-l map "d~" 'common-lisp-hyperspec-format)
				;; Macro expansion
				(vimper-defkey-l map "m" 'vimper-slime-macroexpand-1)
				(vimper-defkey-l map "M" 'vimper-slime-macroexpand-all)
				(vimper-defkey-l map "t" 'slime-toggle-trace-fdefinition)

				;; Disassembly
				(vimper-defkey-l map "D" 'slime-disassemble-symbol)

				;; Abort/Recovery
				(vimper-defkey-l map "b" 'slime-interrupt)
				(vimper-defkey-l map "~" 'slime-sync-package-and-default-directory)
				(vimper-defkey-l map "P" 'slime-repl-set-package)

				;; Cross-reference

				;; All cross-reference functions are
				;; triple key sequences
				;; (vip-slime-leader ?w key)
				(vimper-defkey-l map "wc" 'slime-who-calls)
				(vimper-defkey-l map "wr" 'slime-who-references)
				(vimper-defkey-l map "wb" 'slime-who-binds)
				(vimper-defkey-l map "ws" 'slime-who-sets)
				(vimper-defkey-l map "wm" 'slime-who-macroexpands)
				(vimper-defkey-l map "<" 'slime-list-callers)
				(vimper-defkey-l map ">" 'slime-list-callees)

				;; Inspector
				(vimper-defkey-l map "i" 'slime-inspect)

				;;Repl!
				(vimper-defkey-l map "R" 'slime-switch-to-output-buffer)
				(vimper-defkey-l map "z" 'slime-switch-to-output-buffer)

				;; Profiler
				;; "p" is already taken as a key, we
				;; use "f" to access the profiler functions
				(vimper-defkey-l map "f" (make-sparse-keymap))
				(vimper-defkey-l map "ft" 'slime-toggle-profile-fdefinition)
				(vimper-defkey-l map "fp" 'slime-profile-package)
				(vimper-defkey-l map "fu" 'slime-unprofile-all)
				(vimper-defkey-l map "fr" 'slime-profile-report)
				(vimper-defkey-l map "fR" 'slime-profile-reset)		
				map))

(viper-modify-major-mode 'lisp-mode 'vi-state my-lisp-modified-vi-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Slime Inspector Mode - Viper Mappings    ;;;
;;;                    Slime                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Slime   REPL    Mode - Viper Mappings    ;;;
;;;                    Slime                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq my-repl-modified-vi-map (let ((map (make-sparse-keymap)))
				(vimper-defkey-l map "m" 'vimper-slime-macroexpand-1)
				(vimper-defkey-l map "i" 'slime-inspect)
				(vimper-defkey-l map "<C-return>" 'slime-repl-closing-return)
				(vimper-defkey-l map "G" 'end-of-buffer)))
(viper-modify-major-mode 'slime-repl-mode 'vi-state my-repl-modified-vi-map)

(setq my-repl-modified-insert-map (let ((map (make-sparse-keymap)))
				    (vimper-defkey-l map "<C-return>"
						     'slime-repl-closing-return)))

(viper-modify-major-mode 'slime-repl-mode 'insert-state
			 my-repl-modified-insert-map)

;; }}} End major mode keybinding code



(provide 'viper-in-more-modes)

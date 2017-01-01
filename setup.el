;;; setup.el --- Startup assignments: hooks etc.
;;
;; Filename: setup.el
;; Description: Startup assignments: hooks etc.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2017, Drew Adams, all rights reserved.
;; Created: Thu Dec 28 09:15:00 1995
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Sun Jan  1 11:32:24 2017 (-0800)
;;           By: dradams
;;     Update #: 797
;; URL: http://www.emacswiki.org/setup.el
;; Keywords: internal, local
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `avoid', `fit-frame', `frame-cmds', `frame-fns', `header2',
;;   `hexrgb', `lib-requires', `loadhist', `misc-fns', `oneonone',
;;   `paren', `strings', `thingatpt', `thingatpt+', `zoom-frm'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;     Startup configuration (assignments etc.).
;;
;;  Think of this library more as an extension to your init file
;;  (~/.emacs) than as a true library.  It makes changes to your Emacs
;;  setup.  If you want only some of the changes that are made here,
;;  then either modify this file for your own use or load it and then
;;  modify selected parts of your setup afterward.
;;
;;  The most major setup change this library makes is to use library
;;  `oneonone.el', providing one-frame-per buffer.
;;
;;  Additional setup changes can be found in library `start-opt.el'.
;;
;;  In addition to the setup done in `setup.el' and `start-opt.el', I
;;  recommend that you turn on `icicle-mode' near the end or your init
;;  file, after other libraries might have defined key bindings.  Here
;;  is the code to do that:
;;
;;    (when (fboundp 'icicle-mode) (icicle-mode 1))
;;
;;  (That is done in library `emacs-init.el'.)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2015/09/27 dadams
;;     lisp-indentation-hack:
;;       Use common-lisp-indent-function from flet & labels for cl-flet & cl-labels.
;; 2015/03/02 dadams
;;     Turn on echo-bell-mode, if defined.
;; 2014/11/28 dadams
;;     Set cursor-in-non-selected-windows to box, by default.
;; 2014/01/26 dadams
;;     lisp-indentation-hack:
;;       For Emacs 22+, use load-history-filename-element, not assoc.
;; 2013/07/14 dadams
;;     Removed require of fit-frame.el.
;; 2011/08/19 dadams
;;     lisp-indentation-hack:
;;       Added icicle-with-selected-window, icicle-condition-case-no-debug.
;; 2011/07/25 dadams
;;     Use eval-after-load where appropriate (e.g. instead of featurep/fboundp/boundp).
;; 2009/04/18 dadams
;;     Don't invoke 1on1-emacs if no graphics display.
;; 2009/03/08 dadams
;;     Soft-require oneonone.el.  Protect 1on1-* with fboundp.
;; 2007/12/12 dadams
;;     update-file-header -> auto-update-file-header.
;; 2007/08/14 dadams
;;     Changed eq emacs 22 to > emacs 21.
;; 2007/02/08 dadams
;;     Turn on pretty-control-l-mode.
;; 2007/01/15 dadams
;;     lisp-indentation-hack: Added another Icicles macro.
;; 2007/01/01 dadams
;;     lisp-indentation-hack: Added Icicles macros that define stuff.
;; 2006/09/14 dadams
;;     Mode-line indication of region size: size-indication-mode.
;; 2006/09/08 dadams
;;     Turn off idle highlighting by default.  (Use crosshairs-mode instead.)
;; 2006/09/04 dadams
;;     Turn on idle highlighting, by default: current line, box cursor.
;; 2006/05/26 dadams
;;     Clarified Commentary.
;;     Moved to start-opt.el: show-paren-match-face.
;; 2005/10/21 dadams
;;     (add-hook 'write-file-hooks 'update-file-header)
;;     Require header2.el.
;; 2005/07/10 dadams
;;     Set view-remove-frame-by-deleting to non-nil.
;; 2005/07/02 dadams
;;     Added: lisp-indentation-hack.
;;     Added eldoc-mode, lisp-indentation-hack to hooks for Lisp modes.
;;     Added append argument to mode hooks for notify-user-of-mode.
;; 2005/05/17 dadams
;;     Updated to work with Emacs 22.x.
;; 2005/05/15 dadams
;;     Execute 1on1-emacs here.
;;     Set up same-window hook stuff here.
;;     Do w32-grab-focus here.
;; 2005/01/20 dadams
;;     Added kill-emacs-query-functions confirmation hook (replaces exit-with-confi...)
;; 2004/12/18 dadams
;;     No longer add fit-frame to after-make-frame-functions here.  Do
;;     it in start-opt.el instead, so it is easier to inhibit (undo)
;;     in user's init file.
;; 2004/11/24 dadams
;;     Turned on file-name-shadow-mode.
;;     (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
;; 2004/10/07 dadams
;;     Renamed resize-* to fit-*.
;; 2004/09/21 dadams
;;     Added (icomplete-mode 99).
;; 2004/06/01 dadams
;;     Renamed shrink-* to resize-*.
;; 1999/04/02 dadams
;;     1. Only add make-frame hooks if (featurep 'shrink-fit).
;;     2. Only add erase-nonempty-inactive-minibuffer as pre-command hook if defined.
;;     3. Only add notify-user-of-mode as hook if defined.
;; 1996/07/04 dadams
;;     Mode hooks: set imenu-create-index-function, depending on mode.
;; 1996/03/15 dadams
;;     Added shrink-frame-to-fit to after-make-frame-hook.
;; 1996/03/12 dadams
;;     Add hooks: before-make-frame-hook, after-make-frame-hook,
;;                pre-command-hook (erase-nonempty-inactive-minibuffer).
;; 1996/02/26 dadams
;;     Added require-final-newline.
;; 1996/02/06 dadams
;;     occur-mode-hook, list-options-hook: notify-user-of-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'header2 nil t) ;; (no error if not found): auto-update-file-header
(require 'misc-fns nil t) ;; (no error if not found): notify-user-of-mode
(require 'strings nil t) ;; (no error if not found): erase-nonempty-inactive-minibuffer
;; $$$$$(require 'fit-frame nil t) ;; (no error if not found): fit-frame
(require 'paren nil t) ;; (no error if not found):
                       ;; show-paren-match-face, show-paren-mode
(require 'oneonone nil t) ;; (no error if not found):
                              ;; 1on1-emacs, 1on1-toggle-box-cursor-when-idle

(defvar cursor-in-non-selected-windows) ; Emacs 22+
;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load "pp-c-l" '(pretty-control-l-mode 1)) ; Turn on pretty display of `^L'.

;; One-on-One Emacs - see `oneonone.el'.
(eval-after-load "oneonone"
  '(when (if (fboundp 'display-graphic-p) (display-graphic-p) window-system)
    (1on1-emacs)
    (1on1-toggle-box-cursor-when-idle 1))) ; Change cursor to box when Emacs is idle.

(setq view-remove-frame-by-deleting  t) ; Delete, don't iconify.

(cond ((> emacs-major-version 21)
       (remove-hook 'same-window-regexps "\\*info\\*\\(\\|<[0-9]+>\\)")
       (remove-hook 'same-window-regexps "\\`\\*Customiz.*\\*\\'"))
      ((< emacs-major-version 21)
       (remove-hook 'same-window-buffer-names "*info*"))
      (t                                ; E.g. Emacs 21.3.1
       (remove-hook 'same-window-buffer-names "*info*")
       (remove-hook 'same-window-regexps "\\`\\*Customiz.*\\*\\'")))

;; UNCOMMENT this if you want line/column highlighting when Emacs is idle.
;;
;; Highlighting of current line and column when Emacs is idle.
;; (cond ((fboundp 'toggle-crosshairs-when-idle) ; Defined in `crosshairs.el'.
;;        (toggle-crosshairs-when-idle 1)) ; only if `hl-line+.el' is loaded.
;;       ((fboundp 'toggle-highlight-column-when-idle) ; Defined in `col-highlight.el'.
;;        (toggle-highlight-column-when-idle))
;;       ((fboundp 'toggle-hl-line-when-idle) ; Defined in `hl-line+.el'.
;;        (toggle-hl-line-when-idle 1))) ; Highlight current line when idle.

(setq w32-grab-focus-on-raise      nil
      win32-grab-focus-on-raise    nil) ; older name

;; Ask for confirmation before quitting Emacs
(add-hook 'kill-emacs-query-functions
          (lambda () (y-or-n-p "Do you really want to exit Emacs? "))
          'append)

;; Update file headers when write files.
(eval-after-load "header2" '(add-hook 'write-file-hooks 'auto-update-file-header))

;; Make `transient-mark-mode' buffer local, so you can turn it on and
;; off in separate buffers.
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode  t)

(setq mark-even-if-inactive   t)        ; Be able to use mark even if it is inactive.
(setq highlight-nonselected-windows  t) ; Highlight region everywhere.

;; Don't allow accidental `M-~' to mark buffer as unmodified.
(put 'not-modified 'disabled t)

(make-variable-buffer-local 'require-final-newline)
(setq-default require-final-newline  99999) ; Ask if want newline at eof.

(eval-after-load "paren" '(show-paren-mode 999)) ; Show matching parentheses.

(when (boundp 'cursor-in-non-selected-windows)
  (setq-default cursor-in-non-selected-windows  'box))

;; This must be done *after* loading `paren.el'.
;(setq window-setup-hook                 ; Note: `setq', *not* `add-hook'.  This
;      (function                         ; is needed bc `paren.el' sets this to
;       (lambda () (when window-system   ; also nullify `blink-paren-function'.
;                    (add-hook 'post-command-hook 'show-paren-command-hook)))))

(setq buffers-menu-max-size  30)        ; =< 30 buffers (see `menu-bar.el').
(setq history-length  250)

;; Mode-line
(column-number-mode 1)
(when (fboundp 'size-indication-mode) (size-indication-mode 1)) ; Emacs 21 and 22.

(eval-after-load "icomplete+" '(icomplete-mode 99))

(when (fboundp 'echo-bell-mode) (echo-bell-mode 1)) ; Turn on visual bell in echo area.

;; Hide passwords
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

(when (fboundp 'file-name-shadow-mode) (file-name-shadow-mode 99)) ; Turn it on.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; ****** Begin stuff to do at the end ***********

(eval-after-load "strings"
  '(add-hook 'pre-command-hook 'erase-nonempty-inactive-minibuffer))

;; `notify-user-of-mode' is defined in `misc-fns.el'.
;; `imenu-*' functions are defined in `imenu.el'.
;(add-hook 'emacs-lisp-mode-hook
;          (function (lambda () (setq imenu-create-index-function
;                                     'imenu-example--create-lisp-index))))
;(add-hook 'emacs-lisp-mode-hook       'imenu-add-defs-to-search-menu)



;; This might not be needed for Emacs 22+ - dunno.
;;
(defun lisp-indentation-hack ()
  "Better Lisp indenting.  Use in Lisp mode hooks
such as `lisp-mode-hook', `emacs-lisp-mode-hook', and
`lisp-interaction-mode-hook'."
  (unless (if (fboundp 'load-history-regexp) ; Emacs 22+
              (load-history-filename-element (load-history-regexp "cl-indent"))
            (assoc "cl-indent" load-history))
    (load "cl-indent" nil t))
  (set (make-local-variable 'lisp-indent-function) 'common-lisp-indent-function)
  (setq lisp-indent-maximum-backtracking  10)
  (put 'define-derived-mode              'common-lisp-indent-function '(4 4 4 2 &body))
  (put 'if                               'common-lisp-indent-function '(nil nil &body))
  (put 'icicle-define-command              'common-lisp-indent-function '(4 &body))
  (put 'icicle-define-file-command         'common-lisp-indent-function '(4 &body))
  (put 'icicle-define-sort-command         'common-lisp-indent-function '(4 4 &body))
  (put 'icicle-define-add-to-alist-command 'common-lisp-indent-function '(4 &body))
  (put 'icicle-with-selected-window        'common-lisp-indent-function '(4 &body))
  (put 'icicle-condition-case-no-debug     'common-lisp-indent-function '(4 4 &body))
  (when (featurep 'cl-indent)
    (put 'cl-flet 'common-lisp-indent-function
         (get 'flet 'common-lisp-indent-function))
    (put 'cl-labels 'common-lisp-indent-function
         (get 'labels 'common-lisp-indent-function))))

(add-hook 'emacs-lisp-mode-hook 'lisp-indentation-hack)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode) ; Feedback on current function.

(add-hook 'lisp-mode-hook             'lisp-indentation-hack)
(add-hook 'lisp-interaction-mode-hook 'lisp-indentation-hack)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

(eval-after-load "misc-fns"
  '(progn
    (add-hook 'emacs-lisp-mode-hook       'notify-user-of-mode 'append)
    (add-hook 'lisp-mode-hook             'notify-user-of-mode 'append)
    (add-hook 'cmulisp-mode-hook          'notify-user-of-mode 'append)
    (add-hook 'ccl-mode-hook              'notify-user-of-mode 'append)
    (add-hook 'c-mode-hook                'notify-user-of-mode 'append)
    (add-hook 'c++-mode-hook              'notify-user-of-mode 'append)
    (add-hook 'hide-ifdef-mode-hook       'notify-user-of-mode 'append)
    (add-hook 'makefile-mode-hook         'notify-user-of-mode 'append)
    (add-hook 'fortran-mode-hook          'notify-user-of-mode 'append)
    (add-hook 'cmushell-mode-hook         'notify-user-of-mode 'append)
    (add-hook 'comint-mode-hook           'notify-user-of-mode 'append)
    (add-hook 'ps-mode-hook               'notify-user-of-mode 'append)
    (add-hook 'sh-mode-hook               'notify-user-of-mode 'append)
    (add-hook 'shell-mode-hook            'notify-user-of-mode 'append)
    (add-hook 'dired-after-readin-hook    'notify-user-of-mode 'append)
    (add-hook 'buffer-menu-mode-hook      'notify-user-of-mode 'append)
    (add-hook 'compilation-mode-hook      'notify-user-of-mode 'append)
    (add-hook 'electric-help-mode-hook    'notify-user-of-mode 'append)
    (add-hook 'calc-edit-mode-hook        'notify-user-of-mode 'append)
    (add-hook 'calc-mode-hook             'notify-user-of-mode 'append)
    (add-hook 'calc-trail-mode-hook       'notify-user-of-mode 'append)
    (add-hook 'fixed-width-text-mode-hook 'notify-user-of-mode 'append)
    (add-hook 'texinfo-mode-hook          'notify-user-of-mode 'append)
    (add-hook 'latex-mode-hook            'notify-user-of-mode 'append)
    (add-hook 'tex-mode-hook              'notify-user-of-mode 'append)
    (add-hook 'slitex-mode-hook           'notify-user-of-mode 'append)
    (add-hook 'plain-tex-mode-hook        'notify-user-of-mode 'append)
    (add-hook 'bibtex-mode-hook           'notify-user-of-mode 'append)
    (add-hook 'math-mode-hook             'notify-user-of-mode 'append)
    (add-hook 'mfe-mode-hook              'notify-user-of-mode 'append)
    (add-hook 'mh-letter-mode-hook        'notify-user-of-mode 'append)
    (add-hook 'mh-mode-hook               'notify-user-of-mode 'append)
    (add-hook 'sql-mode-hook              'notify-user-of-mode 'append)
    (add-hook 'terminal-mode-hook         'notify-user-of-mode 'append)
    (add-hook 'array-mode-hook            'notify-user-of-mode 'append)
    (add-hook 'asm-mode-hook              'notify-user-of-mode 'append)
    (add-hook 'awk-mode-hook              'notify-user-of-mode 'append)
    (add-hook 'change-log-mode-hook       'notify-user-of-mode 'append)
    (add-hook 'occur-mode-hook            'notify-user-of-mode 'append)
    ;;(add-hook 'text-mode-hook             'notify-user-of-mode)
    (add-hook 'list-options-hook          'notify-user-of-mode 'append)
    ;; (add-hook 'lisp-mode-hook
    ;;          (function (lambda () (setq  imenu-create-index-function
    ;;                                     'imenu-example--create-lisp-index))))
    ;; (add-hook 'lisp-mode-hook             'imenu-add-defs-to-search-menu)
    ;; (add-hook 'cmulisp-mode-hook
    ;;          (function (lambda () (setq  imenu-create-index-function
    ;;                                     'imenu-example--create-lisp-index))))
    ;; (add-hook 'cmulisp-mode-hook          'imenu-add-defs-to-search-menu)
    ;; (add-hook 'ccl-mode-hook
    ;;          (function (lambda () (setq  imenu-create-index-function
    ;;                                     'imenu-example--create-ccl-index))))
    ;; (add-hook 'ccl-mode-hook              'imenu-add-defs-to-search-menu)
    ;; (add-hook 'c-mode-hook
    ;;          (function (lambda () (setq  imenu-create-index-function
    ;;                                     'imenu-example--create-c-index))))
    ;; (add-hook 'c-mode-hook                'imenu-add-defs-to-search-menu)
    ;; (add-hook 'c++-mode-hook
    ;;          (function (lambda () (setq  imenu-create-index-function
    ;;                                     'imenu-example--create-c++-index))))
    ;; (add-hook 'c++-mode-hook              'imenu-add-defs-to-search-menu)
    ;; (add-hook 'hide-ifdef-mode-hook       'imenu-add-defs-to-search-menu)
    ;; (add-hook 'makefile-mode-hook         'imenu-add-defs-to-search-menu)
    ;; (add-hook 'fortran-mode-hook          'imenu-add-defs-to-search-menu)
    ))

;;; ****** End stuff to do at the end ***********

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup.el ends here

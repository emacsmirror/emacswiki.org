;;; start-opt.el --- Some customizations to be done at the end of startup.
;;
;; Filename: start-opt.el
;; Description: Customizations to be done at the end of startup.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1995-2017, Drew Adams, all rights reserved.
;; Created: Thu Dec 28 09:15:00 1995
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Sun Jan  1 11:37:48 2017 (-0800)
;;           By: dradams
;;     Update #: 2005
;; URL: http://www.emacswiki.org/start-opt.el
;; Keywords: local, init
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos+', `autofit-frame', `avoid', `chistory',
;;   `cl', `cus-theme', `doremi', `doremi-cmd', `doremi-frm',
;;   `easymenu', `eyedropper', `faces', `faces+', `fit-frame',
;;   `frame-cmds', `frame-fns', `header2', `help+20', `hexrgb',
;;   `highlight', `info', `info+20', `isearch+', `iso-transl',
;;   `lib-requires', `loadhist', `menu-bar', `menu-bar+',
;;   `misc-cmds', `misc-fns', `mouse', `mouse+', `mwheel', `naked',
;;   `pp', `pp+', `replace+', `ring', `second-sel', `setup-keys',
;;   `strings', `thingatpt', `thingatpt+', `unaccent',
;;   `w32browser-dlgopen', `wid-edit', `wid-edit+', `widget',
;;   `wimpy-del'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;     Some customizations to be done at the end of startup.
;;
;;  This library replaces some key bindings that might have been made
;;  when other libraries were loaded. It also sets some mode hooks and
;;  makes some parameter (variable settings.
;;
;;  The following bindings are made here for `command-history-map':
;;
;;    `e'        `electric-command-history'
;;    `m'        `repeat-matching-complex-command'
;;
;;  Bindings for the following commands are optionally replaced here
;;  by bindings to substitute commands:
;;
;;    `clipboard-kill-region', `delete-window', `eval-expression',
;;    `eval-last-sexp', `kill-buffer', `kill-region', `list-buffers',
;;    `query-replace', `save-buffers-kill-emacs',
;;
;;  Bindings that would try to modify buffers in these modes have been
;;  removed: Dired, Buffer-menu, Compilation.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2016/03/16 dadams
;;     Do not use latex-mode for *.log files.
;; 2015/03/26 dadams
;;     Added: turn-font-lock-off-then-on.  Use it for dired-after-readin-hook.
;; 2015/03/14 dadams
;;     Moved face settings to my custom-file (finally):
;;       highlight, mode(-)line, region, secondary-selection, show-paren-match(-face),
;;       font-lock-(string|constant|warning|function-name|keyword|builtin)-face, isearch.
;; 2014/05/14 dadams
;;     Added  fit-frame-if-one-window to temp-buffer-window-show-hook also (Emacs 24.4+).
;; 2013/12/17 dadams
;;     Turn off electric-indent-mode (Emacs 24.4 turns it on by default).
;; 2012/10/02 dadams
;;     Handle obsolescence of face modeline.
;; 2011/11/29 dadams
;;     Add *.bmk to auto-mode-alist for emacs-lisp-mode (for editing/viewing bookmarks files).
;; 2011/08/07 dadams
;;     ls-lisp: Use featurep, not eval-after-load: doesn't work otherwise for Emacs 20.
;;              Use MS-Windows, not Microsoft, as the value of ls-emulation.
;; 2011/07/25 dadams
;;     Moved here from start.el: Savehist settings, color-moccur settings.
;;     Use eval-after-load where appropriate (e.g. instead of featurep/fboundp/boundp).
;; 2010/08/07 dadams
;;     Removed - use Customize instead:
;;       bookmark-save-flag, global-font-lock-mode, font-lock-verbose, font-lock-maximum-size,
;;       apropos-do-all, Info-fontify-maximum-menu-size, isearch-resume-in-command-history,
;;       undo-limit, undo-strong-limit, gc-cons-threshold, list-directory-verbose-switches,
;;       default-major-mode, display-buffer-reuse-frames, view-remove-frame-by-deleting,
;;       display-time-24hr-format, list-command-history-max.
;;     Removed: (query-replace|search)-highlight, (regexp-)search-ring-max,
;;              dired-listing-switches, VC stuff.
;; 2010/07/21 dadams
;;     Set font-lock-builtin-face, to get back the default as it was prior to 23.2.
;; 2007/09/23 dadams
;;     Removed second arg to undefine-killer-commands.
;; 2006/12/11 dadams
;;     No longer (undefine-killer-commands compilation-mode-map (current-global-map)).
;; 2006/09/15 dadams
;;     Don't require tool-bar.el here.
;;     Protect call to tooltip-mode.
;; 2006/08/22 dadams
;;     Replaced sub-remove-window by sub-delete-windows-for.
;; 2006/08/18 dadams
;;     Don't use speedbar anymore.
;; 2006/05/26 dadams
;;     Moved here from setup.el: show-paren-match-face.
;; 2006/04/01 dadams
;;     Add speedbar hack for Emacs 22, to fit the frame after the buffer has been filled.
;; 2006/01/28 dadams
;;     Multiplied (regexp-)search-ring-max by 100.
;;     (setq isearch-resume-in-command-history t).
;; 2006/01/20 dadams
;;     Make tooltips use the echo area.
;; 2006/01/01 dadams
;;     isearch face.
;; 2005/12/30 dadams
;;     Redefine list-matching-lines-face here (previously done in replace+.el).
;;     Set Info-fontify-maximum-menu-size here (previously done in setup-info.el).
;;     Removed require of def-face-const.el.
;;     Removed definitions of red and blue foreground faces.
;; 2005/11/22 dadams
;;     Moved mode-line coloring to oneonone.el.
;; 2005/10/23 dadams
;;     Doubled font-lock-maximum-size.
;; 2005/10/21 dadams
;;     Protected add-hook of auto-make-header with fboundp.
;; 2005/07/17 dadams
;;     Explicitly set mode-line-inactive.
;; 2005/07/07 dadams
;;     Added ls-lisp stuff.
;; 2005/05/17 dadams
;;     Don't use images for speedbar.
;;     Updated to work with Emacs 22.x.
;; 2005/05/15 dadams
;;     Set frame-title-format here.
;; 2005/05/10 dadams
;;     No longer turn on customize-toggle-outside-change-updates - do it in .emacs.
;; 2005/01/25 dadams
;;     Turn on automatic updating of Customize, so it takes into account
;;         changes made outside Customize.
;;     Use speedbar.
;; 2005/01/20 dadams
;;     Removed: References to exit-with-confirmation and sub-exit-with-confirmation.
;; 2004/12/18 dadams
;;     Moved here from setup.el: (add-hook 'after-make-frame-functions 'fit-frame)
;; 2004/11/?? dadams
;;     Require tool-bar+.el and use tool-bar-pop-up-mode.
;;     Set display-buffer-reuse-frames, display-buffer-reuse-frame, and
;;         view-remove-frame-by-deleting.
;; 2004/09/21 dadams
;;     Added (icomplete-mode 99).
;; 2000/11/01 dadams
;;     Added add-hooks for idl-mode-hook.
;; 2000/09/27 dadams
;;     Added  fit-frame-if-one-window as temp-buffer-show-hook.
;; 1999/09/03 dadams
;;     Define red- and blue-face-foreground via define-face-const.
;; 1999/09/02 dadams
;;     Protected substitute-key-definition with fboundp and global vars.
;;     Added substitute-key-definition for query-replace-w-options, buffer-menu.
;; 1999/08/30 dadams
;;     Added more c++-mode suffixes (lxx, ixx, gxx, pxx to auto-mode-alist.
;; 1999/08/30 dadams
;;     Added: c-mode-common-hook.
;; 1999/08/25 dadams
;;     Removed F option from dired-listing-switches when Windows NT.
;; 1999/04/02 dadams
;;     Added standard face redefinitions, new faces: red & blue foreground.
;; 1999/04/01 dadams
;;     Added: eval-after-load "ediff" -> set faces.
;; 1999/04/01 dadams
;;     Added global-font-lock-mode.
;; 1999/03/17 dadams
;;     1. Added: ediff-grab-mouse.
;;     2. gc-cons-threshold: 10 x -> 4 x the default.
;; 1996/06/25 dadams
;;     Assigned hook outline-mode-hook.
;; 1996/06/20 dadams
;;     Turn on auto-fill for slitex-mode-hook.
;; 1996/04/23 dadams
;;     Do undefine-killer-commands.
;; 1996/04/12 dadams
;;     1. Increased list-command-history-max.
;;     2. Bound keys in command-history-hook.
;;     3. Globally bound repeat-matching-complex-command.
;; 1996/02/06 dadams
;;     1. Put variable-interactive property on some user variables.
;;     2. Added font-lock-fontify-buffer to list-options-hook.
;; 1996/01/30 dadams
;;     Turn on font lock for makefile and sh-mode modes (hooks).
;; 1996/01/09 dadams
;;     Do again, here: substitute-key-def of kill-buffer, save-buffers-kill-emacs.
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

(require 'chistory) ;; command-history-map
(require 'header2 nil t) ;; (no error if not found): auto-make-header
(require 'misc-cmds nil t) ;; (no error if not found): kill-buffer-and-its-windows
(require 'wimpy-del nil t) ;; (no error if not found): kill-region-wimpy
(require 'setup-keys nil t) ;; (no error if not found):  sub-kill-buffer-and-its-windows,
                            ;; sub-pp-evals, sub-query-replace-w-options, sub-delete-windows-for
(require 'misc-fns nil t) ;; (no error if not found): undefine-killer-commands
(require 'faces+ nil t) ;; (no error if not found): make-face
                        ;; `faces+.el' requires `faces.el': set-face-foreground
(require 'frame-cmds nil t) ;; (no error if not found): remove-frame
(require 'autofit-frame nil t) ;; (no error if not found): fit-frame-if-one-window

;;;;;;;;;;;;;;;;;;;;;;;;

;;; Some settings I use.  I removed them from this file so you can use Customize to set them.
;;;
;;; (eval-after-load "bookmark" '(setq bookmark-save-flag 1))
;;; (global-font-lock-mode t)            ; Turn on font-lock-mode, generally.
;;; (setq font-lock-verbose 50000)       ; "Fontifying...done" only if big buffer.
;;; (setq font-lock-maximum-size 512000) ; Double the default size.
;;; (eval-after-load "apropos" '(setq apropos-do-all t)) ; Set to nil for 2-3 times faster.
;;; (setq Info-fontify-maximum-menu-size 500000)
;;; (when (boundp 'isearch-resume-in-command-history)
;;;   (setq isearch-resume-in-command-history t)) ; Be able to repeat searches as commands.
;;; (setq undo-limit 80000)                 ; 4 X the std default.
;;; (setq undo-strong-limit 120000)         ; 4 X the std default.
;;; (setq gc-cons-threshold 1600000)        ; 4 X the std default (400000).
;;; (setq list-directory-verbose-switches "-Ails") ; Defined in `files.el'.
;;; (setq default-major-mode 'indented-text-mode)
;;; (when (>= emacs-major-version 21)
;;;   (setq display-buffer-reuse-frames t)
;;;   (setq view-remove-frame-by-deleting t))
;;; (eval-after-load "time" '(progn (setq display-time-24hr-format t) ; 24-hour clock.
;;;                                 (display-time)))  ; Put time in mode-line.
;;; (setq list-command-history-max 1000) ; 32 is the default.  In `chistory.el'.



;;; Some standard faces redefined, and two simple faces defined.
;;; (condition-case nil
;;;     (progn
;;;       ;; (set-face-background 'highlight "Cyan") ; Highlight face.
;;;       ;; (set-face-foreground (if (facep 'modeline) 'modeline 'mode-line) ; Mode-line.
;;;       ;;                      "Black")
;;;       ;; (set-face-background 'region "MediumAquamarine")
;;;       ;; (set-face-background list-matching-lines-face "SkyBlue") ; Defined in `replace.el'
;;;       ;; (set-face-background 'secondary-selection "White") ; Secondary sel.
;;;       ;; (set-face-foreground 'secondary-selection "Black") ; (Netscape/X: 000000).
;;;       ;; (set-face-background 'show-paren-match-face "Aquamarine") ; in `paren.el'
;;;       )
;;;   (error nil))

(unless (facep 'font-lock-string-face)
  (make-face 'font-lock-string-face)
  (set-face-foreground 'font-lock-string-face "Magenta4"))
(unless (facep 'font-lock-constant-face)
  (make-face 'font-lock-constant-face)
  (set-face-foreground 'font-lock-constant-face "#00006DE06DE0"))
(unless (facep 'font-lock-warning-face)
  (make-face 'font-lock-warning-face)
  (set-face-foreground 'font-lock-warning-face "Yellow"))
(unless (facep 'font-lock-function-name-face)
  (make-face 'font-lock-function-name-face)
  (set-face-foreground 'font-lock-function-name-face "Red"))
(unless (facep 'font-lock-keyword-face)
  (make-face 'font-lock-keyword-face)
  (set-face-foreground 'font-lock-keyword-face "Blue3"))
;; (set-face-foreground 'font-lock-builtin-face "Orchid") ; Restore default per before Emacs 23.2.


;;; EDIFF stuff.  These variables and functions are defined in `ediff.el'.
;;; (setq ediff-control-frame-position-function 'my-ediff-control-frame-position)
;;; ;;; Replacement for `ediff-make-position'.
;;; (defun my-ediff-control-frame-position (ctl-buffer ctl-frame-width ctl-frame-height))
;;; (defun set-ediff-control-position () "@@@@@@@@@@"
;;;   (modify-frame-parameters ediff-control-frame '((name . "TEST Ediff") (top . 0))))
(eval-after-load "ediff"
  '(progn
    (setq-default ediff-ignore-similar-regions  t)
    (setq-default ediff-auto-refine-limit       10000)
    (setq ediff-grab-mouse                      'maybe) ; Nil -> no grab, t -> grab.
;;; (setq ediff-after-setup-control-frame-hook 'set-ediff-control-position)
    ;;     (when (fboundp 'ediff-set-face)
    ;;       (ediff-set-face 'foreground ediff-even-diff-face-A "White")
    ;;       (ediff-set-face 'foreground ediff-even-diff-face-B "White")
    ;;       (ediff-set-face 'foreground ediff-even-diff-face-C "White")
    ;;       (ediff-set-face 'foreground ediff-even-diff-face-Ancestor "White")
    ;;       (ediff-set-face 'background ediff-even-diff-face-A "Gray")
    ;;       (ediff-set-face 'background ediff-even-diff-face-B "Gray")
    ;;       (ediff-set-face 'background ediff-even-diff-face-C "Gray")
    ;;       (ediff-set-face 'background ediff-even-diff-face-Ancestor "Gray")

    ;;       (ediff-set-face 'foreground ediff-odd-diff-face-A "Black")
    ;;       (ediff-set-face 'foreground ediff-odd-diff-face-B "Black")
    ;;       (ediff-set-face 'foreground ediff-odd-diff-face-C "Black")
    ;;       (ediff-set-face 'foreground ediff-odd-diff-face-Ancestor "Black")
    ;;       (ediff-set-face 'background ediff-odd-diff-face-A "LightGray")
    ;;       (ediff-set-face 'background ediff-odd-diff-face-B "LightGray")
    ;;       (ediff-set-face 'background ediff-odd-diff-face-C "LightGray")
    ;;       (ediff-set-face 'background ediff-odd-diff-face-Ancestor "LightGray")

    ;;       (ediff-set-face 'foreground ediff-current-diff-face-A "Black")
    ;;       (ediff-set-face 'foreground ediff-current-diff-face-B "Black")
    ;;       (ediff-set-face 'foreground ediff-current-diff-face-C "Black")
    ;;       (ediff-set-face 'foreground ediff-current-diff-face-Ancestor "Black")
    ;;       (ediff-set-face 'background ediff-current-diff-face-A "SkyBlue")
    ;;       (ediff-set-face 'background ediff-current-diff-face-B "SkyBlue")
    ;;       (ediff-set-face 'background ediff-current-diff-face-C "SkyBlue")
    ;;       (ediff-set-face 'background ediff-current-diff-face-Ancestor "SkyBlue")

    ;;       (ediff-set-face 'foreground ediff-fine-diff-face-A "Black")
    ;;       (ediff-set-face 'foreground ediff-fine-diff-face-B "Black")
    ;;       (ediff-set-face 'foreground ediff-fine-diff-face-C "Black")
    ;;       (ediff-set-face 'foreground ediff-fine-diff-face-Ancestor "Black")
    ;;       (ediff-set-face 'background ediff-fine-diff-face-A "Cyan")
    ;;       (ediff-set-face 'background ediff-fine-diff-face-B "Cyan")
    ;;       (ediff-set-face 'background ediff-fine-diff-face-C "Cyan")
    ;;       (ediff-set-face 'background ediff-fine-diff-face-Ancestor "Cyan"))
    ))

;;; `ediff+.el' requires `ediff.el', which defines ediff-control-buffer'.
;;; (eval-when-compile (require 'ediff+))
;;; (progn ;; Must be in this order.
;;;  (add-hook 'ediff-quit-hook 'ediff-default-quit-hook)
;;;  (add-hook 'ediff-quit-hook
;;;            (function (lambda ()        ; EDIFF-CONTROL-BUFFER is free here.
;;;                        (delete-1-window-frames-on ediff-control-buffer)))))
;;; (setq ediff-split-window-function 'ediff-use-separate-frames)

(eval-after-load "icomplete+" '(icomplete-mode 99))

;;; VC stuff.  I no longer use VC.
;;; (eval-after-load "vc"
;;;   (progn (setq vc-initial-comment t)    ; VC asks for file purpose comment.
;;;          (setq vc-command-messages t))) ; VC mentions shell commands it runs.

;;; GNUS stuff.  I don't use this either.
;;; (setq gnus-window-configuration '((summary (0 1 0)) (newsgroups (1 0 0))
;;;                                   (article (0 0 1))))


;;; Search and replace stuff:
(copy-face 'secondary-selection 'query-replace) ; For replacement highlighting.
(if (facep 'isearch)
    nil ;; (set-face-attribute 'isearch nil :foreground "Black" :background "Green" :inverse-video nil)
  (defface isearch '((t (:foreground "Black" :background "Green")))
    "Face for highlighting Isearch matches." :group 'isearch))
(setq-default case-fold-search  nil)     ; Case sensitive by default.
;;; (setq search-ring-max  1000)
;;; (setq regexp-search-ring-max  1000)

;; Use `tool-bar-pop-up-mode'.
(when (and (>= emacs-major-version 22) (fboundp 'tool-bar-pop-up-mode))
  (tool-bar-pop-up-mode 99))

;; Automatically fit one-window frames containing "temporary" buffers (e.g. *Help*).
(when (fboundp 'fit-frame-if-one-window) ; Defined in `autofit-frame.el'.
  (add-hook 'temp-buffer-show-hook        'fit-frame-if-one-window 'append)
  (add-hook 'temp-buffer-window-show-hook 'fit-frame-if-one-window 'append)) ; Emacs 24.4+

;; Automatically fit newly created frames.
(when (fboundp 'fit-frame)              ; Defined in `fit-frame.el'.
  (add-hook 'after-make-frame-functions 'fit-frame))

(when (fboundp 'tooltip-mode)           ; Make tooltips use echo area.
  (condition-case nil (tooltip-mode -1) (error nil)))

;; Use TAGS file for Drew's Lisp library.
;; (`drews-lisp-dir' should be defined in your init file: `~/.emacs'.
;; It should point to the directory containing this file, `start-opt.el'.)
(and (boundp 'drews-lisp-dir) drews-lisp-dir (setq tags-file-name  drews-lisp-dir))

;; Savehist.
(if (fboundp 'savehist-mode)
    (savehist-mode 1)
  (when (fboundp 'savehist-load) (savehist-load))) ; Load your saved history lists.

;;; Hilit stuff.  Defined in `hilit19.el'.  I no longer use this.
;;; (setq hilit-auto-highlight nil)         ; Don't hilit when `find-file'.
;;; (setq hilit-face-check nil)             ; To hilit faster.
;;; (setq hilit-inhibit-rebinding nil)      ; Don't rebind recenter, yank, yank-pop

;;; Indicate hiding minor mode in mode line.  In `hide-ifdef.el'.  I don't use it.
;;; (push '(hiding " Hiding") minor-mode-alist)

;;; Indicate `iso-accents-minor-mode' in mode line.  In `iso-acc.el'.  I don't use it.
;;; (or (assq 'iso-accents-minor-mode minor-mode-map-alist)
;;;    (setq minor-mode-alist
;;;          (append minor-mode-alist '((iso-accents-minor-mode " ISO-Acc")))))

;; HTML -- `html-helper-mode'.
(eval-after-load "html-helper-mode"
  (add-hook 'html-mode-hook 'imenu-add-menubar-index))
;;; (add-hook 'html-helper-mode-hook 'turn-on-font-lock)
;;; (add-hook 'html-helper-load-hook '(lambda () (require 'html-font)))
;;; (setq html-helper-do-write-file-hooks t) ; `local-write-file-hooks' timestamp.
;;; (setq html-helper-build-new-buffer t)   ; Add `html-helper-new-buffer-strings'.
;;; (setq html-helper-address-string (user-full-name)) ; Default author string.

;;; (setq abbrev-file-name (substitute-in-file-name "~/.abbrev-defs")) ; In `paths.el'.
;;; (put 'abbrev-file-name 'variable-interactive "FFile to read abbrevs from: ")

;;; Save word abbrevs when files saved.
;;; (setq save-abbrevs t)                   ; Defined in `files.el'.
;;; (setq display-time-day-and-date nil)    ; No date in modeline.  In `time.el'.

(setq-default indent-tabs-mode  nil)    ; SPCs only (no TABs), when indenting.
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1)) ; Use classic `C-j' and `RET'.

;;; The defcustom's in Francis Wright's `ls-lisp.el' cannot take
;;; effect, because `ls-lisp.el' is a standard library, preloaded.
;;; So, make the assignments here.
;;; Use `featurep', not `eval-after-load' - won't work otherwise, for Emacs 20.
(when (featurep 'ls-lisp)
  (setq ls-lisp-emulation  (cond ((memq system-type '(windows-nt ms-dos emx)) 'MS-Windows)
                                 ;; FJW: not sure about how to handle emx!
                                 ((memq system-type '(hpux dgux usg-unix-v unisoft-unix
                                                      rtu irix berkeley-unix))
                                  'UNIX)))
  (setq ls-lisp-ignore-case
        ;; Name change for consistency with other option names.
        (or (eq ls-lisp-emulation 'MS-Windows)
            (and (boundp 'ls-lisp-dired-ignore-case) ls-lisp-dired-ignore-case)))
  (setq ls-lisp-dirs-first  (eq ls-lisp-emulation 'MS-Windows))
  (setq ls-lisp-verbosity  (cond ((eq ls-lisp-emulation 'MS-Windows)
                                  ;; Distinguish NT/2K from 9x
                                  (and (getenv "SystemRoot") '(links)))
                                 ((eq ls-lisp-emulation 'UNIX) '(links uid)) ; UNIX ls
                                 (t '(links uid gid))))) ; GNU ls

(setq auto-mode-alist  (append (list    ; Defined in `files.el'.
                                '("\\.te?xt\\'" . indented-text-mode)
                                '("\\.bmk\\'" . emacs-lisp-mode)
                                '("\\.elc\\'" . emacs-lisp-mode)
                                '("\\.tex\\'" . LaTeX-mode)
                                '("\\.aux\\'" . LaTeX-mode)
                                '("\\.glo\\'" . LaTeX-mode)
                                '("\\.lof\\'" . LaTeX-mode)
                                ;; '("\\.log\\'" . LaTeX-mode)
                                '("\\.lot\\'" . LaTeX-mode)
                                '("\\.toc\\'" . LaTeX-mode)
                                ;; '("\\.ftex\\'" . LaTeX-mode)
                                '("\\.lxx\\'" . c++-mode)
                                '("\\.ixx\\'" . c++-mode)
                                '("\\.gxx\\'" . c++-mode)
                                '("\\.pxx\\'" . c++-mode)
                                '("\\.sql\\'" . sql-mode))
                               auto-mode-alist))

(eval-after-load "tex-mode"
  '(progn
    (add-hook 'tex-mode-hook       'imenu-add-menubar-index)
    ;; Auto-fill by default for text modes.
    (add-hook 'text-mode-hook      'turn-on-auto-fill t)
    (add-hook 'latex-mode-hook     'turn-on-auto-fill t)
    (add-hook 'plain-tex-mode-hook 'turn-on-auto-fill t)
    (add-hook 'tex-mode-hook       'turn-on-auto-fill t)
    (add-hook 'slitex-mode-hook    'turn-on-auto-fill t)))

;;; Frame title: buffer name.
(setq frame-title-format  '(multiple-frames "%b" "%b"))

;;; Mode line.
(eval-after-load "time" (display-time)) ; Put time in mode line.
;;; (line-number-mode 9999)                 ; Put line # in mode-line.


;;; (setq dired-latex-unclean-extensions    ; Defined in `dired-x.el'.
;;;      '(".idx" ".lof" ".lot" ".glo"     ; Std default.
;;;        ".errefs" ".ind" ".ilg"))       ; Extras added here.

;;; The following variables are all defined in `tex-mode.el'.
;;; (setq tex-default-mode 'latex-mode)
;;; (setq tex-dvi-print-command "dvips -f -t a4 * | declp -D p -M p -K 2")
;;; (put 'tex-dvi-print-command 'variable-interactive
;;;     "sCommand used by `tex-print' to print a *.dvi file: ")
;;; (setq tex-alt-dvi-print-command "dvips -f -t a4 * | declp -D p -M p")
;;; (put 'tex-alt-dvi-print-command 'variable-interactive
;;;     "sCommand used by `tex-print' with a prefix arg to print a *.dvi file: ")
;;; (setq tex-dvi-view-command
;;;      (if (eq window-system 'x) "xdvi" "dvi2tty * | cat -s"))
;;; (put 'tex-dvi-view-command 'variable-interactive
;;;     "sCommand used by `tex-view' to display a *.dvi file: ")
;;; (setq tex-show-queue-command "declpstat|more")
;;; (put 'tex-show-queue-command 'variable-interactive
;;;     "sCommand used by `tex-show-print-queue' to show the print queue: ")

;;; Use virtual Dired mode by default.  Defined in `dired-x.el'.
;;; (setq auto-mode-alist (cons '("[^/]\\.dired\\'" . dired-virtual-mode)
;;;                            auto-mode-alist))

;;; Hexl to display ISO chars too.
;;; (setq hexl-iso "-iso")                  ; Defined in `hexl.el'.

;;; (setq-default comint-prompt-regexp shell-prompt-pattern) ; In `comint.el'.
;;; (put 'comint-prompt-regexp 'variable-interactive
;;;     "sRegexp to recognise prompts in the inferior process: ")
;;; (setq shell-popd-regexp "\\(popd\\|o\\)") ; Defined in `cmushell.el'.
;;; (put 'shell-popd-regexp 'variable-interactive
;;;     "sRegexp to match subshell commands equivalent to `popd': ")
;;; (setq shell-pushd-regexp "\\(pushd\\|u\\)") ; Defined in `cmushell.el'.
;;; (put 'shell-pushd-regexp 'variable-interactive
;;;     "sRegexp to match subshell commands equivalent to `pushd': ")

;;; Word completion.  These variables are defined in `completion.el'.
;;; (setq completion-on-separator-character t) ; Makes separators save words.
;;; (setq cmpl-case-fold-search t)          ; Case-sensitive completion.
;;; Use cdabbrev completion if no other (slower)
;;; (setq completion-cdabbrev-prompt-flag t)

;;; COMMENT-LINE-START and COMMENT-LINE-START-SKIP are free variables here.
;;; (eval-when-compile (require 'fortran)) ; comment-line-start(-skip)
;;; (defun setup-for-fortran-comments ()    ; Make useful for file headers etc.
;;;  (setq comment-line-start "C")         ; Upper case C.  Defined in `fortran.el'
;;;  (put 'comment-line-start 'variable-interactive
;;;       "sString inserted to start new full-line comment: ")
;;;  (setq comment-start comment-line-start) ; Defined in `simple.el'. (local var)
;;;  (setq comment-start-skip comment-line-start-skip)) ; Defined in `fortran.el'.
;;; (add-hook 'fortran-mode-hook 'setup-for-fortran-comments)

;;; SHELL-MODE-MAP is a free variable here.  Defined in `shell.el'.
;;; (eval-when-compile (require 'shell)) ;; shell-mode-map
;;; (defun define-shell-completion-keys ()
;;; (define-key shell-mode-map "\C-c\C-i" 'my-shell-complete)
;;; (define-key shell-mode-map "\C-c\?" 'my-shell-completion-help))
;;; (add-hook 'shell-mode-hook 'define-shell-completion-keys)

(add-hook 'command-history-hook (function (lambda ()
                                  (when (fboundp 'electric-command-history)
                                    (define-key command-history-map "e"
                                      'electric-command-history)) ; In `echistory.el'.
                                  (define-key command-history-map "m"
                                    'repeat-matching-complex-command)))) ; `chistory.el'.

(eval-after-load "color-moccur"
  '(progn
    (when (fboundp 'dired-do-moccur) (define-key dired-mode-map "E" 'dired-do-moccur))
    (when (fboundp 'Buffer-menu-moccur)
      (define-key Buffer-menu-mode-map "E" 'Buffer-menu-moccur))
    (when (fboundp 'occur-by-moccur)
      (global-set-key "\C-x\C-o" 'occur-by-moccur)) ; was `delete-blank-lines'
    (when (fboundp 'moccur) (global-set-key "\C-c\C-x\C-o" 'moccur))      
    (when (fboundp 'grep-buffers) (global-set-key "\C-cg" 'grep-buffers))
    (when (fboundp 'search-buffers) (global-set-key "\C-c\C-o" 'search-buffers))))

;;; Use speedbar, but without images
;;; (setq speedbar-use-images nil)
;;; (require 'speedbar)
;;; (speedbar-frame-mode 99)

;;; This is a hack because in Emacs 22 speedbar creates the speedbar frame before it fills
;;; the buffer, and speedbar provides no appropriate hook for doing this at the right time.
;;; Bug filed.
(eval-after-load "fit-frame"
  '(let ((win  (get-buffer-window " SPEEDBAR" t)))
    (when win (save-window-excursion (select-window win) (fit-frame)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;; ****** Begin stuff to do at the end ***********

;;;-----------REPLACEMENT BINDINGS------------------------------------

(eval-after-load "wimpy-del"
  '(progn
    (substitute-key-definition 'kill-region           'kill-region-wimpy global-map)
    (substitute-key-definition 'clipboard-kill-region 'clipboard-kill-region-wimpy global-map)))

;;; These have already been done once in `setup-keys.el'.
;;; Repeat here in case someone has made some more of these bindings.
;;; Do this *after* load `menu-bar+.el', since that sets original bindings.
(eval-after-load "frame-cmds"
  '(when (and (boundp 'sub-delete-windows-for) sub-delete-windows-for)
    (substitute-key-definition 'delete-window 'delete-windows-for global-map)))

(eval-after-load "replace+"
  '(when (and (boundp 'sub-query-replace-w-options) sub-query-replace-w-options)
    (substitute-key-definition 'query-replace 'query-replace-w-options global-map)))

(eval-after-load "misc-cmds"
  '(when (and (boundp 'sub-kill-buffer-and-its-windows) sub-kill-buffer-and-its-windows)
    (substitute-key-definition 'kill-buffer 'kill-buffer-and-its-windows global-map)))

(eval-after-load "pp"                   ; Also in `pp+.el'.
  '(when (and (boundp 'sub-pp-evals) sub-pp-evals)
    (substitute-key-definition 'eval-last-sexp  'pp-eval-last-sexp  global-map)
    (substitute-key-definition 'eval-expression 'pp-eval-expression global-map)))

(eval-after-load "buff-menu"            ; Also in `buff-menu+.el'.
  '(substitute-key-definition 'list-buffers 'buffer-menu global-map))

;;; Undefine some bindings that would try to modify buffers like Dired.
;;; Their key sequences will then appear to the user as available for
;;; local definition.  This may have been done before, in `dired+.el' etc.,
;;; but it may help to do it again, since we have defined more bindings here.
(eval-after-load "misc-fns"
  '(progn
    (when (boundp 'dired-mode-map)       (undefine-killer-commands dired-mode-map))
    (when (boundp 'Buffer-menu-mode-map) (undefine-killer-commands Buffer-menu-mode-map))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Do things this way because there is no `fundamental-mode-hook'.

(eval-after-load "header2"
  '(progn
    (add-hook 'emacs-lisp-mode-hook       'auto-make-header)
    (add-hook 'ccl-mode-hook              'auto-make-header)
    (add-hook 'lisp-mode-hook             'auto-make-header)
    (add-hook 'cmulisp-mode-hook          'auto-make-header)
    (add-hook 'c-mode-hook                'auto-make-header)
    (add-hook 'c++-mode-hook              'auto-make-header)
    (add-hook 'c-mode-common-hook         'auto-make-header)
    (add-hook 'idl-mode-hook              'auto-make-header)
    (add-hook 'fortran-mode-hook          'auto-make-header)))

(add-hook 'emacs-lisp-mode-hook (function (lambda () (require 'lisp-mnt))))

(add-hook 'emacs-lisp-mode-hook       'turn-on-font-lock)
(add-hook 'ccl-mode-hook              'turn-on-font-lock)
(add-hook 'lisp-mode-hook             'turn-on-font-lock)
(add-hook 'cmulisp-mode-hook          'turn-on-font-lock)
(add-hook 'c-mode-hook                'turn-on-font-lock)
(add-hook 'c++-mode-hook              'turn-on-font-lock)
(add-hook 'c-mode-common-hook         'turn-on-font-lock)
(add-hook 'idl-mode-hook              'turn-on-font-lock)
(add-hook 'makefile-mode-hook         'turn-on-font-lock)
(add-hook 'sh-mode-hook               'turn-on-font-lock)
(add-hook 'fortran-mode-hook          'turn-on-font-lock)

;; This is not good enough, for `g' in Dired buffer with explicit list of files.
;; Need to ensure that we Refontify the buffer.
;; (add-hook 'dired-after-readin-hook 'turn-on-font-lock)
;;
;; Library `dired+.el' takes care of this, so do nothing if `diredp-refontify-buffer' is defined.
(unless (fboundp 'diredp-refontify-buffer)
  (defun turn-font-lock-off-then-on ()
    "Refontify."
    (setq font-lock-mode  nil)
    (turn-on-font-lock))
  (add-hook 'dired-after-readin-hook  'turn-font-lock-off-then-on))

(add-hook 'list-options-hook          'font-lock-fontify-buffer)
(add-hook 'outline-mode-hook          'turn-on-font-lock)
(add-hook 'sql-mode-hook              'turn-on-font-lock)

;;; (add-hook 'gnus-group-prepare-hook    'turn-on-font-lock)
;;; (add-hook 'gnus-group-prepare-hook    'mouse-face-each-line) ; `highlight.el'.
;;; (add-hook 'gnus-summary-prepare-hook  'turn-on-font-lock)
;;; (add-hook 'gnus-summary-prepare-hook  'mouse-face-each-line) ; `highlight.el'.
;;; (add-hook 'gnus-article-prepare-hook  'font-lock-fontify-buffer);`font-lock.el'
;;; (add-hook 'rmail-show-message-hook    'font-lock-fontify-buffer)
;;; (add-hook 'rmail-summary-mode-hook    'font-lock-fontify-buffer)
;;;                                         ; Defined in `outline+.el':
;;; (add-hook 'outline-minor-mode-hook    'toggle-outline-minor-mode-font-lock)
;;; (add-hook 'outline-minor-mode-exit-hook 'toggle-outline-minor-mode-font-lock)

;;; ****** End stuff to do at the end ***********

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'start-opt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; start-opt.el ends here

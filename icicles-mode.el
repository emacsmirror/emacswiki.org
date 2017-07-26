;;; icicles-mode.el --- Icicle Mode definition for Icicles
;;
;; Filename: icicles-mode.el
;; Description: Icicle Mode definition for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2016, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 10:21:10 2006
;; Last-Updated: Wed Jul 26 08:20:50 2017 (-0700)
;;           By: dradams
;;     Update #: 10289
;; URL: https://www.emacswiki.org/emacs/download/icicles-mode.el
;; Doc URL: https://www.emacswiki.org/emacs/Icicles
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `advice', `advice-preload', `apropos', `apropos+',
;;   `apropos-fn+var', `autofit-frame', `avoid', `bookmark',
;;   `bookmark+', `bookmark+-1', `bookmark+-bmu', `bookmark+-key',
;;   `bookmark+-lit', `cl', `cus-edit', `cus-face', `cus-load',
;;   `cus-start', `cus-theme', `dired', `dired+', `dired-aux',
;;   `dired-x', `doremi', `easymenu', `el-swank-fuzzy', `ffap',
;;   `ffap-', `fit-frame', `flx', `frame-cmds', `frame-fns', `fuzzy',
;;   `fuzzy-match', `help+20', `hexrgb', `highlight', `icicles-cmd1',
;;   `icicles-cmd2', `icicles-fn', `icicles-mcmd', `icicles-opt',
;;   `icicles-var', `image-dired', `image-file', `info', `info+20',
;;   `kmacro', `levenshtein', `menu-bar', `menu-bar+', `misc-cmds',
;;   `misc-fns', `mouse3', `mwheel', `naked', `package', `pp', `pp+',
;;   `regexp-opt', `ring', `second-sel', `strings', `subr-21',
;;   `thingatpt', `thingatpt+', `unaccent', `w32-browser',
;;   `w32browser-dlgopen', `wid-edit', `wid-edit+', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines the
;;  command `icicle-mode'.  For Icicles documentation, see
;;  `icicles-doc1.el' and `icicles-doc2.el'.
;;
;;  Commands defined here:
;;
;;    `icicle-handle-switch-frame', `icicle-mode', `icy-mode',
;;    `icicle-ORIG-bbdb-complete-mail',
;;    `icicle-ORIG-bbdb-complete-name',
;;    `icicle-ORIG-comint-dynamic-complete',
;;    `icicle-ORIG-comint-dynamic-complete-filename',
;;    `icicle-ORIG-comint-replace-by-expanded-filename',
;;    `icicle-ORIG-complete', `icicle-ORIG-dired-read-shell-command',
;;    `icicle-ORIG-ess-complete-object-name',
;;    `icicle-ORIG-gud-gdb-complete-command',
;;    `icicle-ORIG-read-file-name', `icicle-ORIG-read-shell-command',
;;    `icicle-skip-this-command'.
;;
;;  Non-interactive functions defined here:
;;
;;    `icicle-activate-mark', `icicle-add-menu-item-to-cmd-history',
;;    `icicle-bind-completion-keys',
;;    `icicle-bind-custom-minibuffer-keys',
;;    `icicle-bind-isearch-keys',
;;    `icicle-bind-key-completion-keys-for-map-var',
;;    `icicle-bind-key-completion-keys-in-keymaps-from',
;;    `icicle-bind-other-keymap-keys',
;;    `icicle-cancel-Help-redirection', `icicle-define-cycling-keys',
;;    `icicle-define-icicle-maps', `icicle-define-minibuffer-maps',
;;    `icicle-help-line-buffer', `icicle-help-line-file',
;;    `icicle-last-non-minibuffer-buffer', `icicle-minibuffer-setup',
;;    `icicle-rebind-global', `icicle-redefine-standard-functions',
;;    `icicle-redefine-standard-options',
;;    `icicle-redefine-std-completion-fns',
;;    `icicle-restore-completion-keys',
;;    `icicle-restore-custom-minibuffer-keys',
;;    `icicle-restore-other-keymap-keys',
;;    `icicle-restore-region-face',
;;    `icicle-restore-standard-functions',
;;    `icicle-restore-standard-options',
;;    `icicle-restore-std-completion-fns',
;;    `icicle-run-icicle-post-command-hook',
;;    `icicle-run-icicle-pre-command-hook',
;;    `icicle-select-minibuffer-contents', `icicle-set-calling-cmd',
;;    `icicle-show-current-help-in-mode-line',
;;    `icicle-show-help-in-mode-line', `icicle-show-in-mode-line',
;;    `icicle-S-iso-lefttab-to-S-TAB', `icicle-top-level-prep',
;;    `icicle-unbind-isearch-keys',
;;    `icicle-unbind-key-completion-keys-for-map-var',
;;    `icicle-unbind-key-completion-keys-in-keymaps-from',
;;    `icicle-undo-std-completion-faces', `icicle-unmap',
;;    `icicle-update-ignored-extensions-regexp'.
;;
;;  User options defined here (in Custom group `Icicles'):
;;
;;    `icicle-mode', `icicle-mode-hook'.
;;
;;  Internal variables defined here:
;;
;;    `icicle-bookmark-menu-map', `icicle-custom-menu-map',
;;    `icicle-describe-menu-map', `icicle-dired-multiple-menu-map',
;;    `icicle-dired-recursive-marked-menu-map',
;;    `icicle-edit-menu-map', `icicle-file-menu-map',
;;    `icicle-frames-menu-map', `icicle-goto-imenu-menu-map',
;;    `icicle-goto-menu-map', `icicle-info-menu-map',
;;    `icicle-mode-map', `icicle-options-menu-map',
;;    `icicle-options-choose-menu-map',
;;    `icicle-options-toggle-menu-map', `icicle-search-menu-map'.
;;
;;  For descriptions of changes to this file, see `icicles-chg.el'.
 
;;(@> "Index")
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  https://www.emacswiki.org/emacs/download/linkd.el.
;;
;;  (@> "User Options (alphabetical)")
;;  (@> "Internal variables (alphabetical)")
;;  (@> "Icicle mode command")
;;  (@> "Other Icicles functions that define Icicle mode")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
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
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; ;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl)) ;; flet, pushnew, case
                                  ;; plus, for Emacs < 21: push, dolist
(require 'advice)
  ;; ad-activate, ad-copy-advice-info, ad-deactivate, ad-disable-advice, ad-enable-advice,
  ;; ad-find-some-advice, ad-get-arg, ad-is-active, ad-set-advice-info, defadvice

(eval-when-compile
 (or (condition-case nil
         (load-library "icicles-mac")   ; Use load-library to ensure latest .elc.
       (error nil))
     (require 'icicles-mac)))           ; Require, so can load separately if not on `load-path'.
  ;; icicle-menu-bar-make-toggle
(require 'icicles-opt)                  ; (This is required anyway by `icicles-var.el'.)
  ;; icicle-add-proxy-candidates-flag, icicle-buffer-configs, icicle-buffer-extras,
  ;; icicle-change-region-background-flag, icicle-current-TAB-method, icicle-default-cycling-mode,
  ;; icicle-incremental-completion, icicle-default-value, icicle-kbd, icicle-kmacro-ring-max,
  ;; icicle-minibuffer-setup-hook, icicle-modal-cycle-down-keys, icicle-modal-cycle-up-keys,
  ;; icicle-functions-to-redefine, icicle-regexp-search-ring-max, icicle-region-background,
  ;; icicle-search-ring-max, icicle-shell-command-candidates-cache, icicle-show-Completions-initially-flag,
  ;; icicle-top-level-key-bindings, icicle-touche-pas-aux-menus-flag, icicle-word-completion-keys
(require 'icicles-fn)                   ; (This is required anyway by `icicles-cmd1.el'.)
  ;; icicle-completing-p, icicle-toggle-icicle-mode-twice, icicle-unhighlight-lighter
(require 'icicles-var)                  ; (This is required anyway by `icicles-fn.el'.)
  ;; icicle-advice-info-list, icicle-all-candidates-action, icicle-all-candidates-list-action-fn,
  ;; icicle-all-candidates-list-alt-action-fn, icicle-apropos-complete-match-fn,
  ;; icicle-auto-no-icomplete-mode-p, icicle-auto-no-sort-p, icicle-candidate-action-fn,
  ;; icicle-candidate-alt-action-fn, icicle-candidate-nb, icicle-candidates-alist,
  ;; icicle-cmd-calling-for-completion, icicle-cmd-reading-input, icicle-completing-p,
  ;; icicle-completing-read+insert-candidates, icicle-completion-candidates, icicle-completion-prompt-overlay,
  ;; icicle-completion-style-set, icicle-current-completion-mode, icicle-ess-use-ido,
  ;; icicle-ignored-extensions, icicle-ignored-extensions-regexp, icicle-incremental-completion-p,
  ;; icicle-inhibit-advice-functions, icicle-inhibit-sort-p, icicle-initial-value,
  ;; icicle-input-completion-fail-overlay, icicle-input-fail-pos, icicle-last-completion-candidate,
  ;; icicle-last-completion-command, icicle-last-icomplete-mode-value, icicle-last-input,
  ;; icicle-last-top-level-command, icicle-multi-completing-p, icicle-menu-map, icicle-minor-mode-map-entry,
  ;; icicle-orig-read-file-name-fn, icicle-pre-minibuffer-buffer, icicle-previous-raw-file-name-inputs,
  ;; icicle-previous-raw-non-file-name-inputs, icicle-progressive-completing-p, icicle-proxy-candidates,
  ;; icicle-saved-candidate-overlays, icicle-saved-completion-candidates, icicle-saved-kmacro-ring-max,
  ;; icicle-saved-proxy-candidates, icicle-saved-regexp-search-ring-max, icicle-saved-region-background,
  ;; icicle-saved-search-ring-max, icicle-search-complement-domain-p, icicle-search-current-overlay,
  ;; icicle-search-map, icicle-search-overlays, icicle-search-refined-overlays, icicle-toggle-map
(require 'icicles-cmd1)                 ; (This is required anyway by `icicles-cmd2.el'.)
  ;; icicle-add-buffer-candidate, icicle-add-buffer-config, icicle-customize-face-other-window,
  ;; icicle-select-bookmarked-region
(require 'icicles-cmd2)
  ;; icicle-imenu, icicle-imenu-command, icicle-imenu-command-full, icicle-imenu-face,
  ;; icicle-imenu-face-full, icicle-imenu-full, icicle-imenu-key-explicit-map,
  ;; icicle-imenu-key-explicit-map-full, icicle-imenu-key-implicit-map, icicle-imenu-key-implicit-map-full,
  ;; icicle-imenu-macro, icicle-imenu-macro-full, icicle-imenu-non-interactive-function,
  ;; icicle-imenu-non-interactive-function-full, icicle-imenu-user-option, icicle-imenu-user-option-full,
  ;; icicle-imenu-variable, icicle-imenu-variable-full, icicle-occur, icicle-occur-dired-marked,
  ;; icicle-occur-dired-marked-recursive, icicle-search, icicle-search-all-tags-bookmark,
  ;; icicle-search-all-tags-regexp-bookmark, icicle-search-autofile-bookmark,
  ;; icicle-search-autonamed-bookmark, icicle-search-bookmark, icicle-search-bookmark-list-bookmark,
  ;; icicle-search-bookmark-list-marked, icicle-search-bookmarks-together, icicle-search-buffer,
  ;; icicle-search-buff-menu-marked, icicle-search-char-property, icicle-search-dired-bookmark,
  ;; icicle-search-dired-marked, icicle-search-dired-marked-recursive, icicle-search-file,
  ;; icicle-search-file-bookmark, icicle-search-gnus-bookmark, icicle-search-highlight-cleanup,
  ;; icicle-search-ibuffer-marked, icicle-search-info-bookmark, icicle-searching-p, icicle-search-keywords,
  ;; icicle-search-local-file-bookmark, icicle-search-man-bookmark, icicle-search-non-file-bookmark,
  ;; icicle-search-overlay-property, icicle-search-pages, icicle-search-paragraphs,
  ;; icicle-search-region-bookmark, icicle-search-remote-file-bookmark, icicle-search-sentences,
  ;; icicle-search-some-tags-bookmark, icicle-search-some-tags-regexp-bookmark,
  ;; icicle-search-specific-buffers-bookmark, icicle-search-specific-files-bookmark,
  ;; icicle-search-temporary-bookmark, icicle-search-text-property, icicle-search-thing,
  ;; icicle-search-this-buffer-bookmark, icicle-search-url-bookmark, icicle-search-w3m-bookmark,
  ;; icicle-search-w-isearch-string, icicle-search-word, icicle-search-xml-element,
  ;; icicle-search-xml-element-text-node

;; Use `condition-case' because if `mb-depth.el' can't be found, `mb-depth+.el' is not provided.
(when (>= emacs-major-version 22) (condition-case nil (require 'mb-depth+ nil t) (error nil)))
  ;; (no error if not found): minibuffer-depth-indicate-mode

(require 'dired+ nil t) ;; (no error if not found):
                        ;; diredp-menu-bar-multiple-menu, diredp-menu-bar-operate-menu,
                        ;; diredp-menu-bar-recursive-marked-menu, diredp-menu-bar-dir-menu,
;;                      ;; diredp-menu-bar-subdir-menu
(require 'dired) ;; dired-mode-map
(require 'menu-bar+ nil t) ;; (no error if not found):
  ;; menu-bar-apropos-menu, menu-bar-describe-menu, menu-bar-edit-menu, menu-bar-file-menu,
  ;; menu-bar-frames-menu, menu-bar-options-menu, menu-bar-search-tags-menu

;; `icicle-apropos-complete' is used here.  It is defined in `icicles-mcmd.el'.
;; `icicle-file-name-input-p' is used here.  It is defined in `icicles-fn.el'.

;;; Defvars to quiet byte-compiler:
(when (< emacs-major-version 22)
  (defvar kmacro-ring-max)
  (defvar minibuffer-local-filename-completion-map)
  (defvar minibuffer-local-must-match-filename-map)
  (defvar minibuffer-local-filename-must-match-map)
  (defvar mouse-wheel-down-event)
  (defvar mouse-wheel-up-event)
  (defvar read-file-name-function))

(defvar Buffer-menu-mode-map)           ; In `buff-menu.el'.
(defvar comint-mode-map)                ; In `comint.el'.
(defvar crm-local-completion-map)       ; In `crm.el'.
(defvar crm-local-must-match-map)       ; In `crm.el'.
(defvar dired-mode-map)                 ; In `dired.el'.
(defvar diredp-menu-bar-multiple-menu)  ; In `dired+.el'
(defvar diredp-menu-bar-operate-menu)   ; In `dired+.el' (old name)
(defvar diredp-menu-bar-recursive-marked-menu) ; In `dired+.el'
(defvar diredp-menu-bar-dir-menu)       ; In `dired+.el'
(defvar diredp-menu-bar-subdir-menu)    ; In `dired+.el' (old name)
(defvar gud-minibuffer-local-map)       ; In `gud.el'.
(defvar ibuffer-mode-map)               ; In `ibuffer.el'.
(defvar ibuffer-mode-operate-map)       ; In `ibuffer.el'.
(defvar icicle-crm-local-completion-map) ; In `icicles-fn.el' after load `crm.el'.
(defvar icicle-crm-local-must-match-map) ; In `icicles-fn.el' after load `crm.el'.
(defvar icicle-kmacro-ring-max)         ; In `icicles-opt.el' for Emacs 22+.
(defvar icicle-ORIG-crm-local-completion-map) ; In `icicles-fn.el' after load `crm.el'.
(defvar icicle-ORIG-crm-local-must-match-map) ; In `icicles-fn.el' after load `crm.el'.
(defvar icicle-saved-kmacro-ring-max)   ; In `icicles-var.el' for Emacs 22+.
(defvar ielm-map)                       ; In `ielm.el'.
(defvar inferior-tcl-mode-map)          ; In `tcl.el'.
(defvar Info-mode-map)                  ; In `info.el'.
(defvar isearch-mode-map)               ; In `isearch.el'.
(defvar menu-bar-buffers-menu-command-entries) ; In `menu-bar.el' for Emacs 24+.
(defvar menu-bar-goto-menu)             ; In `menu-bar.el'.
(defvar savehist-minibuffer-history-variables) ; In `savehist.el'
(defvar shell-mode-map)                 ; In `shell.el'.
(defvar sh-mode-map)                    ; In `sh-script.el'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "User Options (alphabetical)")

;;; User Options (alphabetical) --------------------------------------

;; Emacs 20 only
(unless (fboundp 'define-minor-mode)
  (defcustom icicle-mode nil
    "*Non-nil means use Icicles minibuffer input completion and cycling.
Setting this variable directly does not take effect;
use either \\[customize] or command `icy-mode' (aka `icicle-mode')."
    :set (lambda (symbol value) (icicle-mode (if value 1 -1)))
    :initialize 'custom-initialize-default
    :type 'boolean :group 'Icicles-Miscellaneous :require 'icicles))

(defcustom icicle-mode-hook nil
  "*Functions run after entering and exiting Icicle mode."
  :type 'hook :group 'Icicles-Miscellaneous)
 
;;(@* "Internal variables (alphabetical)")

;;; Internal variables (alphabetical) --------------------------------

(defvar icicle-mode-map nil
  "Keymap for Icicle mode.  These are top-level key bindings.
See also `icicle-define-minibuffer-maps' for minibuffer bindings and
bindings in `*Completions*'.")
 
;;(@* "Icicle mode command")

;;; Icicle mode command ----------------------------------------------

;; Main command.
(defalias 'icy-mode 'icicle-mode)
(when (fboundp 'define-minor-mode)      ; Emacs 21+ ------------
  (when (> emacs-major-version 22)
    (defadvice call-interactively (after icicle-save-to-history disable activate)
      "Save command to `icicle-interactive-history'."
      ;; If command's input is not a parameterized (e.g. mouse) event, record it.
      (let* ((fn   (ad-get-arg 0))
             (int  (interactive-form fn)))
        (when (and (symbolp fn)  (consp int)  (or (not (stringp (cadr int)))
                                                  (string= (cadr int) "")
                                                  (not (eq ?e (aref (cadr int) 0)))))
          (pushnew (symbol-name fn) icicle-interactive-history))))
    (when (boundp 'savehist-save-hook)  ; Do not save `icicle-interactive-history' (too large).
      (add-hook 'savehist-save-hook
                (lambda () (setq savehist-minibuffer-history-variables
                                 (delq 'icicle-interactive-history
                                       savehist-minibuffer-history-variables)))))
    
    ;; Just replace the original ESS definitions so that the new ones use Icicles completion.
    (defadvice ess-internal-complete-object-name (around icicle-ess-internal-complete-object-name
                                                         disable activate)
      "`ess-internal-complete-object-name', but in Icicle mode use Icicles completion."
      (if (fboundp 'cl-flet)
          (cl-flet ((comint-dynamic-simple-complete (stub candidates)
                      (icicle-comint-dynamic-simple-complete stub candidates)))
            ad-do-it)
        (flet ((comint-dynamic-simple-complete (stub candidates)
                 (icicle-comint-dynamic-simple-complete stub candidates)))
          ad-do-it)))

    (defadvice ess-complete-filename (around icicle-ess-complete-filename disable activate)
      "`ess-complete-filename', but in Icicle mode use Icicles completion."
      (if (fboundp 'cl-flet)
          (cl-flet ((comint-dynamic-complete-filename (&optional replace-to-eol-p)
                      (icicle-comint-dynamic-complete-filename replace-to-eol-p))
                    (comint-replace-by-expanded-filename () ; This one is not used for recent ESS versions.
                      (icicle-comint-replace-by-expanded-filename)))
            ad-do-it)
        (flet ((comint-dynamic-complete-filename (&optional replace-to-eol-p)
                 (icicle-comint-dynamic-complete-filename replace-to-eol-p))
               (comint-replace-by-expanded-filename () ; This one is not used for recent ESS versions.
                 (icicle-comint-replace-by-expanded-filename)))
          ad-do-it)))

    (defadvice ess-R-complete-object-name (around icicle-ess-R-complete-object-name disable activate)
      "`ess-R-complete-object-name', but in Icicle mode use Icicles completion."
      (if (fboundp 'cl-flet)
          (cl-flet ((comint-dynamic-simple-complete (stub candidates)
                      (icicle-comint-dynamic-simple-complete stub candidates)))
            ad-do-it)
        (flet ((comint-dynamic-simple-complete (stub candidates)
                 (icicle-comint-dynamic-simple-complete stub candidates)))
          ad-do-it)))

    (defadvice ess-completing-read (around icicle-ess-completing-read disable activate)
      "Make `ess-completing-read' use Icicles completion in Icicle mode."
      (let ((ess-use-ido  (or icicle-ess-use-ido  nil)))
        ad-do-it)))

  (when (> emacs-major-version 21)
    (defadvice describe-face (before icicle-WYSIWYG-&-preds activate)
      "Respect `icicle-WYSIWYG-Completions-flag'.  Provide preds for `M-&'.
1. If `icicle-WYSIWYG-Completions-flag' is non-nil, then completion
   does not use `completing-read-multiple' (which cannot take advantage
   of WYSIWYG).
2. During completion, `M-&' offers face predicates for narrowing."
      (interactive
       (let ((icicle-face-completing-p  t))
         (list (read-face-name "Describe face" (if (> emacs-major-version 23)
                                                   (or  (if (and (= emacs-major-version 24)
                                                                 (< emacs-minor-version 4))
                                                            (face-at-point)
                                                          (face-at-point t))
                                                       'default)
                                                 "= `default' face")
                               (not icicle-WYSIWYG-Completions-flag)))))))
  
  ;; Eval this so that even if the library is byte-compiled with Emacs 20,
  ;; loading it into Emacs 21+ will define variable `icicle-mode'.
  (eval '(define-minor-mode icicle-mode
          "Icicle mode: Toggle minibuffer input completion and cycling.
Non-nil prefix ARG turns mode on if ARG > 0, else turns it off.
Icicle mode is a global minor mode.  It binds keys in the minibuffer.

You can use `customize-group Icicles' or `C-u customize-mode
icicle-mode' to customize Icicles options and faces.

For more information, use `\\<minibuffer-local-completion-map>\\[icicle-minibuffer-help]' \
when the minibuffer is active.

Depending on your platform, if you use Icicles in a text terminal
\(that is, without a window system/manager), you might need to change
some of the key bindings if some of the default bindings are not
available to you.

Icicle mode defines many top-level commands.  For a list, see the
Commentary headers of files `icicles-cmd1.el' and `icicles-cmd2.el'."
          :global t :group 'Icicles :lighter " Icy" :init-value nil
          (cond (icicle-mode
                 ;; (when (interactive-p)
                 ;;   (unless (or window-system  (and (fboundp 'daemonp)  (daemonp)))
                 ;;     (icicle-with-help-window "*Attention*"
                 ;;       (princ "You are using Icicles in a text terminal (no window ")
                 ;;       (princ "system/manager).\n\nIcicles makes use of many keys that are ")
                 ;;      (princ "unavailable when running\nEmacs in a text terminal.  You will ")
                 ;;       (princ "want to rebind those keys.\n")
                 ;;       (princ "See the Icicles doc, section Key Bindings.\n"))
                 ;;  (message "Icicles uses keys that might not be suitable for a text terminal")
                 ;;     (sit-for 5)))
                 (icicle-define-icicle-maps)
                 (icicle-bind-other-keymap-keys)
                 (add-hook 'minibuffer-setup-hook       'icicle-minibuffer-setup)
                 (add-hook 'minibuffer-exit-hook        'icicle-cancel-Help-redirection)
                 (add-hook 'minibuffer-exit-hook        'icicle-restore-region-face)
                 (add-hook 'minibuffer-exit-hook        'icicle-unhighlight-lighter)
                 (add-hook 'icicle-post-command-hook    'icicle-activate-mark 'append)
                 (add-hook 'icicle-post-command-hook    'icicle-show-current-help-in-mode-line 'append)
                 (add-hook 'completion-setup-hook       'icicle-set-calling-cmd 'append)
                 (when icicle-customize-save-flag
                   (add-hook 'kill-emacs-hook           'icicle-command-abbrev-save))
                 (add-hook 'comint-mode-hook            'icicle-comint-hook-fn)
                 (add-hook 'compilation-mode-hook       'icicle-compilation-hook-fn)
                 (add-hook 'compilation-minor-mode-hook 'icicle-compilation-hook-fn)
                 ;; $$$$$$ Do this only in `icicle-display-candidates-in-Completions' now.
                 ;; $$$$$$ (add-hook 'temp-buffer-show-hook       'icicle-fit-completions-window)
                 (icicle-undo-std-completion-faces)
                 (icicle-redefine-std-completion-fns)
                 (icicle-redefine-standard-functions)
                 (icicle-redefine-standard-options)
                 (icicle-redefine-standard-widgets)
                 (when (ad-find-some-advice 'describe-face 'before 'icicle-respect-WYSIWYG)
                   (ad-enable-advice 'describe-face 'before 'icicle-respect-WYSIWYG)
                   (unless (ad-is-active 'describe-face) (ad-activate 'describe-face)))
                 (when (ad-find-some-advice 'ess-internal-complete-object-name 'around
                                            'icicle-ess-internal-complete-object-name)
                   (ad-enable-advice 'ess-internal-complete-object-name 'around
                                     'icicle-ess-internal-complete-object-name)
                   (unless (ad-is-active 'ess-internal-complete-object-name)
                     (ad-activate 'ess-internal-complete-object-name)))
                 (when (ad-find-some-advice 'ess-complete-filename 'around
                                            'icicle-ess-complete-filename)
                   (ad-enable-advice 'ess-complete-filename 'around 'icicle-ess-complete-filename)
                   (unless (ad-is-active 'ess-complete-filename) (ad-activate 'ess-complete-filename)))
                 (when (ad-find-some-advice 'ess-R-complete-object-name 'around
                                            'icicle-ess-R-complete-object-name)
                   (ad-enable-advice 'ess-R-complete-object-name 'around
                                     'icicle-ess-R-complete-object-name)
                   (unless (ad-is-active 'ess-R-complete-object-name)
                     (ad-activate 'ess-R-complete-object-name)))
                 (when (ad-find-some-advice 'ess-completing-read 'around 'icicle-ess-completing-read)
                   (ad-enable-advice 'ess-completing-read 'around 'icicle-ess-completing-read)
                   (unless (ad-is-active 'ess-completing-read) (ad-activate 'ess-completing-read)))
                 (when (fboundp 'minibuffer-depth-indicate-mode) ; In `mb-depth(+).el'
                   (minibuffer-depth-indicate-mode 99))
                 (if icicle-menu-items-to-history-flag
                     (add-hook 'pre-command-hook 'icicle-add-menu-item-to-cmd-history)
                   (remove-hook 'pre-command-hook 'icicle-add-menu-item-to-cmd-history))
                 (when (> emacs-major-version 22)
                   (when icicle-populate-interactive-history-flag
                     (ad-enable-advice 'call-interactively 'after 'icicle-save-to-history))
                   (ad-activate 'call-interactively))
                 (dolist (fn  icicle-inhibit-advice-functions)
                   (when (and (fboundp fn)  (ad-is-active fn))
                     (push (cons fn (ad-copy-advice-info fn)) icicle-advice-info-list)
                     (ad-deactivate fn))))
                (t
                 (makunbound 'icicle-mode-map)
                 (icicle-restore-other-keymap-keys)
                 (remove-hook 'minibuffer-setup-hook    'icicle-minibuffer-setup)
                 (remove-hook 'minibuffer-exit-hook     'icicle-cancel-Help-redirection)
                 (remove-hook 'minibuffer-exit-hook     'icicle-restore-region-face)
                 (remove-hook 'icicle-post-command-hook 'icicle-activate-mark)
                 (remove-hook 'icicle-post-command-hook 'icicle-show-current-help-in-mode-line)
                 ;; The pre- and post-command hooks are local to the minibuffer,
                 ;; So they are added in `icicle-minibuffer-setup', not here.
                 ;; Nevertheless, they are removed here when Icicle mode is exited.
                 (remove-hook 'pre-command-hook         'icicle-top-level-prep)
                 (remove-hook 'pre-command-hook         'icicle-run-icicle-pre-command-hook t)
                 (remove-hook 'post-command-hook        'icicle-run-icicle-post-command-hook t)
                 (remove-hook 'completion-setup-hook    'icicle-set-calling-cmd)
                 (remove-hook 'kill-emacs-hook          'icicle-command-abbrev-save)
                 (remove-hook 'comint-mode-hook         'icicle-comint-hook-fn)
                 (remove-hook 'compilation-mode-hook    'icicle-compilation-hook-fn)
                 (remove-hook 'compilation-minor-mode-hook 'icicle-compilation-hook-fn)
                 ;; $$$$$$ Do this only in `icicle-display-candidates-in-Completions' now.
                 ;; $$$$$$ (remove-hook 'temp-buffer-show-hook    'icicle-fit-completions-window)

                 ;; $$ Should restore standard completion faces here.
                 (icicle-restore-std-completion-fns)
                 (icicle-restore-standard-functions)
                 (icicle-restore-standard-options)
                 (icicle-restore-standard-widgets)
                 (when (ad-find-some-advice 'describe-face 'before 'icicle-respect-WYSIWYG)
                   (ad-disable-advice 'describe-face 'before 'icicle-respect-WYSIWYG))
                 (when (ad-find-some-advice 'ess-internal-complete-object-name 'around
                                            'icicle-ess-internal-complete-object-name)
                   (ad-disable-advice 'ess-internal-complete-object-name 'around
                                      'icicle-ess-internal-complete-object-name))
                 (when (ad-find-some-advice 'ess-complete-filename 'around
                                            'icicle-ess-complete-filename)
                   (ad-disable-advice 'ess-complete-filename 'around 'icicle-ess-complete-filename))
                 (when (ad-find-some-advice 'ess-R-complete-object-name 'around
                                            'icicle-ess-R-complete-object-name)
                   (ad-disable-advice 'ess-R-complete-object-name 'around
                                      'icicle-ess-R-complete-object-name))
                 (when (ad-find-some-advice 'ess-completing-read 'around 'icicle-ess-completing-read)
                   (ad-disable-advice 'ess-completing-read 'around 'icicle-ess-completing-read))
                 (when (fboundp 'minibuffer-depth-indicate-mode)
                   (minibuffer-depth-indicate-mode -99))
                 (remove-hook 'pre-command-hook 'icicle-add-menu-item-to-cmd-history)
                 (when (> emacs-major-version 22)
                   (ad-disable-advice 'call-interactively 'after 'icicle-save-to-history)
                   (ad-activate 'call-interactively))
                 (dolist (fn  icicle-inhibit-advice-functions)
                   (let ((info  (memq fn icicle-advice-info-list)))
                     (when (and (fboundp fn)  info)
                       (ad-set-advice-info fn info)
                       (when (ad-is-active fn) (ad-activate fn)))))))
          (unless (eq icicle-guess-commands-in-path 'load)
            (setq icicle-shell-command-candidates-cache  ())) ; Reset - toggle Icy to update.
          (message "Turning %s Icicle mode..."
           (icicle-propertize (if icicle-mode "ON" "OFF") 'face 'icicle-msg-emphasis))
          (icicle-define-minibuffer-maps icicle-mode)
          (run-hooks 'icicle-mode-hook)
          (message "Turning %s Icicle mode...done"
           (icicle-propertize (if icicle-mode "ON" "OFF") 'face 'icicle-msg-emphasis))))

  ;; Do this so users can do `C-u customize-mode icicle-mode'.  See Emacs bugs #11299 and #11301.
  (put 'icicle-mode 'custom-mode-group 'Icicles))

(unless (fboundp 'define-minor-mode)    ; Emacs 20 ------------
  (defun icicle-mode (&optional arg)
    "Icicle mode: Toggle minibuffer input completion and cycling.
Non-nil prefix ARG turns mode on if ARG > 0, else turns it off.
Icicle mode is a global minor mode.  It binds keys in the minibuffer.

For more information, use `\\<minibuffer-local-completion-map>\\[icicle-minibuffer-help]' \
when the minibuffer is active.

Depending on your platform, if you use Icicles in a text terminal
\(that is, without a window system/manager), you might need to change
some of the key bindings if some of the default bindings are not
available to you.

Icicle mode defines many top-level commands.  For a list, see the
Commentary headers of files `icicles-cmd1.el' and `icicles-cmd2.el'."
    (interactive "P")
    (setq icicle-mode  (if arg (> (prefix-numeric-value arg) 0) (not icicle-mode)))
    (icicle-define-minibuffer-maps icicle-mode)
    (cond (icicle-mode
           ;; (when (interactive-p)
           ;;   (unless (or window-system  (and (fboundp 'daemonp)  (daemonp)))
           ;;     (icicle-with-help-window "*Attention*"
           ;;       (princ "You are using Icicles in a text terminal (no window ")
           ;;       (princ "system/manager).\n\nIcicles makes use of many keys that are ")
           ;;       (princ "unavailable when running\nEmacs in a text terminal.  You will ")
           ;;       (princ "want to rebind those keys.\n")
           ;;       (princ "See the Icicles doc, section Key Bindings.\n"))
           ;;     (message "Icicles uses keys that might not be suitable for a text terminal")
           ;;     (sit-for 5)))
           (icicle-define-icicle-maps)
           (icicle-bind-other-keymap-keys)
           ;; This is not really necessary after the first time - no great loss.
           (add-hook 'minibuffer-setup-hook       'icicle-minibuffer-setup)
           (add-hook 'minibuffer-exit-hook        'icicle-cancel-Help-redirection)
           (add-hook 'minibuffer-exit-hook        'icicle-restore-region-face)
           (add-hook 'minibuffer-exit-hook        'icicle-unhighlight-lighter)
           (add-hook 'icicle-post-command-hook    'icicle-activate-mark 'append)
           (add-hook 'icicle-post-command-hook    'icicle-show-current-help-in-mode-line 'append)
           (add-hook 'completion-setup-hook       'icicle-set-calling-cmd 'append)
           (when icicle-customize-save-flag
             (add-hook 'kill-emacs-hook           'icicle-command-abbrev-save))
           (add-hook 'comint-mode-hook            'icicle-comint-hook-fn)
           (add-hook 'compilation-mode-hook       'icicle-compilation-hook-fn)
           (add-hook 'compilation-minor-mode-hook 'icicle-compilation-hook-fn)
           ;; $$$$$$ Do this only in `icicle-display-candidates-in-Completions' now.
           ;; $$$$$$ (add-hook 'temp-buffer-show-hook       'icicle-fit-completions-window)
           (icicle-redefine-std-completion-fns)
           (icicle-redefine-standard-functions)
           (icicle-redefine-standard-options)
           (icicle-redefine-standard-widgets)
           (if icicle-menu-items-to-history-flag
               (add-hook 'pre-command-hook 'icicle-add-menu-item-to-cmd-history)
             (remove-hook 'pre-command-hook 'icicle-add-menu-item-to-cmd-history))
           (dolist (fn  icicle-inhibit-advice-functions)
             (when (and (fboundp fn)  (ad-is-active fn))
               (push (cons fn (ad-copy-advice-info fn)) icicle-advice-info-list)
               (ad-deactivate fn)))
           (run-hooks 'icicle-mode-hook)
           (message "Icicle mode is now ON"))
          (t
           (makunbound 'icicle-mode-map)
           (icicle-restore-other-keymap-keys)
           (remove-hook 'minibuffer-setup-hook    'icicle-minibuffer-setup)
           (remove-hook 'minibuffer-exit-hook     'icicle-cancel-Help-redirection)
           (remove-hook 'minibuffer-exit-hook     'icicle-restore-region-face)
           (remove-hook 'icicle-post-command-hook 'icicle-activate-mark)
           (remove-hook 'icicle-post-command-hook 'icicle-show-current-help-in-mode-line)
           ;; The pre- and post-command hooks are local to the minibuffer,
           ;; So they are added in `icicle-minibuffer-setup', not here.
           ;; Nevertheless, they are removed here when Icicle mode is exited.
           (remove-hook 'pre-command-hook         'icicle-top-level-prep)
           (remove-hook 'pre-command-hook         'icicle-run-icicle-pre-command-hook t)
           (remove-hook 'post-command-hook        'icicle-run-icicle-post-command-hook t)
           (remove-hook 'completion-setup-hook    'icicle-set-calling-cmd)
           (remove-hook 'kill-emacs-hook          'icicle-command-abbrev-save)
           (remove-hook 'comint-mode-hook         'icicle-comint-hook-fn)
           (remove-hook 'compilation-mode-hook    'icicle-compilation-hook-fn)
           (remove-hook 'compilation-minor-mode-hook 'icicle-compilation-hook-fn)
           ;; $$$$$$ Do this only in `icicle-display-candidates-in-Completions' now.
           ;; $$$$$$ (remove-hook 'temp-buffer-show-hook    'icicle-fit-completions-window)
           (icicle-restore-std-completion-fns)
           (icicle-restore-standard-functions)
           (icicle-restore-standard-options)
           (icicle-restore-standard-widgets)
           (unless (eq icicle-guess-commands-in-path 'load)
             (setq icicle-shell-command-candidates-cache  ())) ; Reset - toggle Icy to update.
           (remove-hook 'pre-command-hook 'icicle-add-menu-item-to-cmd-history)
           (dolist (fn  icicle-inhibit-advice-functions)
             (let ((info  (memq fn icicle-advice-info-list)))
               (when (and (fboundp fn)  info)
                 (ad-set-advice-info fn info)
                 (when (ad-is-active fn) (ad-activate fn)))))
           (run-hooks 'icicle-mode-hook)
           (message "Icicle mode is now OFF")))

    (add-to-list 'minor-mode-alist '(icicle-mode " Icy"))))


;; `Minibuf' > `Save/Retrieve Candidates' Menu------------------------
(defvar icicle-minibuf-save-retrieve-menu-map (make-sparse-keymap)
  "`Minibuf' > `Save/Retrieve Candidates' submenu.")

(define-key icicle-minibuf-save-retrieve-menu-map [icicle-saved-completion-candidates]
  '(menu-item "Swap Saved and Current Sets" icicle-candidate-set-swap
    :enable icicle-saved-completion-candidates
    :help "Swap the saved and current sets of completion candidates"))
(define-key icicle-minibuf-save-retrieve-menu-map [icicle-candidate-set-save-more-selected]
  '(menu-item "Save More Selected (Region) Candidates"
    icicle-candidate-set-save-more-selected
    :help "Add the candidates in the region to the saved candidates"))
(define-key icicle-minibuf-save-retrieve-menu-map [icicle-candidate-set-save-selected]
  '(menu-item "Save Selected (Region) Candidates"
    icicle-candidate-set-save-selected
    :help "Save the candidates in the region, for later recall"))
(define-key icicle-minibuf-save-retrieve-menu-map [icicle-candidate-set-save-more]
  '(menu-item "Save More Candidates" icicle-candidate-set-save-more
    :help "Add current completion candidates to saved candidates set"))
(define-key icicle-minibuf-save-retrieve-menu-map [icicle-candidate-set-save-persistently]
  '(menu-item "    to Cache File..." icicle-candidate-set-save-persistently
    :help "Save current completion candidates to a cache file, for later recall"))
(define-key icicle-minibuf-save-retrieve-menu-map [icicle-candidate-set-save-to-variable]
  '(menu-item "    to Variable..." icicle-candidate-set-save-to-variable
    :help "Save current completion candidates to a variable, for later recall"))
(define-key icicle-minibuf-save-retrieve-menu-map [icicle-candidate-set-save]
  '(menu-item "Save Candidates" icicle-candidate-set-save
    :help "Save the set of current completion candidates, for later recall"))
(define-key icicle-minibuf-save-retrieve-menu-map [icicle-candidate-set-retrieve-more]
  '(menu-item "Retrieve More Saved Candidates" icicle-candidate-set-retrieve-more
    :help "Add saved candidates to current completion candidates"))
(define-key icicle-minibuf-save-retrieve-menu-map [icicle-candidate-set-retrieve-persistent]
  '(menu-item "    from Cache File..."
    icicle-candidate-set-retrieve-persistent
    :help "Retrieve saved completion candidates from a cache file, making them current"))
(define-key icicle-minibuf-save-retrieve-menu-map [icicle-candidate-set-retrieve-from-variable]
  '(menu-item "    from Variable..." icicle-candidate-set-retrieve-from-variable
    :help "Retrieve saved completion candidates from variable, making them current"))
(define-key icicle-minibuf-save-retrieve-menu-map [icicle-candidate-set-retrieve]
  '(menu-item "Retrieve Saved Candidates" icicle-candidate-set-retrieve
    :enable icicle-saved-completion-candidates
    :help "Retrieve the saved set of completion candidates, making it current"))


;; `Minibuf' > `Candidate Set' Menu-----------------------------------

(defvar icicle-minibuf-candidate-set-menu-map (make-sparse-keymap)
  "`Minibuf' > `Candidate Set' submenu.")

(define-key icicle-minibuf-candidate-set-menu-map [icicle-candidate-set-define]
  '(menu-item "Define Candidates by Lisp Sexp" icicle-candidate-set-define
    :help "Define the set of current completion candidates by evaluating a sexp"))
(define-key icicle-minibuf-candidate-set-menu-map [icicle-keep-only-past-inputs]
  '(menu-item "Keep Only Previously Entered" icicle-keep-only-past-inputs
    :enable (and icicle-completion-candidates  (consp (symbol-value minibuffer-history-variable)))
    :help "Removed candidates that you have not previously chosen and entered"))
(define-key icicle-minibuf-candidate-set-menu-map [icicle-candidate-set-union]
  '(menu-item "Add (Union) Saved Candidate Set" icicle-candidate-set-union
    :enable icicle-saved-completion-candidates
    :help "Set union between the current and saved completion candidates"))
(define-key icicle-minibuf-candidate-set-menu-map [icicle-candidate-set-difference]
  '(menu-item "Subtract Saved Candidate Set" icicle-candidate-set-difference
    :enable icicle-saved-completion-candidates
    :help "Set difference between the current and saved completion candidates"))
(define-key icicle-minibuf-candidate-set-menu-map [icicle-candidate-set-intersection]
  '(menu-item "Intersect Saved Candidate Set" icicle-candidate-set-intersection
    :enable icicle-saved-completion-candidates
    :help "Set intersection between the current and saved candidates"))
(define-key icicle-minibuf-candidate-set-menu-map [icicle-save-predicate-to-variable]
  '(menu-item "Save Predicate to Variable" icicle-save-predicate-to-variable
    :help "Save the current completion predicate to a variable"))
(define-key icicle-minibuf-candidate-set-menu-map [icicle-narrow-candidates-with-predicate]
  '(menu-item "Satisfy Also Predicate..." icicle-narrow-candidates-with-predicate
    :help "Match another input pattern (narrow completions set)"
    :enable icicle-completion-candidates))
(define-key icicle-minibuf-candidate-set-menu-map [icicle-narrow-candidates]
  '(menu-item "Match Also Regexp..." icicle-narrow-candidates
    :enable icicle-completion-candidates
    :help "Match another input pattern (narrow completions set)"))
(define-key icicle-minibuf-candidate-set-menu-map [icicle-widen-candidates]
  '(menu-item "Match Alternative..." icicle-widen-candidates
    :enable icicle-completion-candidates
    :help "Match alternative input pattern (widen completions set)"))
(define-key icicle-minibuf-candidate-set-menu-map [icicle-candidate-set-complement]
  '(menu-item "Complement Candidates" icicle-candidate-set-complement
    :help "Complement the set of current completion candidates"))


;; `Minibuf' > `Act on All Candidates' Menu---------------------------

(defvar icicle-minibuf-act-on-all-menu-map (make-sparse-keymap)
  "`Minibuf' > `Act on All Candidates' submenu.")

(define-key icicle-minibuf-act-on-all-menu-map [icicle-all-candidates-list-alt-action]
  '(menu-item "Alternate Act on List" icicle-all-candidates-list-alt-action
    :help "Apply the alternative action to the list of matching completion candidates"
    :enable icicle-all-candidates-list-alt-action-fn))
(define-key icicle-minibuf-act-on-all-menu-map [icicle-all-candidates-alt-action]
  '(menu-item "Alternate Act on Each" icicle-all-candidates-alt-action
    :help "Apply the alternative action to each matching completion candidates"
    :enable icicle-candidate-alt-action-fn))
(define-key icicle-minibuf-act-on-all-menu-map [separator-alt] '("--"))
(define-key icicle-minibuf-act-on-all-menu-map [icicle-all-candidates-list-action]
  '(menu-item "Act on List" icicle-all-candidates-list-action
    :help "Apply the command action to the list of matching completion candidates"
    :enable icicle-all-candidates-list-action-fn))
(define-key icicle-minibuf-act-on-all-menu-map [icicle-all-candidates-action]
  '(menu-item "Act on Each" icicle-all-candidates-action
    :help "Apply the command action to each matching completion candidates"
    :enable icicle-candidate-action-fn))


;; `Minibuf' > `History' Menu--------------------------------------------

(defvar icicle-minibuf-history-menu-map (make-sparse-keymap)
  "`Minibuf' > `History' submenu.")

(define-key icicle-minibuf-history-menu-map [icicle-clear-current-history]
  '(menu-item "Clear History Entries" icicle-clear-current-history
    :help "Clear current minibuffer history of selected entries" :keys "M-K"))
(define-key icicle-minibuf-history-menu-map [icicle-erase-minibuffer]
  '(menu-item "Delete from History" icicle-erase-minibuffer-or-history-element
    :visible (memq last-command
              '(previous-history-element next-history-element
                icicle-erase-minibuffer-or-history-element
                previous-matching-history-element next-matching-history-element))
    :help "Delete current history element (in minibuffer now)" :keys "M-k"))
(define-key icicle-minibuf-history-menu-map [history-isearch-forward]
  '(menu-item "Isearch History Forward" isearch-forward
    :help "Incrementally search minibuffer history forward"))
(define-key icicle-minibuf-history-menu-map [history-isearch-backward]
  '(menu-item "Isearch History Backward" isearch-backward
    :help "Incrementally search minibuffer history backward"))
(define-key icicle-minibuf-history-menu-map [history-next]
  '(menu-item "Next History Item" next-history-element
    :help "Put next minibuffer history element in the minibuffer"))
(define-key icicle-minibuf-history-menu-map [history-previous]
  '(menu-item "Previous History Item" previous-history-element
    :help "Put previous minibuffer history element in the minibuffer"))


;; `Minibuf' > `Edit' Menu--------------------------------------------

(defvar icicle-minibuf-edit-menu-map (make-sparse-keymap)
  "`Minibuf' > `Edit' submenu.")

(define-key icicle-minibuf-edit-menu-map [icicle-delete-history-element]
  '(menu-item "Clear (Erase) Minibuffer" icicle-erase-minibuffer-or-history-element
    :visible (not (memq last-command
                   '(previous-history-element next-history-element
                     icicle-erase-minibuffer-or-history-element
                     previous-matching-history-element next-matching-history-element)))
    :help "Erase the Minibuffer" :keys "M-k"))
(define-key icicle-minibuf-edit-menu-map [icicle-goto/kill-failed-input]
  '(menu-item "Cursor to Mismatch (Repeat: Kill)" icicle-goto/kill-failed-input
    :visible icicle-completing-p
    :enable (and (overlayp icicle-input-completion-fail-overlay)
             (overlay-start icicle-input-completion-fail-overlay))
    :help "Put cursor where input fails to complete - repeat to kill mismatch"))
(define-key icicle-minibuf-edit-menu-map [icicle-retrieve-next-input]
  '(menu-item "Restore Next Completion Input" icicle-retrieve-next-input
    :visible icicle-completing-p
    :enable (consp (symbol-value (if (icicle-file-name-input-p)
                                     'icicle-previous-raw-file-name-inputs
                                   'icicle-previous-raw-non-file-name-inputs)))
    :help "Cycle forward to insert a previous completion input in the minibuffer (`C-u': \
complete)"))
(define-key icicle-minibuf-edit-menu-map [icicle-retrieve-previous-input]
  '(menu-item "Restore Previous Completion Input" icicle-retrieve-previous-input
    :visible icicle-completing-p
    :enable (consp (symbol-value (if (icicle-file-name-input-p)
                                     'icicle-previous-raw-file-name-inputs
                                   'icicle-previous-raw-non-file-name-inputs)))
    :help "Cycle backward to insert a previous completion input in the minibuffer (`C-u': \
complete)"))
(define-key icicle-minibuf-edit-menu-map [icicle-regexp-quote-input]
  '(menu-item "Regexp-Quote Input" icicle-regexp-quote-input
    :enable (with-current-buffer (window-buffer (minibuffer-window))
              (goto-char (icicle-minibuffer-prompt-end))  (not (eobp)))
    :help "Regexp-quote current input or its active region, then apropos-complete" :keys "M-%"))
(define-key icicle-minibuf-edit-menu-map [icicle-insert-key-description]
  '(menu-item "Insert Key Description" icicle-insert-key-description
    :visible (not icicle-searching-p) :keys "M-q"
    :help "Read key and insert its description - e.g., reading ^F inserts `C-f'"))
(define-key icicle-minibuf-edit-menu-map [icicle-roundup]
  '(menu-item "Insert Completion Candidate(s)" icicle-roundup
    :visible icicle-completing-p
    :enable icicle-completion-candidates
    :help "Insert one or more completion candidates in the minibuffer" :keys "M-r"))
(define-key icicle-minibuf-edit-menu-map [icicle-insert-string-from-a-var]
  '(menu-item "Insert String from a Variable..." icicle-insert-string-from-variable
    :visible current-prefix-arg :keys "C-="
    :help "Read a variable name and insert its string value into the minibuffer"))
(define-key icicle-minibuf-edit-menu-map [icicle-insert-string-from-std-var]
  '(menu-item "Insert `icicle-input-string'" icicle-insert-string-from-variable
    :visible (not current-prefix-arg) :keys "C-="
    :help "Insert text from variable `icicle-input-string' into the minibuffer"))
(define-key icicle-minibuf-edit-menu-map [icicle-completing-read+insert]
  '(menu-item "Insert On-Demand Completion" icicle-completing-read+insert
    :visible (consp icicle-completing-read+insert-candidates)
    :help "Read and insert something using (lax) completion"))
(define-key icicle-minibuf-edit-menu-map [icicle-read+insert-file-name]
  '(menu-item "Insert File Name" icicle-read+insert-file-name
    :help "Read and insert a file name using (lax) completion"))
(define-key icicle-minibuf-edit-menu-map [icicle-insert-history-element]
  '(menu-item "Insert Past Input(s) using Completion" icicle-insert-history-element
    :enable (consp (symbol-value minibuffer-history-variable))
    :help "Use completion to insert a previous input into the minibuffer"))
(define-key icicle-minibuf-edit-menu-map [icicle-insert-list-join-string]
  '(menu-item "Insert Join-String" icicle-insert-list-join-string
    :visible icicle-multi-completing-p
    :help "Insert `icicle-list-join-string' into the minibuffer" :keys "C-M-j"))
(define-key icicle-minibuf-edit-menu-map [icicle-insert-string-at-point]
  '(menu-item "Insert Text from Point" icicle-insert-string-at-point
    :help "Insert text at or near the cursor into the minibuffer"))



(defun icicle-add-menu-item-to-cmd-history ()
  "Add `this-command' to command history, if it is a menu item.
Menu items that are not associated with a command symbol are ignored.
Used on `pre-command-hook'."
  (condition-case nil                   ; Just in case, since this is on `pre-command-hook'.
      (when (and (> (length (this-command-keys-vector)) 0)
                 (equal '(menu-bar) (elt (this-command-keys-vector) 0))
                 ;; Exclude uninterned symbols such as `menu-function-356'.
                 (symbolp this-command) (or (< emacs-major-version 21)  (intern-soft this-command)))
        (pushnew (symbol-name this-command) extended-command-history))
    (error nil)))

(defun icicle-top-level-prep ()
  "Do top-level stuff.  Used in `pre-command-hook'."
  ;; Reset `icicle-current-TAB-method', `icicle-completion-style-set' and `icicle-apropos-complete-match-fn'
  ;;  if temporary.
  ;; Save this top-level command as `icicle-last-top-level-command'.
  ;; Reset `icicle-candidates-alist' to ().
  (unless (> (minibuffer-depth) 0)
    (let ((TAB-method  (get 'icicle-last-top-level-command 'icicle-current-TAB-method))
          (style-set   (get 'icicle-last-top-level-command 'icicle-completion-style-set))
          (apropos-fn  (get 'icicle-last-top-level-command 'icicle-apropos-complete-match-fn)))
      (when TAB-method (setq icicle-current-TAB-method  TAB-method))
      (when style-set (setq icicle-completion-style-set  style-set))
      (when apropos-fn (setq icicle-apropos-complete-match-fn  apropos-fn)))
    (unless (memq this-command '(minibuffer-complete-and-exit icicle-minibuffer-complete-and-exit
                                 exit-minibuffer              icicle-exit-minibuffer))
      (setq icicle-last-top-level-command   this-command))
    (setq icicle-candidates-alist  ())))

(defun icicle-define-icicle-maps ()
  "Define `icicle-mode-map' and `icicle-menu-map'."
  (setq icicle-mode-map  (make-sparse-keymap)) ; Recreate it each time, to capture latest bindings.

  ;; Define `Icicles' menu-bar menu.  Create it only once: sacrifice any new bindings for speed.
  (unless icicle-menu-map
    (setq icicle-menu-map  (make-sparse-keymap "Icicles"))

    ;; End of `Icicles' menu -----------------------------------------
    (define-key icicle-menu-map [icicle-report-bug]
      '(menu-item "Send Icicles Bug Report" icicle-send-bug-report))
    (define-key icicle-menu-map [icicle-customize-icicles-group]
      '(menu-item "Customize Icicles" icicle-customize-icicles-group))
    (when (fboundp 'icicle-complete-keys)
      (define-key icicle-menu-map [icicle-complete-keys]
        '(menu-item "Show Available Keys (Complete Key)" icicle-complete-keys
          :help "Show available keys (`C-g') or complete prefix key")))
    (define-key icicle-menu-map [icicle-help]
      '(menu-item "Icicles Help" icicle-minibuffer-help
        :help "Display help for minibuffer input and completion" :keys "M-? in minibuf"))


    ;; `Icicle Options' -----------------------------------------------------
    (cond ((not icicle-touche-pas-aux-menus-flag)
           (defvar icicle-options-menu-map (make-sparse-keymap)
             "`Options' > `Icicles' submenu.")
           (define-key menu-bar-options-menu [icicles]
             (list 'menu-item "Icicles" icicle-options-menu-map :visible 'icicle-mode)))
          (t
           (defvar icicle-options-menu-map (make-sparse-keymap)
             "`Icicles' > `Icicles Options' submenu.")
           (define-key icicle-menu-map [options]
             (list 'menu-item "Icicles Options" icicle-options-menu-map))))

    (define-key icicle-options-menu-map [icicle-set-option-to-t]
      '(menu-item "+ Turn On Any Option..." icicle-set-option-to-t
        :help "Set boolean option to `t' (C-u: any user option, C--: any var)"))
    (define-key icicle-options-menu-map [icicle-reset-option-to-nil]
      '(menu-item "+ Turn Off Any Option..." icicle-reset-option-to-nil
        :help "Reset an option to `nil' (C-u: reset any variable)"))
    (define-key icicle-options-menu-map [icicle-toggle-option]
      '(menu-item "+ Toggle Any Option..." icicle-toggle-option
        :help "Toggle boolean option (C-u: any user option, C--: any var)"))


    ;; `Icicle Options' > `Toggle' ------------------------------------------
    (defvar icicle-options-toggle-menu-map (make-sparse-keymap)
      "`Toggle' submenu of Icicles options menu.")
    (define-key icicle-options-menu-map [toggle]
      (list 'menu-item "Toggle" icicle-options-toggle-menu-map :visible 'icicle-mode))

    (define-key icicle-options-toggle-menu-map [icicle-toggle-completion-mode-keys]
      '(menu-item "Completion Mode Keys" icicle-toggle-completion-mode-keys :keys "C-S-<tab>"
        :help "Toggle keys between apropos and prefix completion"))
    (define-key icicle-options-toggle-menu-map [icicle-toggle-C-for-actions]
      '(menu-item "Using `C-' for Actions" icicle-toggle-C-for-actions :keys "M-g"
        :help "Toggle option `icicle-use-C-for-actions-flag'"))
    (define-key icicle-options-toggle-menu-map [icicle-toggle-~-for-home-dir]
      (icicle-menu-bar-make-toggle icicle-use-~-for-home-dir-flag icicle-use-~-for-home-dir-flag
                                   "Using `~' for $HOME"
                                   "Using `~' for home directory is now %s"
                                   "Toggle option `icicle-use-~-for-home-dir-flag'")) ; :keys "M-~"
    (define-key icicle-options-toggle-menu-map [icicle-toggle-search-cleanup]
      '(menu-item "Icicle-Search Highlighting Cleanup" icicle-toggle-search-cleanup
        :keys "C-." :help "Toggle option `icicle-search-cleanup-flag'"))
    (define-key icicle-options-toggle-menu-map [icicle-toggle-search-replace-common-match]
      (icicle-menu-bar-make-toggle icicle-search-replace-common-match-flag
                                   icicle-search-replace-common-match-flag
                                   "Replacing Longest Common Match"
                                   "Removal of Icicles search highlighting is now %s"
                                   "Toggle option `icicle-search-replace-common-match-flag'")) ; :keys "M-;"
    (define-key icicle-options-toggle-menu-map [icicle-toggle-search-replace-whole]
      (icicle-menu-bar-make-toggle icicle-search-replace-whole-candidate-flag
                                   icicle-search-replace-whole-candidate-flag
                                   "Replacing Whole Search Hit"
                                   "Replacing whole search context is now %s"
                                   "Toggle option `icicle-search-replace-whole-candidate-flag'"))
                                        ; :keys "M-_"
    (define-key icicle-options-toggle-menu-map [icicle-toggle-search-whole-word]
      (icicle-menu-bar-make-toggle icicle-search-whole-word-flag icicle-search-whole-word-flag
                                   "Whole-Word Searching (Icicles Search)"
                                   "Whole-word searching is now %s, starting with next search"
                                   "Toggle `icicle-search-whole-word-flag'")) ; :keys "M-q"
    (define-key icicle-options-toggle-menu-map [icicle-toggle-regexp-quote]
      '(menu-item "Escaping Special Chars" icicle-toggle-regexp-quote :keys "C-`"
        :help "Toggle option `icicle-regexp-quote-flag'"))
    (define-key icicle-options-toggle-menu-map [icicle-toggle-dot]
      '(menu-item "Dot (`.') Matching Newlines Too" icicle-toggle-dot :keys "C-M-."
        :help "Toggle `icicle-dot-string' between `.' and `icicle-anychar-regexp'"))
    (define-key icicle-options-toggle-menu-map [icicle-toggle-icomplete-mode]
      '(menu-item "Icomplete Mode" icicle-toggle-icomplete-mode
        :help "Toggle Icomplete mode" :enable (featurep 'icomplete-mode) :keys "C-M-#"))
    (define-key icicle-options-toggle-menu-map [icicle-toggle-show-multi-completion]
      '(menu-item "Showing Multi-Completions" icicle-toggle-show-multi-completion
        :help "Toggle option `icicle-show-multi-completion-flag'"))
    (define-key icicle-options-toggle-menu-map [icicle-toggle-completions-format]
      '(menu-item "Horizontal/Vertical Layout" icicle-toggle-completions-format
        :keys "C-M-^" :help "Toggle `icicle-completions-format' between vertical and horizontal."))
    (define-key icicle-options-toggle-menu-map [icicle-toggle-hiding-non-matching-lines]
      '(menu-item "Hiding Non-Matching Lines"
        icicle-toggle-hiding-non-matching-lines
        :keys "C-u C-x ." :help "Toggle option `icicle-hide-non-matching-lines-flag'"))
    (define-key icicle-options-toggle-menu-map [icicle-toggle-hiding-common-match]
      '(menu-item "Hiding Common Match" icicle-toggle-hiding-common-match
        :keys "C-x ." :help "Toggle option `icicle-hide-common-match-in-Completions-flag'"))
    (define-key icicle-options-toggle-menu-map [icicle-toggle-expand-to-common-match]
      '(menu-item "Expansion to Common Match" icicle-toggle-expand-to-common-match
        :keys "C-\"" :help "Toggle option `icicle-expand-input-to-common-match'"))
    (define-key icicle-options-toggle-menu-map [icicle-toggle-ignoring-comments]
      '(menu-item "Ignoring Comments" icicle-toggle-ignoring-comments
        :keys "C-M-;" :help "Toggle option `icicle-ignore-comments-flag'"))
    (define-key icicle-options-toggle-menu-map [icicle-toggle-ignored-space-prefix]
      '(menu-item "Ignoring Space Prefix" icicle-toggle-ignored-space-prefix
        :keys "M-_" :help "Toggle option `icicle-buffer-ignore-space-prefix-flag'"))
    (define-key icicle-options-toggle-menu-map [icicle-toggle-ignored-extensions]
      '(menu-item "Ignored File Extensions" icicle-toggle-ignored-extensions
        :keys "C-." :help "Toggle respect of `completion-ignored-extensions'"))
    (define-key icicle-options-toggle-menu-map [icicle-toggle-remote-file-testing]
      '(menu-item "Remote File Handling" icicle-toggle-remote-file-testing
        :enable (not icicle-searching-p) :keys "C-^"
        :help "Toggle option `icicle-test-for-remote-files-flag'"))
    (when (> emacs-major-version 20)
      (define-key icicle-options-toggle-menu-map [icicle-toggle-angle-brackets]
        '(menu-item "Angle Brackets" icicle-toggle-angle-brackets
          :help "Toggle option `icicle-key-descriptions-use-<>-flag'")))
    (define-key icicle-options-toggle-menu-map [icicle-toggle-annotation]
      '(menu-item "Candidate Annotation"
        icicle-toggle-annotation :keys "C-x C-a"
        :help "Toggle option `icicle-show-annotations-flag': hide/show annotations"))
    (define-key icicle-options-toggle-menu-map [icicle-toggle-WYSIWYG-Completions]
      (icicle-menu-bar-make-toggle icicle-WYSIWYG-Completions-flag icicle-WYSIWYG-Completions-flag
                                   "WYSIWYG for `*Completions*'"
                                   "Using WYSIWYG for `*Completions*' display is now %s"
                                   "Toggle `icicle-WYSIWYG-Completions-flag'")) ; :keys "C-S-pause"
    (define-key icicle-options-toggle-menu-map [icicle-toggle-highlight-saved-candidates]
      '(menu-item "Highlighting Saved Candidates"
        icicle-toggle-highlight-saved-candidates :keys "S-pause"
        :help "Toggle option `icicle-highlight-saved-candidates-flag'"))
    (define-key icicle-options-toggle-menu-map [icicle-toggle-highlight-historical-candidates]
      '(menu-item "Highlighting Past Inputs"
        icicle-toggle-highlight-historical-candidates :keys "C-pause"
        :help "Toggle option `icicle-highlight-historical-candidates-flag'"))
    (define-key icicle-options-toggle-menu-map [icicle-toggle-case-sensitivity]
      '(menu-item "Case Sensitivity" icicle-toggle-case-sensitivity :keys "C-A"
        :help "Toggle `case-fold-search', `completion-ignore-case' (C-u: file & buffer too)"))
    (define-key icicle-options-toggle-menu-map [icicle-toggle-proxy-candidates]
      '(menu-item "Including Proxy Candidates" icicle-toggle-proxy-candidates
        :keys "C-M-_" :help "Toggle option `icicle-add-proxy-candidates-flag'"))
    (define-key icicle-options-toggle-menu-map [icicle-toggle-transforming]
      '(menu-item "Duplicate Removal" icicle-toggle-transforming :keys "C-$"
        :help "Toggle use of `icicle-transform-function' (default: remove dups)"))
    (define-key icicle-options-toggle-menu-map [icicle-toggle-alternative-sorting]
      '(menu-item "Alternative Sort (Swap)" icicle-toggle-alternative-sorting :keys "C-M-,"
        :help "Swap current sort order for current alternative sort order"))


    ;; `Icicle Options' > `Choose' ------------------------------------------
    (defvar icicle-options-choose-menu-map (make-sparse-keymap)
      "`Choose' submenu of Icicles options menu.")
    (define-key icicle-options-menu-map [choose]
      (list 'menu-item "Choose" icicle-options-choose-menu-map :visible 'icicle-mode))

    (when (fboundp 'doremi)
      (define-key icicle-options-choose-menu-map [icicle-doremi-increment-swank-prefix-length+]
        '(menu-item "Swank Min Match Chars - Do Re Mi"
          icicle-doremi-increment-swank-prefix-length+
          :enable (eq (icicle-current-TAB-method) 'swank) :keys "C-x 2"
          :help "Change `icicle-swank-prefix-length' incrementally"))
      (define-key icicle-options-choose-menu-map [icicle-doremi-increment-swank-timeout+]
        '(menu-item "Swank Timeout - Do Re Mi"
          icicle-doremi-increment-swank-timeout+
          :enable  (eq (icicle-current-TAB-method) 'swank) :keys "C-x 1"
          :help "Change `icicle-swank-timeout' incrementally")))
    (define-key icicle-options-choose-menu-map [icicle-next-TAB-completion-method]
      '(menu-item "`TAB' Completion Method" icicle-next-TAB-completion-method
        :keys "C-(" :help "Cycle to the next `TAB' completion method (C-u: ONE-OFF)"))
    (define-key icicle-options-choose-menu-map [icicle-next-S-TAB-completion-method]
      '(menu-item "`S-TAB' Completion Method" icicle-next-S-TAB-completion-method
        :keys "M-(" :help "Cycle to the next `S-TAB' completion method (C-u: ONE-OFF)"))
    (when (fboundp 'icicle-cycle-image-file-thumbnail)
      (define-key icicle-options-choose-menu-map [icicle-cycle-image-file-thumbnail]
        '(menu-item "Image-File Thumbnail Setting" icicle-cycle-image-file-thumbnail
          :keys "C-x t" :help "Cycle Thumbnail Image File Setting")))
    (define-key icicle-options-choose-menu-map [icicle-change-alternative-sort-order]
      '(menu-item "Alternative Sort Order" icicle-change-alternative-sort-order
        :keys "M-," :help "Choose alt sort order (C-9: reverse, C-u: cyle/complete)"))
    (define-key icicle-options-choose-menu-map [icicle-change-sort-order]
      '(menu-item "Sort Order" icicle-change-sort-order
        :enable (not icicle-inhibit-sort-p) :keys "C-,"
        :help "Choose sort order (C-9: reverse, C-u: cyle/complete)"))
    (define-key icicle-options-choose-menu-map [icicle-cycle-expand-to-common-match]
      '(menu-item "Expansion to Common Match" icicle-cycle-expand-to-common-match
        :keys "C-M-\"" :help "Choose value for option `icicle-expand-input-to-common-match'"))
    (when (fboundp 'doremi)
      (when (fboundp 'text-scale-increase) ; Emacs 23+.
        (define-key icicle-options-choose-menu-map [icicle-doremi-zoom-Completions+]
          '(menu-item "*Completions* Zoom Factor - Do Re Mi"
            icicle-doremi-zoom-Completions+
            :enable (get-buffer-window "*Completions*" 'visible) :keys "C-x -"
            :help "Zoom text in `*Completions*' incrementally")))
      (define-key icicle-options-choose-menu-map [icicle-doremi-inter-candidates-min-spaces+]
        '(menu-item "*Completions* Candidate Spacing - Do Re Mi"
          icicle-doremi-inter-candidates-min-spaces+
          :enable (get-buffer-window "*Completions*" 'visible) :keys "C-x |"
          :help "Change `icicle-inter-candidates-min-spaces' incrementally"))
      (define-key icicle-options-choose-menu-map [icicle-doremi-candidate-width-factor+]
        '(menu-item "*Completions* Column Width - Do Re Mi"
          icicle-doremi-candidate-width-factor+
          :enable (get-buffer-window "*Completions*" 'visible) :keys "C-x w"
          :help "Change `icicle-candidate-width-factor' incrementally"))
      (define-key icicle-options-choose-menu-map [icicle-doremi-increment-max-candidates+]
        '(menu-item "Max # of Completions - Do Re Mi"
          icicle-doremi-increment-max-candidates+
          :enable (active-minibuffer-window) :keys "C-x #"
          :help "Change `icicle-max-candidates' incrementally")))
    (define-key icicle-options-choose-menu-map [icicle-cycle-incremental-completion]
      '(menu-item "Incremental Completion" icicle-cycle-incremental-completion
        :keys "C-#" :help "Cycle option `icicle-incremental-completion'"))



    ;; Beginning of non-submenu `Icicles' menu -----------------------
    (define-key icicle-menu-map [icicle-separator-help] '("--"))

    (define-key icicle-menu-map [icicle-mode]
      '(menu-item "Turn Off Icicle Mode" icicle-mode))
    (define-key icicle-menu-map [icicle-top-level]
      '(menu-item "Top Level (Cancel All Minibuffers)" icicle-top-level
        :enable (active-minibuffer-window)
        :help "Cancel all minibuffers and return to the top level" :keys "C-M-T"))
    (define-key icicle-menu-map [icicle-abort]
      '(menu-item "Cancel This Minibuffer" icicle-abort-recursive-edit
        :enable (active-minibuffer-window)
        :help "Cancel this minibuffer and return to the next higher level"))
    (define-key icicle-menu-map [icicle-recomplete-from-original-domain]
      '(menu-item "Recompute Completions" icicle-recomplete-from-original-domain
        :enable (and (active-minibuffer-window)  icicle-completing-p)
        :help "Recomplete your last typed input, using the original domain."))
    (define-key icicle-menu-map [icicle-separator-levels] '("--"))

    (define-key icicle-menu-map [icicle-apply]
      '(menu-item "+ Apply Function to Alist Items..." icicle-apply
        :help "Selectively apply a function to elements in an alist"))
    (define-key icicle-menu-map [icicle-save-string-to-variable]
      '(menu-item "Save String to Variable..." icicle-save-string-to-variable
        :help "Save a string (text) you are prompted for to a variable"))
    (define-key icicle-menu-map [icicle-color-theme]
      '(menu-item "+ Choose Color Theme..." icicle-color-theme
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Change color theme (cycling etc.)"))
    (define-key icicle-menu-map [icicle-remove-saved-completion-set]
      '(menu-item "+ Remove Saved Candidate Set..." icicle-remove-saved-completion-set
        :enable icicle-saved-completion-sets
        :help "Remove an entry from `icicle-saved-completion-sets'"))
    (define-key icicle-menu-map [icicle-add/update-saved-completion-set]
      '(menu-item "Add/Update Saved Candidate Set..." icicle-add/update-saved-completion-set
        :help "Add or update an entry in `icicle-saved-completion-sets'"))
    (when (fboundp 'icicle-kmacro)      ; Emacs 22+
      (define-key icicle-menu-map [icicle-kmacro]
        '(menu-item "+ Execute Nth Keyboard Macro..." icicle-kmacro
          :enable (or (kmacro-ring-head)  kmacro-ring)
          :help "Execute a keyboard macro according to its position in `kmacro-ring'")))
    (define-key icicle-menu-map [icicle-execute-named-keyboard-macro]
      '(menu-item "+ Execute Named Keyboard Macro..." icicle-execute-named-keyboard-macro
        :help "Read the name of a keyboard macro, then execute it"))


    ;; `Frames' ----------------------------------------------------
    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-frames-menu)) ; Use `Frames' menu, defined in `menu-bar+.el'.
           (defvar icicle-frames-menu-map (make-sparse-keymap)
             "`Frames' > `Icicles' submenu.")
           (define-key menu-bar-frames-menu [icicles]
             (list 'menu-item "Icicles" icicle-frames-menu-map :visible 'icicle-mode)))
          (t
           (defvar icicle-frames-menu-map (make-sparse-keymap)
             "`Icicles' > `Frames' submenu.")
           (define-key icicle-menu-map [frames]
             (list 'menu-item "Frames" icicle-frames-menu-map))))

    (define-key icicle-frames-menu-map [icicle-font]
      '(menu-item "+ Change Font of Frame..." icicle-font
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Change font of current frame."))
    (define-key icicle-frames-menu-map [icicle-frame-fg]
      '(menu-item "+ Change Foreground of Frame..." icicle-frame-fg
        :visible (fboundp 'icicle-frame-fg) ; Requires `hexrgb.el'
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Change foreground of current frame."))
    (define-key icicle-frames-menu-map [icicle-frame-bg]
      '(menu-item "+ Change Background of Frame..." icicle-frame-bg
        :visible (fboundp 'icicle-frame-bg) ; Requires `hexrgb.el'
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Change background of current frame."))


    ;; `Customize' ---------------------------------------------------
    (cond ((not icicle-touche-pas-aux-menus-flag)
           (defvar icicle-custom-menu-map (make-sparse-keymap)
             "`Customize Emacs' > `Icicles' submenu.")
           (define-key menu-bar-custom-menu [icicles]
             (list 'menu-item "Icicles" icicle-custom-menu-map :visible 'icicle-mode)))
          (t
           (defvar icicle-custom-menu-map (make-sparse-keymap)
             "`Icicles' > `Customize' submenu.")
           (define-key icicle-menu-map [customize]
             (list 'menu-item "Customize" icicle-custom-menu-map))))

    (define-key icicle-custom-menu-map [icicle-customize-apropos]
      '(menu-item "Groups, Faces & Options Matching..." icicle-customize-apropos
        :help "Customize groups, faces, and options that match a regexp"))
    (define-key icicle-custom-menu-map [icicle-customize-face-other-window]
      '(menu-item "+ Face..." icicle-customize-face-other-window
        :help "Customize any number of faces (`C-RET')"))
    (define-key icicle-custom-menu-map [icicle-customize-apropos-faces]
      '(menu-item "Faces Matching..." icicle-customize-apropos-faces
        :help "Customize faces that match a regexp"))
    (define-key icicle-custom-menu-map [icicle-customize-apropos-opts-w-val-satisfying]
      '(menu-item "+ Options Matching with a Value Satisfying..."
        icicle-customize-apropos-opts-w-val-satisfying
        :help "Customize options whose values satisfy a predicate that match a regexp"))
    (define-key icicle-custom-menu-map [icicle-customize-apropos-options-of-type]
      '(menu-item "+ Options of Type Matching..."
        icicle-customize-apropos-options-of-type
        :help "Customize user options of a given type that match a regexp"))
    (define-key icicle-custom-menu-map [icicle-customize-apropos-options]
      '(menu-item "Options Matching..." icicle-customize-apropos-options
        :help "Customize user options that match a regexp"))
    (define-key icicle-custom-menu-map [icicle-customize-apropos-groups]
      '(menu-item "Groups Matching..." icicle-customize-apropos-groups
        :help "Customize customization groups that match a regexp"))


    ;; `Apropos' -----------------------------------------------------
    (cond ((and (not icicle-touche-pas-aux-menus-flag)  (boundp 'menu-bar-apropos-menu))
           (defvar icicle-apropos-menu-map (make-sparse-keymap)
             "`Help' > `Apropos' > `Icicles' submenu.")
           (define-key menu-bar-apropos-menu [icicles]
             (list 'menu-item "Icicles" icicle-apropos-menu-map :visible 'icicle-mode)))
          (t
           (defvar icicle-apropos-menu-map (make-sparse-keymap)
             "`Icicles' > `Apropos' submenu.")
           (define-key icicle-menu-map [apropos]
             (list 'menu-item "Apropos" icicle-apropos-menu-map))))

    (define-key icicle-apropos-menu-map [icicle-apropos-zippy]
      '(menu-item "Zippy..." icicle-apropos-zippy
        :help "Show all Zippy quotes matching a regular-expression"))
    (cond ((fboundp 'apropos-option)
           (define-key icicle-apropos-menu-map [icicle-apropos]
             '(menu-item "Symbols..." icicle-apropos
               :help "Like `apropos', but lets you see the list of matches (with `S-TAB')"))
           (define-key icicle-apropos-menu-map [icicle-apropos-vars-w-val-satisfying]
             '(menu-item "+ Variables with a Value Satisfying..." icicle-apropos-vars-w-val-satisfying
               :help "Show variables whose values satisfy a given predicate"))
           (define-key icicle-apropos-menu-map [icicle-apropos-value]
             '(menu-item "+ Variables with Values..." icicle-apropos-value
               :help "Show variables that match by name and/or value"))
           (define-key icicle-apropos-menu-map [icicle-apropos-variable]
             '(menu-item "Variables..." icicle-apropos-variable
               :help "Show variables that match PATTERN"))
           (define-key icicle-apropos-menu-map [icicle-apropos-options-of-type]
             '(menu-item "+ Options of Type..." icicle-apropos-options-of-type
               :help "Show user options (variables) of a given `defcustom' type"))
           (define-key icicle-apropos-menu-map [icicle-apropos-option]
             '(menu-item "Options..." icicle-apropos-option
               :help "Show user options (variables) that match PATTERN"))
           (define-key icicle-apropos-menu-map [icicle-apropos-function]
             '(menu-item "Functions..." icicle-apropos-function
               :help "Show functions that match PATTERN"))
           (define-key icicle-apropos-menu-map [icicle-apropos-command]
             '(menu-item "Commands..." icicle-apropos-command
               :help "Show commands that match PATTERN")))
          (t
           (define-key icicle-apropos-menu-map [icicle-apropos-vars-w-val-satisfying]
             '(menu-item "Variables with a Value Satisfying..." icicle-apropos-vars-w-val-satisfying
               :help "Show variables whose values satisfy a given predicate"))
           (define-key icicle-apropos-menu-map [icicle-apropos-value]
             '(menu-item "Variables with Values..." icicle-apropos-value
               :help "Show variables that match by name and/or value"))
           (define-key icicle-apropos-menu-map [icicle-apropos-variable]
             '(menu-item "Variables..." icicle-apropos-variable
               :help "Show variables that match PATTERN"))
           (define-key icicle-apropos-menu-map [icicle-apropos-command]
             '(menu-item "Commands..." icicle-apropos-command
               :help "Show commands that match PATTERN"))))


    ;; `Describe' ----------------------------------------------------
    (cond ((not icicle-touche-pas-aux-menus-flag)
           (defvar icicle-describe-menu-map (make-sparse-keymap)
             "`Describe' > `Icicles' submenu.")
           (define-key menu-bar-describe-menu [icicles]
             (list 'menu-item "Icicles" icicle-describe-menu-map :visible 'icicle-mode)))
          (t
           (defvar icicle-describe-menu-map (make-sparse-keymap)
             "`Icicles' > `Describe' submenu.")
           (define-key icicle-menu-map [describe]
             (list 'menu-item "Describe" icicle-describe-menu-map :visible 'icicle-mode))))

    (define-key icicle-describe-menu-map [icicle-plist]
      '(menu-item "+ Symbol with Property List..." icicle-plist
        :help "Choose a symbol and its property list"))
    (define-key icicle-describe-menu-map [icicle-doc]
      '(menu-item "+ Doc of Fun, Var, or Face..." icicle-doc
        :help "Choose documentation for a symbol"))
    (define-key icicle-describe-menu-map [icicle-fundoc]
      '(menu-item "+ Function with Name, Doc..." icicle-fundoc
        :help "Choose a function description"))
    (define-key icicle-describe-menu-map [icicle-vardoc]
      '(menu-item "+ Variable with Name, Doc..." icicle-vardoc
        :help "Choose a variable description"))
    (define-key icicle-describe-menu-map [icicle-describe-var-w-val-satisfying]
      '(menu-item "+ Variable with Value Satifying..." icicle-describe-var-w-val-satisfying
        :help "Describe a variable that satisfies a given predicate"))
    (define-key icicle-describe-menu-map [icicle-describe-option-of-type]
      '(menu-item "+ Option of Type..." icicle-describe-option-of-type
        :help "Describe a user option that was defined with a given `defcustom' type"))
    (define-key icicle-describe-menu-map [icicle-where-is]
      '(menu-item "+ Where Is..." icicle-where-is
        :help "Show keyboard/menu/mouse sequences that invoke specified command"))
    (when icicle-touche-pas-aux-menus-flag
      (define-key icicle-menu-map [icicle-separator-about] '("--")))


    ;; `Search' ------------------------------------------------------
    (cond ((not icicle-touche-pas-aux-menus-flag)
           (cond ((boundp 'menu-bar-search-tags-menu) ; `Tags' menu defined in `menu-bar+.el'.
                  (defvar icicle-search-menu-map (make-sparse-keymap)
                    "`Search' > `Icicles' menu.")
                  (define-key menu-bar-search-menu [icicles]
                    (list 'menu-item "Icicles" icicle-search-menu-map :visible 'icicle-mode)))
                 (t
                  (defvar icicle-search-menu-map (make-sparse-keymap)
                    "`Search' > `Icicles' menu.")
                  (define-key menu-bar-search-menu [icicles]
                    (list 'menu-item "Icicles" icicle-search-menu-map :visible 'icicle-mode)))))
          (t
           (defvar icicle-search-menu-map (make-sparse-keymap)
             "`Icicles' > `Icicles Search' menu.")
           (define-key icicle-menu-map [search]
             (list 'menu-item "Icicles Search" icicle-search-menu-map))))

    ;; `Go To' -------------------------------------------------------
    (cond ((not icicle-touche-pas-aux-menus-flag)
           (cond ((boundp 'menu-bar-goto-menu)
                  (defvar icicle-goto-menu-map (make-sparse-keymap)
                    "`Go To' > `Icicles' menu.")
                  (define-key menu-bar-goto-menu [icicles]
                    (list 'menu-item "Icicles" icicle-goto-menu-map)))
                 (t
                  (defvar icicle-goto-menu-map (make-sparse-keymap)
                    "`Search' > `Icicles' > `Go To' menu.")
                  (define-key icicle-search-menu-map [goto]
                    (list 'menu-item "Go To" icicle-goto-menu-map)))))
          (t
           (defvar icicle-goto-menu-map (make-sparse-keymap)
             "`Icicles' > `Go To' menu.")
           (define-key icicle-menu-map [goto]
             (list 'menu-item "Go To" icicle-goto-menu-map))))

    (define-key icicle-goto-menu-map [icicle-pop-tag-mark]
      '(menu-item "+ Back (Pop Emacs Tag Mark)" icicle-pop-tag-mark
        :enable (and (boundp 'find-tag-marker-ring)
                 (not (ring-empty-p find-tag-marker-ring))
                 (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))
        :help "Pop back to where `M-.' was last invoked"))
    (define-key icicle-goto-menu-map [icicle-find-first-tag-other-window]
      '(menu-item "+ Find First Emacs Tag ..." icicle-find-first-tag-other-window
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Find first tag in current Emacs TAGS table whose name matches your input"))
    (define-key icicle-goto-menu-map [icicle-find-tag]
      '(menu-item "+ Find Emacs Tag ..." icicle-find-tag
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Navigate among all Emacs TAGS table tags that match a regexp"))
    (define-key icicle-goto-menu-map [separator-goto-1] '("--"))

    (define-key icicle-goto-menu-map [icicle-goto-global-marker]
      '(menu-item "+ Global Marker..." icicle-goto-global-marker
        :enable (consp global-mark-ring) :keys "C-- C-x C-SPC"
        :help "Go to a global marker, choosing it by the line that includes it"))
    (define-key icicle-goto-menu-map [icicle-goto-any-marker]
      '(menu-item "+ Marker Anywhere..." icicle-goto-any-marker
        :enable (consp global-mark-ring) ; We do not use this ring, but this is a good test.
        :keys "C-0 C-SPC"
        :help "Go to a marker in any buffer, choosing it by the line that includes it"))
    (define-key icicle-goto-menu-map [icicle-goto-marker]
      '(menu-item "+ Marker in This Buffer..." icicle-goto-marker
        :enable (mark t) :keys "C-- C-SPC"
        :help "Go to a marker in this buffer, choosing it by the line that includes it"))
    (define-key icicle-goto-menu-map [icicle-select-bookmarked-region]
      '(menu-item "+ Select Bookmarked Region..." icicle-select-bookmarked-region
        :enable (featurep 'bookmark+) :keys "C-u C-x C-x"
        :help "Jump to a bookmarked region in other window, and select (activate) it"))

    ;; `Go To' > `Definition (Imenu)' menu.
    (defvar icicle-goto-imenu-menu-map (make-sparse-keymap)
      "Icicles `Definition (Imenu)' submenu of `Go To' menu.")
    (define-key icicle-goto-menu-map [imenu]
      (list 'menu-item "Definition (Imenu)" icicle-goto-imenu-menu-map
            :visible 'imenu-generic-expression))

    (define-key icicle-goto-imenu-menu-map [icicle-imenu-key-explicit-map]
      '(menu-item "+ Key in Map..." icicle-imenu-key-explicit-map
        :enable (and imenu-generic-expression  (eq major-mode 'emacs-lisp-mode))
        :help "Go to a key definition in some map, using `icicle-search'"))
    (define-key icicle-goto-imenu-menu-map [icicle-imenu-key-implicit-map]
      '(menu-item "+ Key..." icicle-imenu-key-implicit-map
        :enable (and imenu-generic-expression  (eq major-mode 'emacs-lisp-mode))
        :help "Go to a (global) key definition using `icicle-search'"))
    (define-key icicle-goto-imenu-menu-map [icicle-imenu-variable]
      '(menu-item "+ Variable..." icicle-imenu-variable
        :enable (and imenu-generic-expression  (eq major-mode 'emacs-lisp-mode))
        :help "Go to a variable definition using `icicle-search'"))
    (define-key icicle-goto-imenu-menu-map [icicle-imenu-user-option]
      '(menu-item "+ User Option..." icicle-imenu-user-option
        :enable (and imenu-generic-expression  (eq major-mode 'emacs-lisp-mode))
        :help "Go to an option definition using `icicle-search'"))
    (define-key icicle-goto-imenu-menu-map [icicle-imenu-face]
      '(menu-item "+ Face..." icicle-imenu-face
        :enable (and imenu-generic-expression  (eq major-mode 'emacs-lisp-mode))
        :help "Go to a face definition using `icicle-search'"))
    (define-key icicle-goto-imenu-menu-map [icicle-imenu-macro]
      '(menu-item "+ Macro..." icicle-imenu-macro
        :enable (and imenu-generic-expression  (eq major-mode 'emacs-lisp-mode))
        :help "Go to a Lisp macro definition using `icicle-search'"))
    (define-key icicle-goto-imenu-menu-map [icicle-imenu-non-interactive-function]
      '(menu-item "+ Non-Interactive Function..." icicle-imenu-non-interactive-function
        :enable (and imenu-generic-expression  (eq major-mode 'emacs-lisp-mode))
        :help "Go to a non-command function definition using `icicle-search'"))
    (define-key icicle-goto-imenu-menu-map [icicle-imenu-command]
      '(menu-item "+ Command..." icicle-imenu-command
        :enable (and imenu-generic-expression  (eq major-mode 'emacs-lisp-mode))
        :help "Go to a command definition using `icicle-search'"))
    (define-key icicle-goto-imenu-menu-map [icicle-imenu]
      '(menu-item "+ Any..." icicle-imenu
        :enable imenu-generic-expression :help "Go to a definition using `icicle-search'"))

    ;; `Search' menu
    (define-key icicle-search-menu-map [icicle-compilation-search]
      '(menu-item "+ Search Compilation/Grep Hits (Regexp)..."
        icicle-compilation-search
        :visible (and (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
                  (condition-case nil (eq (current-buffer) (compilation-find-buffer))
                    (error nil)))
        :keys "C-`" :help "Icicles search, showing the matching compilation-buffer hit"))
    (define-key icicle-search-menu-map [icicle-grep-saved-file-candidates]
      '(menu-item "Grep Saved File-Name Candidates..."
        icicle-grep-saved-file-candidates
        :enable icicle-saved-completion-candidates
        :help "Run `grep' on the set of completion candidates saved using `C-M->'"))
    (define-key icicle-search-menu-map [separator-search-0] '("--"))

    (define-key icicle-search-menu-map [icicle-search-highlight-cleanup]
      '(menu-item "Remove Icicle-Search Highlighting..." icicle-search-highlight-cleanup
        :enable (or icicle-search-overlays
                 (overlayp icicle-search-current-overlay)
                 (overlayp icicle-search-refined-overlays) icicle-search-refined-overlays)
        :help "Remove all highlighting from the last use of `icicle-search'"))
    (define-key icicle-search-menu-map [icicle-search-define-replacement]
      '(menu-item "Define Replacement String..." icicle-search-define-replacement
        :help "Set the replacement string for use in `icicle-search'"))
    (define-key icicle-search-menu-map [separator-search-1] '("--"))

    (define-key icicle-search-menu-map [icicle-tags-search]
      '(menu-item "+ Search Emacs-Tagged Files ..." icicle-tags-search
        :help "Search all source files listed in Emacs TAGS tables for matches for a regexp"))
    (define-key icicle-search-menu-map [icicle-search-file]
      '(menu-item "+ Search Files (Regexp)..." icicle-search-file
        :help "Search multiple files completely"))
    (define-key icicle-search-menu-map [icicle-search-buffer]
      '(menu-item "+ Search Buffers (Regexp)..." icicle-search-buffer
        :help "Search multiple buffers completely"))
    (define-key icicle-search-menu-map [separator-search-2] '("--"))

    (define-key icicle-search-menu-map [icicle-search-overlay-property]
      '(menu-item "+ Search Overlay Property..." icicle-search-overlay-property
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Search for an overlay that has a property with a certain value"))
    (define-key icicle-search-menu-map [icicle-search-char-property]
      '(menu-item "+ Search Character Property..." icicle-search-char-property
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Search for text that has a property with a certain value"))
    (define-key icicle-search-menu-map [icicle-search-text-property]
      '(menu-item "+ Search Text Property..." icicle-search-text-property
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Search for text or overlay that has a property with a certain value"))
    (define-key icicle-search-menu-map [separator-search-3] '("--"))

    (define-key icicle-search-menu-map [icicle-search-xml-element-text-node]
      '(menu-item "+ Search XML text() Nodes..." icicle-search-xml-element-text-node
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Icicles search with text() nodes of ELEMENTs as search contexts"))
    (define-key icicle-search-menu-map [icicle-search-xml-element]
      '(menu-item "+ Search XML Elements..." icicle-search-xml-element
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Icicles search with XML ELEMENTs as search contexts - you are prompted for ELEMENT"))
    (define-key icicle-search-menu-map [icicle-search-pages]
      '(menu-item "+ Search Pages..." icicle-search-pages
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Icicles search with pages as search contexts"))
    (define-key icicle-search-menu-map [icicle-search-paragraphs]
      '(menu-item "+ Search Paragraphs..." icicle-search-paragraphs
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Icicles search with paragraphs as search contexts"))
    (define-key icicle-search-menu-map [icicle-search-sentences]
      '(menu-item "+ Search Sentences..." icicle-search-sentences
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Icicles search with sentences as search contexts"))
    (define-key icicle-search-menu-map [icicle-occur]
      '(menu-item "+ Search Lines (`icicle-occur')..." icicle-occur
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Icicles search with lines as search contexts"))
    (define-key icicle-search-menu-map [icicle-search-thing]
      '(menu-item "+ Search Things..." icicle-search-thing
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Icicles search with THINGs as search contexts - you are prompted for THING"))
    (define-key icicle-search-menu-map [separator-search-4] '("--"))

    (define-key icicle-search-menu-map [icicle-search-word]
      '(menu-item "+ Search for Word..." icicle-search-word
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Whole-word Icicles search"))
    (define-key icicle-search-menu-map [icicle-search-w-isearch-string]
      '(menu-item "+ Search with Isearch Contexts..." icicle-search-w-isearch-string
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Icicles search with search contexts that match a previous Isearch string"))
    (define-key icicle-search-menu-map [icicle-search-keywords]
      '(menu-item "+ Search with Keywords (Regexps)..." icicle-search-keywords
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Icicles search with one or more keywords, which can each be a regexp"))
    (define-key icicle-search-menu-map [icicle-search]
      '(menu-item "+ Search (Regexp)..." icicle-search
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :keys "C-`"
        :help "Icicles search for regexp matches, with completion, cycling, & hit replacement"))

    ;; `M-s M-s m' - mode-specific search
    (define-key icicle-search-menu-map [icicle-search-dired-marked-recursive]
      '(menu-item "+ Search Marked, Here and Below..." icicle-search-dired-marked-recursive
        :help "Search the marked files, including those in marked subdirs"
        :visible (eq major-mode 'dired-mode)))
    (define-key icicle-search-menu-map [icicle-search-bookmark-list-marked]
      '(menu-item "+ Search Marked..." icicle-search-bookmark-list-marked
        :help "Search the target files of the marked bookmarks"
        :visible (eq major-mode 'bookmark-bmenu-mode)))
    (define-key icicle-search-menu-map [icicle-search-ibuffer-marked]
      '(menu-item "+ Search Marked..." icicle-search-ibuffer-marked
        :help "Search the marked files" :visible (eq major-mode 'ibuffer-mode)))
    (define-key icicle-search-menu-map [icicle-search-buff-menu-marked]
      '(menu-item "+ Search Marked..." icicle-search-buff-menu-marked
        :help "Search the marked files" :visible (eq major-mode 'Buffer-menu-mode)))

    ;; `Search' > `Bookmarks' menu, for searching bookmarks.
    (defvar icicle-search-bookmarks-menu-map (make-sparse-keymap)
      "Icicles `Bookmarks' submenu of `Search' menu.")
    (define-key icicle-search-menu-map [bookmarks]
      (list 'menu-item "Bookmarks" icicle-search-bookmarks-menu-map :visible 'icicle-mode))

    (define-key icicle-search-bookmarks-menu-map [icicle-search-w3m-bookmark]
      '(menu-item "+ W3M Bookmarks..." icicle-search-w3m-bookmark
        :visible (featurep 'bookmark+)
        :help "Search text of W3M bookmarks"))
    (define-key icicle-search-bookmarks-menu-map [icicle-search-url-bookmark]
      '(menu-item "+ URL Bookmarks..." icicle-search-url-bookmark
        :visible (featurep 'bookmark+)
        :help "Search text of bookmarks whose targets are defined by URLs"))
    (define-key icicle-search-bookmarks-menu-map [icicle-search-gnus-bookmark]
      '(menu-item "+ Gnus Bookmarks..." icicle-search-gnus-bookmark
        :visible (featurep 'bookmark+)
        :help "Search text of Gnus bookmarks"))
    (define-key icicle-search-bookmarks-menu-map [icicle-search-man-bookmark]
      '(menu-item "+ Man Bookmarks..." icicle-search-man-bookmark
        :visible (featurep 'bookmark+)
        :help "Search text of `man' bookmarks"))
    (define-key icicle-search-bookmarks-menu-map [icicle-search-info-bookmark]
      '(menu-item "+ Info Bookmarks..." icicle-search-info-bookmark
        :visible (featurep 'bookmark+)
        :help "Search text of Info (manual) bookmarks"))
    (define-key icicle-search-bookmarks-menu-map [icicle-search-bookmark-list-bookmark]
      '(menu-item "+ Bookmark-List Bookmarks..." icicle-search-bookmark-list-bookmark
        :visible (featurep 'bookmark+)
        :help "Search text of bookmark-list bookmarks"))
    (define-key icicle-search-bookmarks-menu-map [icicle-search-dired-bookmark]
      '(menu-item "+ Dired Bookmarks..." icicle-search-dired-bookmark
        :visible (featurep 'bookmark+)
        :help "Search buffers of Dired bookmarks"))
    (define-key icicle-search-bookmarks-menu-map [separator-search-bookmarks-6]
      '(menu-item "--" nil :visible (featurep 'bookmark+)))

    (define-key icicle-search-bookmarks-menu-map [icicle-search-some-tags-regexp-bookmark]
      '(menu-item "+ Bookmarks Tagged Some Regexp..." icicle-search-some-tags-regexp-bookmark
        :visible (featurep 'bookmark+)
        :help "Search text of bookmarks some of whose tags matches a regexp"))
    (define-key icicle-search-bookmarks-menu-map [icicle-search-all-tags-regexp-bookmark]
      '(menu-item "+ Bookmarks Tagged All Regexp..." icicle-search-all-tags-regexp-bookmark
        :visible (featurep 'bookmark+)
        :help "Search text of bookmarks all of whose tags match a regexp"))
    (define-key icicle-search-bookmarks-menu-map [icicle-search-some-tags-bookmark]
      '(menu-item "+ Bookmarks Tagged Some..." icicle-search-some-tags-bookmark
        :visible (featurep 'bookmark+)
        :help "Search text of bookmarks some of whose tags are among those tags you choose"))
    (define-key icicle-search-bookmarks-menu-map [icicle-search-all-tags-bookmark]
      '(menu-item "+ Bookmarks Tagged All..." icicle-search-all-tags-bookmark
        :visible (featurep 'bookmark+)
        :help "Search text of bookmarks all of whose tags are among those tags you choose"))
    (define-key icicle-search-bookmarks-menu-map [separator-search-bookmarks-5]
      '(menu-item "--" nil :visible (featurep 'bookmark+)))

    (define-key icicle-search-bookmarks-menu-map [separator-search-bookmarks-5]
      '(menu-item "--" nil :visible (featurep 'bookmark+)))

    (define-key icicle-search-bookmarks-menu-map [icicle-search-non-file-bookmark]
      '(menu-item "+ Non-File Bookmarks..." icicle-search-non-file-bookmark
        :visible (featurep 'bookmark+)
        :help "Search text of bookmarks whose targets are not files - e.g., buffers"))
    (define-key icicle-search-bookmarks-menu-map [icicle-search-remote-file-bookmark]
      '(menu-item "+ Remote File Bookmarks..." icicle-search-remote-file-bookmark
        :visible (featurep 'bookmark+)
        :help "Search text of bookmarks whose targets are remote files"))
    (define-key icicle-search-bookmarks-menu-map [icicle-search-local-file-bookmark]
      '(menu-item "+ Local File Bookmarks..." icicle-search-local-file-bookmark
        :visible (featurep 'bookmark+)
        :help "Search text of bookmarks whose targets are local files"))
    (define-key icicle-search-bookmarks-menu-map [icicle-search-file-bookmark]
      '(menu-item "+ File Bookmarks..." icicle-search-file-bookmark
        :visible (featurep 'bookmark+)
        :help "Search text of bookmarks whose targets are files"))
    (define-key icicle-search-bookmarks-menu-map [icicle-search-autofile-bookmark]
      '(menu-item "+ Autofile Bookmarks..." icicle-search-autofile-bookmark
        :visible (featurep 'bookmark+)
        :help "Search text of autofile bookmarks: files bookmarked under their own names"))
    (define-key icicle-search-bookmarks-menu-map [separator-search-bookmarks-3]
      '(menu-item "--" nil :visible (featurep 'bookmark+)))

    (define-key icicle-search-bookmarks-menu-map [icicle-search-specific-files-bookmark]
      '(menu-item "+ Bookmarks for Specific Files..." icicle-search-specific-files-bookmark
        :visible (featurep 'bookmark+)
        :help "Search text of bookmarks defined for specific files that you choose"))
    (define-key icicle-search-bookmarks-menu-map [icicle-search-specific-buffers-bookmark]
      '(menu-item "+ Bookmarks for Specific Buffers..." icicle-search-specific-buffers-bookmark
        :visible (featurep 'bookmark+)
        :help "Search text of bookmarks defined for specific buffers that you choose"))
    (define-key icicle-search-bookmarks-menu-map [icicle-search-this-buffer-bookmark]
      '(menu-item "+ Bookmarks for This Buffer..." icicle-search-this-buffer-bookmark
        :visible (featurep 'bookmark+)
        :help "Search text of bookmarks defined for this buffer"))
    (define-key icicle-search-bookmarks-menu-map [separator-search-bookmarks-2]
      '(menu-item "--" nil :visible (featurep 'bookmark+)))

    (define-key icicle-search-bookmarks-menu-map [icicle-search-temporary-bookmark]
      '(menu-item "+ Temporary Bookmarks..." icicle-search-temporary-bookmark
        :visible (featurep 'bookmark+)
        :help "Search text of temporary bookmarks"))
    (define-key icicle-search-bookmarks-menu-map [icicle-search-autonamed-bookmark]
      '(menu-item "+ Autonamed Bookmarks..." icicle-search-autonamed-bookmark
        :visible (featurep 'bookmark+)
        :help "Search text of autonamed bookmarks"))
    (define-key icicle-search-bookmarks-menu-map [separator-search-bookmarks-1]
      '(menu-item "--" nil :visible (featurep 'bookmark+)))

    (define-key icicle-search-bookmarks-menu-map [icicle-search-region-bookmark]
      '(menu-item "+ Bookmarked Regions..." icicle-search-region-bookmark
        :visible (featurep 'bookmark+)
        :help "Search bookmarked regions"))
    (define-key icicle-search-bookmarks-menu-map [icicle-search-bookmark]
      '(menu-item "+ Bookmarks..." icicle-search-bookmark
        :help "Search bookmarked text"))
    (define-key icicle-search-bookmarks-menu-map [icicle-search-bookmarks-together]
      '(menu-item "+ Bookmarks Together..." icicle-search-bookmarks-together
        :help "Search text of bookmarks, taken together" :keys "C-u C-`"))

    ;; `Search' > `Definitions' menu.
    (defvar icicle-search-imenu-menu-map (make-sparse-keymap)
      "Icicles `Definitions' submenu of `Search' menu.")
    (define-key icicle-search-menu-map [imenu]
      (list 'menu-item "Definitions" icicle-search-imenu-menu-map
            :visible 'imenu-generic-expression))

    (define-key icicle-search-imenu-menu-map [icicle-imenu-key-explicit-map-full]
      '(menu-item "+ Key in Map..." icicle-imenu-key-explicit-map-full
        :enable (and imenu-generic-expression  (eq major-mode 'emacs-lisp-mode))
        :help "Search a key definition in some map, using `icicle-search'"))
    (define-key icicle-search-imenu-menu-map [icicle-imenu-key-implicit-map-full]
      '(menu-item "+ Key..." icicle-imenu-key-implicit-map-full
        :enable (and imenu-generic-expression  (eq major-mode 'emacs-lisp-mode))
        :help "Search a global key definition using `icicle-search'"))
    (define-key icicle-search-imenu-menu-map [icicle-imenu-variable-full]
      '(menu-item "+ Variable..." icicle-imenu-variable-full
        :enable (and imenu-generic-expression  (eq major-mode 'emacs-lisp-mode))
        :help "Search a variable definition using `icicle-search'"))
    (define-key icicle-search-imenu-menu-map [icicle-imenu-user-option-full]
      '(menu-item "+ User Option..." icicle-imenu-user-option-full
        :enable (and imenu-generic-expression  (eq major-mode 'emacs-lisp-mode))
        :help "Search an option definition using `icicle-search'"))
    (define-key icicle-search-imenu-menu-map [icicle-imenu-face-full]
      '(menu-item "+ Face..." icicle-imenu-face-full
        :enable (and imenu-generic-expression  (eq major-mode 'emacs-lisp-mode))
        :help "Search a face definition using `icicle-search'"))
    (define-key icicle-search-imenu-menu-map [icicle-imenu-macro-full]
      '(menu-item "+ Macro..." icicle-imenu-macro-full
        :enable (and imenu-generic-expression  (eq major-mode 'emacs-lisp-mode))
        :help "Search a Lisp macro definition using `icicle-search'"))
    (define-key icicle-search-imenu-menu-map [icicle-imenu-non-interactive-function-full]
      '(menu-item "+ Non-Interactive Function..." icicle-imenu-non-interactive-function-full
        :enable (and imenu-generic-expression  (eq major-mode 'emacs-lisp-mode))
        :help "Search a non-command function definition using `icicle-search'"))
    (define-key icicle-search-imenu-menu-map [icicle-imenu-command-full]
      '(menu-item "+ Command..." icicle-imenu-command-full
        :enable (and imenu-generic-expression  (eq major-mode 'emacs-lisp-mode))
        :help "Search a command definition using `icicle-search'"))
    (define-key icicle-search-imenu-menu-map [icicle-imenu-full]
      '(menu-item "+ Any..." icicle-imenu-full
        :enable imenu-generic-expression :help "Search a definition using `icicle-search'"))


    ;; `Bookmarks' ---------------------------------------------------
    (require 'bookmark)                 ; `bookmark-buffer-name' is not autoloaded.

    ;; Icicles bookmark jump commands.
    ;;
    ;; Whether or not `icicle-top-level-key-bindings' remaps `Bookmark+' commands to `Icicles' bookmark
    ;; commands, we put the latter on menus, prefixing the menu items with `+ ', as usual.
    ;;
    (defvar icicle-bookmark-menu-map (make-sparse-keymap)
      "Menu of Icicles bookmark jump commands.")
    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-bookmark-map)) ; Use `Bookmarks' menu, if available.
           (cond ((boundp 'bmkp-jump-menu) ; Use `Bookmarks' > `Jump To' menu, if `Bookmark+'.
                  (define-key bmkp-jump-menu [icicles]
                    (list 'menu-item "Icicles" icicle-bookmark-menu-map :visible 'icicle-mode)))
                 (t                     ; Use vanilla `Bookmarks' menu.
                  (define-key menu-bar-bookmark-map [icicles]
                    (list 'menu-item "Icicles" icicle-bookmark-menu-map :visible 'icicle-mode)))))
          (t
           (define-key icicle-menu-map [bookmarks]
             (list 'menu-item "Jump to Bookmarks" icicle-bookmark-menu-map))))

    (when (featurep 'bookmark+)

      ;; Icicles `Bookmark+' jump commands not involving tags.
      (define-key icicle-bookmark-menu-map [icicle-bookmark-temporary-other-window]
        '(menu-item "+ Temporary Bookmark..." icicle-bookmark-temporary-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a temporary bookmark"))
      (define-key icicle-bookmark-menu-map [icicle-bookmark-autofile-other-window]
        '(menu-item "+ Autofile Bookmark..." icicle-bookmark-autofile-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to an autofile bookmark"))
      (define-key icicle-bookmark-menu-map [icicle-bookmark-autonamed-this-buffer-other-window]
        '(menu-item "+ Autonamed Bookmark for This Buffer..."
          icicle-bookmark-autonamed-this-buffer-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to an autonamed bookmark for this buffer"))
      (define-key icicle-bookmark-menu-map [icicle-bookmark-autonamed-other-window]
        '(menu-item "+ Autonamed Bookmark..." icicle-bookmark-autonamed-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to an autonamed bookmark"))
      (define-key icicle-bookmark-menu-map [icicle-bookmark-specific-files-other-window]
        '(menu-item "+ Bookmark for Specific Files..." icicle-bookmark-specific-files-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to bookmarks for specific files that you choose"))
      (define-key icicle-bookmark-menu-map [icicle-bookmark-specific-buffers-other-window]
        '(menu-item "+ Bookmark for Specific Buffers..."
          icicle-bookmark-specific-buffers-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to bookmarks for specific buffers that you choose"))
      (define-key icicle-bookmark-menu-map [icicle-bookmark-this-buffer-other-window]
        '(menu-item "+ Bookmark for This Buffer..." icicle-bookmark-this-buffer-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a bookmark for this buffer"))
      (define-key icicle-bookmark-menu-map [icicle-bookmark-url-other-window]
        '(menu-item "+ URL Bookmark..." icicle-bookmark-url-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a URL bookmark"))
      (define-key icicle-bookmark-menu-map [icicle-bookmark-gnus-other-window]
        '(menu-item "+ Gnus Bookmark..." icicle-bookmark-gnus-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a Gnus bookmark"))
      (define-key icicle-bookmark-menu-map [icicle-bookmark-man-other-window]
        '(menu-item "+ `man' Bookmark..." icicle-bookmark-man-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a `man'-page bookmark"))
      (define-key icicle-bookmark-menu-map [icicle-bookmark-info-other-window]
        '(menu-item "+ Info Bookmark..." icicle-bookmark-info-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to an Info bookmark"))
      (define-key icicle-bookmark-menu-map [icicle-bookmark-image-other-window]
        '(menu-item "+ Image Bookmark..." icicle-bookmark-image-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to an image bookmark"))
      (define-key icicle-bookmark-menu-map [icicle-bookmark-non-file-other-window]
        '(menu-item "+ Buffer (Non-File) Bookmark..." icicle-bookmark-non-file-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a buffer (i.e., a non-file) bookmark"))
      (define-key icicle-bookmark-menu-map [icicle-bookmark-region-other-window]
        '(menu-item "+ Region Bookmark..." icicle-bookmark-region-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a bookmark and activate its recorded region"))
      (define-key icicle-bookmark-menu-map [icicle-bookmark-remote-file-other-window]
        '(menu-item "+ Remote-File Bookmark..." icicle-bookmark-remote-file-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a remote-file bookmark"))
      (define-key icicle-bookmark-menu-map [icicle-bookmark-local-file-other-window]
        '(menu-item "+ Local-File Bookmark..." icicle-bookmark-local-file-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a local-file bookmark"))
      (define-key icicle-bookmark-menu-map [icicle-bookmark-file-other-window]
        '(menu-item "+ File Bookmark..." icicle-bookmark-file-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a file bookmark"))
      (define-key icicle-bookmark-menu-map [icicle-bookmark-dired-other-window]
        '(menu-item "+ Dired Bookmark..." icicle-bookmark-dired-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a Dired bookmark"))
      (define-key icicle-bookmark-menu-map [icicle-bookmark-bookmark-file]
        '(menu-item "+ Bookmark-File Bookmark..." icicle-bookmark-bookmark-file
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a bookmark-file bookmark"))
      (define-key icicle-bookmark-menu-map [icicle-bookmark-bookmark-list]
        '(menu-item "+ Bookmark-List Bookmark..." icicle-bookmark-bookmark-list
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a bookmark-list bookmark"))
      (define-key icicle-bookmark-menu-map [icicle-bookmark-desktop]
        '(menu-item "+ Desktop Bookmark..." icicle-bookmark-desktop
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to an Emacs desktop bookmark")))

    ;; Icicles bookmark commands besides `Bookmark+'.
    (define-key icicle-bookmark-menu-map [icicle-bookmark]
      '(menu-item "+ Bookmark (Same Window)..." icicle-bookmark
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Jump to a bookmark (C-u: reverse `icicle-bookmark-refresh-cache-flag')"))
    (define-key icicle-bookmark-menu-map [icicle-bookmark-other-window]
      '(menu-item "+ Bookmark..." icicle-bookmark-other-window
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Jump to a bookmark (C-u: reverse `icicle-bookmark-refresh-cache-flag')"))

    (when (featurep 'bookmark+)

      ;; `With Tags' -------------------------------------------------
      ;; submenu for Icicles bookmark jump commands that involve `Bookmark+' tags.
      (defvar icicle-bookmark-with-tags-menu-map (make-sparse-keymap)
        "Menu of Icicles bookmark commands involving bookmark tags.")
      (define-key icicle-bookmark-menu-map [with-tags]
        (list 'menu-item "With Tags" icicle-bookmark-with-tags-menu-map))

      (define-key icicle-bookmark-with-tags-menu-map
          [icicle-bookmark-file-this-dir-all-tags-regexp-other-window]
        '(menu-item "+ File This Dir, All Tags Matching Regexp..."
          icicle-bookmark-file-this-dir-all-tags-regexp-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a file bookmark for this dir, where each tag matches a regexp"))
      (define-key icicle-bookmark-with-tags-menu-map
          [icicle-bookmark-file-this-dir-some-tags-regexp-other-window]
        '(menu-item "+ File This Dir, Any Tag Matching Regexp..."
          icicle-bookmark-file-this-dir-some-tags-regexp-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a file bookmark for this dir, where some tag matches a regexp"))
      (define-key icicle-bookmark-with-tags-menu-map [icicle-bookmark-file-this-dir-all-tags-other-window]
        '(menu-item "+ File This Dir, All Tags in Set..."
          icicle-bookmark-file-this-dir-all-tags-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a file bookmark for this dir, which has all of a set of tags"))
      (define-key icicle-bookmark-with-tags-menu-map [icicle-bookmark-file-this-dir-some-tags-other-window]
        '(menu-item "+ File This Dir, Any Tag in Set..."
          icicle-bookmark-file-this-dir-some-tags-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a file bookmark for this dir, which has some of a set of tags"))
      (define-key icicle-bookmark-with-tags-menu-map [icicle-bookmark-file-all-tags-regexp-other-window]
        '(menu-item "+ File, All Tags Matching Regexp..."
          icicle-bookmark-file-all-tags-regexp-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a file or dir bookmark where each tag matches a regexp"))
      (define-key icicle-bookmark-with-tags-menu-map [icicle-bookmark-file-some-tags-regexp-other-window]
        '(menu-item "+ File, Any Tag Matching Regexp..."
          icicle-bookmark-file-some-tags-regexp-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a file or dir bookmark where at least one tag matches a regexp"))
      (define-key icicle-bookmark-with-tags-menu-map [icicle-bookmark-file-all-tags-other-window]
        '(menu-item "+ File, All Tags in Set..." icicle-bookmark-file-all-tags-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a file or dir bookmark that has all of a set of tags"))
      (define-key icicle-bookmark-with-tags-menu-map [icicle-bookmark-file-some-tags-other-window]
        '(menu-item "+ File, Any Tag in Set..." icicle-bookmark-file-some-tags-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a file or dir bookmark that has some of a set of tags"))
      (define-key icicle-bookmark-with-tags-menu-map [icicle-find-file-tagged-other-window]
        '(menu-item "+ File with Tags..." icicle-find-file-tagged-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a file bookmark, matching its name or tags or both"))
      (define-key icicle-bookmark-with-tags-menu-map [icicle-bookmark-autofile-all-tags-regexp-other-window]
        '(menu-item "+ Autofile, All Tags Matching Regexp..."
          icicle-bookmark-autofile-all-tags-regexp-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to an autofile bookmark where each tag matches a regexp"))
      (define-key icicle-bookmark-with-tags-menu-map [icicle-bookmark-autofile-some-tags-regexp-other-window]
        '(menu-item "+ Autofile, Any Tag Matching Regexp..."
          icicle-bookmark-autofile-some-tags-regexp-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to an autofile bookmark where at least one tag matches a regexp"))
      (define-key icicle-bookmark-with-tags-menu-map [icicle-bookmark-autofile-all-tags-other-window]
        '(menu-item "+ Autofile, All Tags in Set..."
          icicle-bookmark-autofile-all-tags-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to an autofile bookmark that has all of a set of tags"))
      (define-key icicle-bookmark-with-tags-menu-map [icicle-bookmark-autofile-some-tags-other-window]
        '(menu-item "+ Autofile, Any Tag in Set..."
          icicle-bookmark-autofile-some-tags-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to an autofile bookmark that has some of a set of tags"))
      (define-key icicle-bookmark-with-tags-menu-map [icicle-bookmark-all-tags-regexp-other-window]
        '(menu-item "+ All Tags Matching Regexp..." icicle-bookmark-all-tags-regexp-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a bookmark that has each tag matching a regexp that you enter"))
      (define-key icicle-bookmark-with-tags-menu-map [icicle-bookmark-some-tags-regexp-other-window]
        '(menu-item "+ Any Tag Matching Regexp..." icicle-bookmark-some-tags-regexp-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a bookmark with at least one tag matching a regexp"))
      (define-key icicle-bookmark-with-tags-menu-map [icicle-bookmark-all-tags-other-window]
        '(menu-item "+ All Tags in Set..." icicle-bookmark-all-tags-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a bookmark that has all of a set of tags that you enter"))
      (define-key icicle-bookmark-with-tags-menu-map [icicle-bookmark-some-tags-other-window]
        '(menu-item "+ Any Tag in Set..." icicle-bookmark-some-tags-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a bookmark that has some of a set of tags that you enter"))
      (define-key icicle-bookmark-with-tags-menu-map [icicle-bookmark-tagged-other-window]
        '(menu-item "+ Any Bookmark with Tags..." icicle-bookmark-tagged-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a bookmark, matching its name or tags or both")))


    ;; `Edit' --------------------------------------------------------
    (cond ((not icicle-touche-pas-aux-menus-flag)
           (defvar icicle-edit-menu-map (make-sparse-keymap)
             "`Edit' > `Icicles' submenu.")
           (define-key menu-bar-edit-menu [icicles]
             (list 'menu-item "Icicles" icicle-edit-menu-map :visible 'icicle-mode)))
          (t
           (defvar icicle-edit-menu-map (make-sparse-keymap)
             "`Icicles' > `Edit' submenu.")
           (define-key icicle-menu-map [Edit]
             (list 'menu-item "Edit" icicle-edit-menu-map))))

    (define-key icicle-edit-menu-map [icicle-complete-thesaurus-entry]
      '(menu-item "Complete with Thesaurus..." icicle-complete-thesaurus-entry
        :enable (and (not buffer-read-only)  (boundp 'synonyms-obarray))
        :help "Complete a word to an entry from a thesaurus"))
    (define-key icicle-edit-menu-map [icicle-insert-thesaurus-entry]
      '(menu-item "+ Insert Thesaurus Entry..." icicle-insert-thesaurus-entry
        :enable (and (not buffer-read-only)  (boundp 'synonyms-obarray))
        :help "Insert an entry from a thesaurus"))
    (define-key icicle-edit-menu-map [icicle-completing-yank]
      '(menu-item "+ Paste Copied Text..." icicle-completing-yank
        :enable (not buffer-read-only) :keys "C-- C-y"
        :help "Yank an entry from the `kill-ring', choosing it using completion"))


    ;; `File' --------------------------------------------------------
    (cond ((not icicle-touche-pas-aux-menus-flag)
           (defvar icicle-file-menu-map (make-sparse-keymap)
             "`File' > `Icicles' submenu.")
           (define-key menu-bar-file-menu [icicles]
             (list 'menu-item "Icicles" icicle-file-menu-map :visible 'icicle-mode)))
          (t
           (defvar icicle-file-menu-map (make-sparse-keymap)
             "`Icicles' > `File' submenu.")
           (define-key icicle-menu-map [file]
             (list 'menu-item "File" icicle-file-menu-map :visible 'icicle-mode))))

    (define-key icicle-file-menu-map [icicle-delete-file]
      '(menu-item "+ Delete File..." icicle-delete-file :visible icicle-mode
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Delete a file or directory"))
    (when (featurep 'recentf)
      (define-key icicle-file-menu-map [icicle-remove-file-from-recentf-list]
        '(menu-item "+ Remove from Recent Files List..."
          icicle-remove-file-from-recentf-list :visible icicle-mode
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Remove file from `recentf-list' - the list of recently used files"))
      (define-key icicle-file-menu-map [icicle-recent-file-other-window]
        '(menu-item "+ Open Recent File (Other Window)..."
          icicle-recent-file-other-window :visible icicle-mode
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Open a recently used file in another window"))
      (define-key icicle-file-menu-map [icicle-recent-file]
        '(menu-item "+ Open Recent File..." icicle-recent-file :visible icicle-mode
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Open a recently used file")))
    (define-key icicle-file-menu-map [icicle-dired-saved-file-candidates-other-window]
      '(menu-item "Open Dired for Chosen Files..."
        icicle-dired-saved-file-candidates-other-window :visible icicle-mode
        :enable (and icicle-saved-completion-candidates
                 (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))
        :help "Open Dired on a set of files & directories of your choice"))
    (define-key icicle-file-menu-map [icicle-dired-project-other-window]
      '(menu-item "Open Dired for Project..." icicle-dired-project-other-window
        :visible icicle-mode
        :enable (and icicle-saved-completion-sets
                 (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))
        :help "Open Dired on a saved project (in another window)"))
    (define-key icicle-file-menu-map [icicle-locate-file-other-window]
      '(menu-item "+ Open File Under Directory (Other Window)..."
        icicle-locate-file-other-window :visible icicle-mode
        :help "Visit a file within a directory or its subdirectories, in another window"))
    (define-key icicle-file-menu-map [icicle-locate-file]
      '(menu-item "+ Open File Under Directory..." icicle-locate-file :visible icicle-mode
        :help "Visit a file within a directory or its subdirectories"))
    (define-key icicle-file-menu-map [icicle-file-other-window]
      '(menu-item "+ Open File or Directory (Other Window)..." icicle-file-other-window
        :visible icicle-mode
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Visit a file or directory in another window"))
    (define-key icicle-file-menu-map [icicle-file]
      '(menu-item "+ Open File or Directory..." icicle-file :visible icicle-mode
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Visit a file or directory (C-u absolute, C-- absolute by date)"))


    ;; `Buffers' -----------------------------------------------------
    (cond ((not icicle-touche-pas-aux-menus-flag)
           (cond ((boundp 'menu-bar-buffers-menu-command-entries) ; Emacs 22+.
                  (defvar icicle-buffers-menu-map (make-sparse-keymap)
                    "`Buffers' > `Icicles' submenu.")
                  (setq menu-bar-buffers-menu-command-entries
                        (cons (list 'Icicles 'menu-item "Icicles" icicle-buffers-menu-map)
                              menu-bar-buffers-menu-command-entries)))
                 (t
                  (defvar icicle-buffers-menu-map (make-sparse-keymap)
                    "`File' > `Icicles' > `Buffers' submenu.")
                  (define-key icicle-file-menu-map [buffers]
                    (list 'menu-item "Buffers" icicle-buffers-menu-map)))))
          (t
           (defvar icicle-buffers-menu-map (make-sparse-keymap)
             "`Icicles' > `Buffers' submenu.")
           (define-key icicle-menu-map [buffers]
             (list 'menu-item "Buffers" icicle-buffers-menu-map))))

    (define-key icicle-buffers-menu-map [icicle-buffer-list]
      '(menu-item "+ Buffer List..." icicle-buffer-list
        :help "Choose a list of buffer names"))
    (define-key icicle-buffers-menu-map [icicle-remove-buffer-config]
      '(menu-item "+ Remove Buffer Configuration..." icicle-remove-buffer-config
        :enable icicle-buffer-configs
        :help "Remove buffer configuration from `icicle-buffer-configs'"))
    (define-key icicle-buffers-menu-map [icicle-add-buffer-config]
      '(menu-item "New Buffer Configuration..." icicle-add-buffer-config
        :help "Add buffer configuration to `icicle-buffer-configs'"))
    (define-key icicle-buffers-menu-map [icicle-buffer-config]
      '(menu-item "+ Choose Buffer Configuration..." icicle-buffer-config
        :enable icicle-buffer-configs
        :help "Choose a configuration of user options for `icicle-buffer'"))
    (define-key icicle-buffers-menu-map [icicle-remove-buffer-candidate]
      '(menu-item "+ Don't Always Include Buffer..." icicle-remove-buffer-candidate
        :enable icicle-buffer-extras
        :help "Remove buffer as an always-show completion candidate"))
    (define-key icicle-buffers-menu-map [icicle-add-buffer-candidate]
      '(menu-item "+ Always Include Buffer..." icicle-add-buffer-candidate
        :help "Add buffer as an always-show completion candidate"))
    (define-key icicle-buffers-menu-map [icicle-kill-buffer]
      '(menu-item "+ Kill Buffer..." icicle-kill-buffer
        :help "Kill a buffer (C-0: same-mode, C-9: file, C-- this-frame"))
    (define-key icicle-buffers-menu-map [icicle-insert-buffer]
      '(menu-item "+ Insert Buffer..." icicle-insert-buffer
        :help "Multi-command version of `insert-buffer'"))
    (define-key icicle-buffers-menu-map [icicle-delete-windows]
      '(menu-item "+ Delete Windows on Buffer..." icicle-delete-windows :keys "C-u C-x 0"
        :help "Delete windows showing a buffer, anywhere"))
    (define-key icicle-buffers-menu-map [icicle-buffer-other-window]
      '(menu-item "+ Switch to Buffer (Other Window)..." icicle-buffer-other-window
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (define-key icicle-buffers-menu-map [icicle-buffer]
      '(menu-item "+ Switch to Buffer..." icicle-buffer
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Switch to a buffer (C-0: same mode, C-9: file buffer, C--: same frame"))


    ;; `Dired Marked' ------------------------------------------------
    (cond ((not icicle-touche-pas-aux-menus-flag)
           (defvar icicle-dired-multiple-menu-map (make-sparse-keymap)
             "`Icicles' submenu for Dired's `Multiple' (or `Operate') menu.")
           (if (or (boundp 'diredp-menu-bar-multiple-menu) ; In `dired+.el'.
                   (boundp 'diredp-menu-bar-operate-menu)) ; In `dired+.el' (old name).
               (let ((menu  (if (boundp 'diredp-menu-bar-multiple-menu)
                                diredp-menu-bar-multiple-menu
                              diredp-menu-bar-operate-menu)))
                 (define-key menu [icicles]
                   (list 'menu-item "Icicles" icicle-dired-multiple-menu-map :visible 'icicle-mode)))
             (define-key dired-mode-map [menu-bar operate icicles]
               (list 'menu-item "Icicles" icicle-dired-multiple-menu-map :visible 'icicle-mode)))

           (when (boundp 'diredp-menu-bar-recursive-marked-menu) ; Defined in `dired+.el'
             (defvar icicle-dired-recursive-marked-menu-map (make-sparse-keymap)
               "`Icicles' submenu for Dired+'s `Multiple' > `Marked Here and Below' menu.")
             (define-key diredp-menu-bar-recursive-marked-menu [icicles]
               (list 'menu-item "Icicles" icicle-dired-recursive-marked-menu-map
                     :visible 'icicle-mode))))

          (t
           (defvar icicle-dired-multiple-menu-map (make-sparse-keymap)
             "`Icicles' > `Dired Marked' submenu, in Dired mode.")
           (define-key icicle-menu-map [dired-marked]
             (list 'menu-item "Dired Marked" icicle-dired-multiple-menu-map
                   :visible '(eq major-mode 'dired-mode)))

           (when (boundp 'diredp-menu-bar-recursive-marked-menu) ; Defined in `dired+.el'
             (defvar icicle-dired-recursive-marked-menu-map (make-sparse-keymap)
               "`Icicles' > `Dired Marked Here and Below' menu.")
             (define-key icicle-dired-multiple-menu-map [here-and-below]
               (list 'menu-item "Here and Below" icicle-dired-recursive-marked-menu-map
                     :visible '(eq major-mode 'dired-mode))))))

    (define-key icicle-dired-multiple-menu-map [icicle-dired-save-marked-as-project]
      '(menu-item "Save Names Persistently (Project)" icicle-dired-save-marked-as-project
        :help "Save the marked names as a persistent set"))
    (define-key icicle-dired-multiple-menu-map [icicle-dired-save-marked-to-variable]
      '(menu-item "Save Names to Variable" icicle-dired-save-marked-to-variable
        :help "Save the marked names to a variable"))
    (define-key icicle-dired-multiple-menu-map [icicle-dired-save-marked-more]
      '(menu-item "Add to Saved Candidates" icicle-dired-save-marked-more
        :help "Add the marked names to the saved completion-candidates set"))
    (define-key icicle-dired-multiple-menu-map [icicle-dired-save-marked]
      '(menu-item "Save Names as Completion Candidates" icicle-dired-save-marked
        :help "Save the marked names as a set of completion candidates"))
    (define-key icicle-dired-multiple-menu-map [separator-dired-multiple-1] '(menu-item "--" nil))
    (define-key icicle-dired-multiple-menu-map [icicle-search-dired-marked]
      '(menu-item "Icicles Search (and Replace)..." icicle-search-dired-marked
        :help "Search the marked files"
        :enable (condition-case nil     ; Avoid an Emacs 22 error with cursor on ./ or ../
                    (dired-get-marked-files nil nil (lambda (file) (not (file-directory-p file))))
                  (error nil))))
    (define-key icicle-dired-multiple-menu-map [icicle-occur-dired-marked]
      '(menu-item "Icicles Occur..." icicle-occur-dired-marked
        :help "Search lines of the marked files"
        :enable (condition-case nil     ; Avoid an Emacs 22 error with cursor on ./ or ../
                    (dired-get-marked-files nil nil (lambda (file) (not (file-directory-p file))))
                  (error nil))))
    (define-key icicle-dired-multiple-menu-map [icicle-visit-marked-file-of-content-other-window]
      '(menu-item "Open File of Content (Other Window)"
        icicle-visit-marked-file-of-content-other-window
        :help "Visit a marked file whose content matches a regexp, in another window"
        :enable (condition-case nil     ; Avoid an Emacs 22 error with cursor on ./ or ../
                    (dired-get-marked-files nil nil (lambda (file) (not (file-directory-p file))))
                  (error nil))))
    (define-key icicle-dired-multiple-menu-map [icicle-visit-marked-file-of-content]
      '(menu-item "Open File of Content" icicle-visit-marked-file-of-content
        :help "Visit a marked file whose content matches a regexp"
        :enable (condition-case nil     ; Avoid an Emacs 22 error with cursor on ./ or ../
                    (dired-get-marked-files nil nil (lambda (file) (not (file-directory-p file))))
                  (error nil))))

    (when (boundp 'diredp-menu-bar-recursive-marked-menu) ; Defined in `dired+.el'
      (define-key icicle-dired-recursive-marked-menu-map
          [icicle-dired-save-marked-to-fileset-recursive]
        '(menu-item "Save Names to Fileset" icicle-dired-save-marked-to-fileset-recursive
          :help "Save marked names, including those in marked subdirs, to an Emacs fileset"))
      (define-key icicle-dired-recursive-marked-menu-map
          [icicle-dired-save-marked-to-cache-file-recursive]
        '(menu-item "Save Names to Cache File" icicle-dired-save-marked-to-cache-file-recursive
          :help "Save marked names, including those in marked subdirs, to an Icicles cache file"))
      (define-key icicle-dired-recursive-marked-menu-map
          [icicle-dired-save-marked-to-variable-recursive]
        '(menu-item "Save Names to Variable" icicle-dired-save-marked-to-variable-recursive
          :help "Save marked names, including those in marked subdirs, to a variable"))
      (define-key icicle-dired-recursive-marked-menu-map [icicle-dired-save-marked-more-recursive]
        '(menu-item "Add to Saved Candidates" icicle-dired-save-marked-more-recursive
          :help "Add marked files, including those in marked subdirs, to saved candidates"))
      (define-key icicle-dired-recursive-marked-menu-map [icicle-dired-save-marked-recursive]
        '(menu-item "Save Names as Completion Candidates" icicle-dired-save-marked-recursive
          :help "Save the marked file names in Dired, including those in marked subdirs"))
      (define-key icicle-dired-recursive-marked-menu-map [separator-dired-recursive-1]
        '(menu-item "--" nil))
      (define-key icicle-dired-recursive-marked-menu-map [icicle-search-dired-marked-recursive]
        '(menu-item "Icicles Search (and Replace)..." icicle-search-dired-marked-recursive
          :help "Search the marked files, including those in marked subdirs"
          :enable (condition-case nil   ; Avoid an Emacs 22 error with cursor on ./ or ../
                      (let ((files   (dired-get-marked-files nil nil nil 'DISTINGUISH-ONE-MARKED)))
                        (and files  (cdr files))) ; Must have at least one actual mark.
                    (error nil))))
      (define-key icicle-dired-recursive-marked-menu-map [icicle-occur-dired-marked-recursive]
        '(menu-item "Icicles Occur..." icicle-occur-dired-marked-recursive
          :help "Search lines of the marked files, including those in marked subdirs"
          :enable (condition-case nil   ; Avoid an Emacs 22 error with cursor on ./ or ../
                      (let ((files   (dired-get-marked-files nil nil nil 'DISTINGUISH-ONE-MARKED)))
                        (and files  (cdr files))) ; Must have at least one actual mark.
                    (error nil))))
      (define-key icicle-dired-recursive-marked-menu-map
          [icicle-visit-marked-file-of-content-recursive-other-window]
        '(menu-item "Open File of Content (Other Window)"
          icicle-visit-marked-file-of-content-recursive-other-window
          :help "Visit marked file, including in marked subdir, whose content matches, in other window"
          ;; Do not test with `diredp-get-files' - no need to, and too slow
          :enable (condition-case nil   ; Avoid an Emacs 22 error with cursor on ./ or ../
                      (let ((files   (dired-get-marked-files nil nil nil 'DISTINGUISH-ONE-MARKED)))
                        (and files  (cdr files))) ; Must have at least one actual mark.
                    (error nil))))
      (define-key icicle-dired-recursive-marked-menu-map
          [icicle-visit-marked-file-of-content-recursive]
        '(menu-item "Open File of Content" icicle-visit-marked-file-of-content-recursive
          :help "Visit marked file, including in a marked subdir, whose content matches"
          ;; Do not test with `diredp-get-files' - no need to, and too slow
          :enable (condition-case nil   ; Avoid an Emacs 22 error with cursor on ./ or ../
                      (let ((files   (dired-get-marked-files nil nil nil 'DISTINGUISH-ONE-MARKED)))
                        (and files  (cdr files))) ; Must have at least one actual mark.
                    (error nil)))))

    ;; `Dired Dirs' ------------------------------------------------
    (cond ((not icicle-touche-pas-aux-menus-flag)
           (defvar icicle-dired-dir-menu-map (make-sparse-keymap)
             "`Icicles' submenu for Dired's `Dir' (or `Subdir') menu.")
           (if (or (boundp 'diredp-menu-bar-dir-menu) ; In `dired+.el'.
                   (boundp 'diredp-menu-bar-subdir-menu)) ; In `dired+.el' (old name).
               (let ((menu  (if (boundp 'diredp-menu-bar-dir-menu)
                                diredp-menu-bar-dir-menu
                              diredp-menu-bar-subdir-menu)))
                 (define-key menu [icicles]
                   (list 'menu-item "Icicles" icicle-dired-dir-menu-map :visible 'icicle-mode)))
             (define-key dired-mode-map [menu-bar subdir icicles]
               (list 'menu-item "Icicles" icicle-dired-dir-menu-map :visible 'icicle-mode))))
          (t
           (defvar icicle-dired-dir-menu-map (make-sparse-keymap)
             "`Icicles' > `Dired Dirs' submenu, in Dired mode.")
           (define-key icicle-menu-map [dired-dirs]
             (list 'menu-item "Dired Dirs" icicle-dired-dir-menu-map
                   :visible '(eq major-mode 'dired-mode)))))

    (when (fboundp 'icicle-dired-insert-as-subdir) ; Emacs 21+
      (define-key icicle-dired-dir-menu-map [icicle-dired-insert-as-subdir]
        '(menu-item "Insert Dir into Ancestor Dired..." icicle-dired-insert-as-subdir
          :help "Insert a directory into a Dired ancestor directory listing")))
    (define-key icicle-dired-dir-menu-map [icicle-dired-saved-file-candidates-other-window]
      '(menu-item "Open Dired for Chosen Files..."
        icicle-dired-saved-file-candidates-other-window
        :enable icicle-saved-completion-candidates
        :help "Open Dired on a set of files & directories of your choice"))
    (define-key icicle-dired-dir-menu-map [icicle-dired-project-other-window]
      '(menu-item "Open Dired for Project..." icicle-dired-project-other-window
        :enable icicle-saved-completion-sets
        :help "Open Dired on a saved project"))


    ;; `Info' --------------------------------------------------------
    (require 'info)                     ; `Info-mode-menu' is not preloaded.
    (cond ((not icicle-touche-pas-aux-menus-flag)
           (defvar icicle-info-menu-map (make-sparse-keymap)
             "`Info' > `Icicles' submenu.")
           (define-key Info-mode-menu [icicles]
             (list 'menu-item "Icicles" icicle-info-menu-map :visible 'icicle-mode)))
          (t
           (defvar icicle-info-menu-map (make-sparse-keymap)
             "`Icicles' > `Info' submenu, in Info mode.")
           (define-key icicle-menu-map [info]
             (list 'menu-item "Info Mode" icicle-info-menu-map :visible '(eq major-mode 'Info-mode)))))

    (when (fboundp 'icicle-Info-virtual-book)
      (define-key icicle-info-menu-map [icicle-Info-virtual-book]
        '(menu-item "Virtual Book" icicle-Info-virtual-book
          :help "Open Info on a virtual book of saved Info nodes")))
    (define-key icicle-info-menu-map [icicle-Info-goto-node]
      '(menu-item "+ Go to Node..." icicle-Info-goto-node
        :help "Go to an Info node by name"))
    (define-key icicle-info-menu-map [icicle-Info-menu]
      '(menu-item "+ Go to Menu Node..." icicle-Info-menu
        :help "Choose a menu node by name"))
    (define-key icicle-info-menu-map [icicle-Info-index]
      '(menu-item "+ Look Up in Index..." icicle-Info-index
        :help "Go to an indexed topic - multi-command version of `Info-index'"))


    ;; `Bookmark+', in buffer `*Bookmark List*' ----------------------
    ;;
    ;; Do this regardless of `icicle-touche-pas-aux-menus-flag', since `Bookmark+' is my menu.
    (when (boundp 'bmkp-bmenu-menubar-menu) ; In `bookmark+-bmu.el'.
      (defvar icicle-bookmark+-menu-map (make-sparse-keymap)
        "`Bookmark+' > `Icicles' submenu.")
      (define-key bmkp-bmenu-menubar-menu [icicles]
        (list 'menu-item "Icicles" icicle-bookmark+-menu-map :visible 'icicle-mode))
      (define-key icicle-bookmark+-menu-map [icicle-search-bookmark-list-marked]
        '(menu-item "Search & Replace in Marked Files..." icicle-search-bookmark-list-marked
          :help "Search the files of the marked bookmarks"))
      (define-key icicle-bookmark+-menu-map [icicle-bookmark-save-marked-files-more]
        '(menu-item "Save File Names of Marked as More Candidates..."
          icicle-bookmark-save-marked-files-more
          :help "Add file names of marked bookmarks to saved file-completion candidates"))
      (define-key icicle-bookmark+-menu-map [icicle-bookmark-save-marked-files]
        '(menu-item "Save File Names of Marked as Candidates..." icicle-bookmark-save-marked-files
          :help "Save file names of marked bookmarks as a set of file-completion candidates"))
      (define-key icicle-bookmark+-menu-map [icicle-bookmark-save-marked-files-as-project]
        '(menu-item "Save Marked as Project" icicle-bookmark-save-marked-files-as-project
          :help "Save the file names of the marked bookmarks as a persistent set")))

    )


  ;; Install `Icicles' menu-bar menu.
  (define-key icicle-mode-map [menu-bar icicles] (cons "Icicles" icicle-menu-map))

  ;; Optional `icicle-mode-map' bindings - governed by `icicle-top-level-key-bindings'.
  (icicle-bind-top-level-commands)

  ;; Put all Icicles search commands on a common prefix key, `icicle-search-key-prefix'.
  (define-key icicle-mode-map icicle-search-key-prefix icicle-search-map)

  ;; Install or update `icicle-mode-map'.
  (if icicle-minor-mode-map-entry
      (setcdr icicle-minor-mode-map-entry icicle-mode-map)
    (setq icicle-minor-mode-map-entry  (cons 'icicle-mode icicle-mode-map))
    (add-to-list 'minor-mode-map-alist icicle-minor-mode-map-entry)))

(defun icicle-S-iso-lefttab-to-S-TAB (strg)
  "Return string STRG, but with \"S-iso-lefttab\" replaced by \"S-TAB\"."
  (replace-regexp-in-string "S-iso-lefttab" "S-TAB" strg))

(defun icicle-bind-other-keymap-keys ()
  "Bind some keys in maps other than minibuffer maps and `icicle-mode-map'"

  ;; Bind Isearch keys.
  (icicle-bind-isearch-keys)

  ;; Bind keys in Comint mode.
  (when (boundp 'comint-mode-map)
    (define-key comint-mode-map (icicle-kbd "C-c C-i") 'icicle-comint-command)  ; `C-c TAB'
    (define-key comint-mode-map (icicle-kbd "C-c tab") 'icicle-comint-command)) ; `C-c TAB'

  (when (boundp 'facemenu-keymap)
    (define-key 'facemenu-keymap "n" 'icicle-next-font-lock-keywords-repeat) ; `M-o n'
    (define-key 'facemenu-keymap "I" 'icicle-font-lock-keyword))             ; `M-o I'

  ;; Bind keys in Shell mode.
  (when (and (boundp 'shell-mode-map)  (memq 'comint-dynamic-complete icicle-functions-to-redefine))
    (define-key shell-mode-map (icicle-kbd "C-i") 'icicle-comint-dynamic-complete))

  ;; Bind keys in Shell Script mode.
  (when (and (boundp 'sh-mode-map)  (memq 'comint-dynamic-complete icicle-functions-to-redefine))
    (icicle-remap 'comint-dynamic-complete 'icicle-comint-dynamic-complete sh-mode-map))

  ;; Bind keys in Ielm mode.
  (when (and (boundp 'ielm-map)  (memq 'comint-dynamic-complete icicle-functions-to-redefine))
    (define-key ielm-map (icicle-kbd "C-i") 'icicle-comint-dynamic-complete))

  ;; Bind keys in Tcl mode.
  (when (and (boundp 'inferior-tcl-mode-map)  (memq 'comint-dynamic-complete icicle-functions-to-redefine))
    (define-key inferior-tcl-mode-map (icicle-kbd "C-i") 'icicle-comint-dynamic-complete))

  ;; Bind keys in GUD (Debugger) mode.
  (when (and (boundp 'gud-minibuffer-local-map)  (memq 'comint-dynamic-complete-filename
                                                       icicle-functions-to-redefine))
    (define-key gud-minibuffer-local-map (icicle-kbd "C-i")
      'icicle-comint-dynamic-complete-filename))

  ;; Bind some keys in `bookmark-bmenu-mode' mode (*Bookmark List*) - requires Bookmark+.
  (when (and (featurep 'bookmark+)  (boundp 'bookmark-bmenu-mode-map))
    (unless (lookup-key bookmark-bmenu-mode-map (icicle-kbd "C-M->")) ; *Bookmark List* `C-M->'
      (define-key bookmark-bmenu-mode-map (icicle-kbd "C-M->") 'icicle-bookmark-save-marked-files))
    (unless (lookup-key bookmark-bmenu-mode-map (icicle-kbd "C->")) ; *Bookmark List* `C->'
      (define-key bookmark-bmenu-mode-map (icicle-kbd "C->")
        'icicle-bookmark-save-marked-files-more))
    (unless (lookup-key bookmark-bmenu-mode-map (icicle-kbd "C-M-}")) ; *Bookmark List* `C-M-}'
      (define-key bookmark-bmenu-mode-map (icicle-kbd "C-M-}")
        'icicle-bookmark-save-marked-files-to-variable))
    (unless (lookup-key bookmark-bmenu-mode-map (icicle-kbd "C-}")) ; *Bookmark List* `C-}'
      (define-key bookmark-bmenu-mode-map (icicle-kbd "C-}")
        'icicle-bookmark-save-marked-files-as-project))
    (let* ((key  (apply 'vector (append (listify-key-sequence icicle-search-key-prefix)
                                        (listify-key-sequence (icicle-kbd "m"))))) ; `M-s M-s m'
           (def  (lookup-key bookmark-bmenu-mode-map key)))
      (unless (and def  (not (integerp def)))
        (define-key bookmark-bmenu-mode-map key 'icicle-search-bookmark-list-marked))))

  ;; Bind some keys in Dired mode.
  (when (boundp 'dired-mode-map)
    (unless (lookup-key dired-mode-map (icicle-kbd "C-M-<")) ; Dired `C-M-<'
      (define-key dired-mode-map (icicle-kbd "C-M-<")
        'icicle-dired-saved-file-candidates-other-window))
    (unless (lookup-key dired-mode-map (icicle-kbd "C-{")) ; Dired `C-{'
      (define-key dired-mode-map (icicle-kbd "C-{") 'icicle-dired-project-other-window))
    (unless (lookup-key dired-mode-map (icicle-kbd "C-M->")) ; Dired `C-M->'
      (define-key dired-mode-map (icicle-kbd "C-M->") 'icicle-dired-save-marked))
    (unless (lookup-key dired-mode-map (icicle-kbd "C->")) ; Dired `C->'
      (define-key dired-mode-map (icicle-kbd "C->") 'icicle-dired-save-marked-more))
    (unless (lookup-key dired-mode-map (icicle-kbd "C-M-}")) ; Dired `C-M-}'
      (define-key dired-mode-map (icicle-kbd "C-M-}") 'icicle-dired-save-marked-to-variable))
    (unless (lookup-key dired-mode-map (icicle-kbd "C-}")) ; Dired `C-}'
      (define-key dired-mode-map (icicle-kbd "C-}") 'icicle-dired-save-marked-as-project))
    (unless (lookup-key dired-mode-map (icicle-kbd "C-S-f")) ; Dired `C-S-f', aka `C-F'
      (define-key dired-mode-map (icicle-kbd "C-S-f") 'icicle-visit-marked-file-of-content))
    (unless (lookup-key dired-mode-map (icicle-kbd "C-M-S-f")) ; Dired `C-M-S-f', aka `C-M-F'
      (define-key dired-mode-map (icicle-kbd "C-M-S-f")
        'icicle-visit-marked-file-of-content-other-window))
    (unless (lookup-key dired-mode-map (icicle-kbd "C-S-o")) ; Dired `C-S-o', aka `C-O'
      (define-key dired-mode-map (icicle-kbd "C-S-o") 'icicle-occur-dired-marked))
    (unless (lookup-key dired-mode-map (icicle-kbd "C-S-s")) ; Dired `C-S-s', aka `C-S'
      (define-key dired-mode-map (icicle-kbd "C-S-s") 'icicle-search-dired-marked))


    )

  ;; More Dired keys, but these require `dired+.el'.
  (when (boundp 'diredp-recursive-map)

    ;; `dired-mode-map'.
    (let* ((key  (apply 'vector
                        (append (listify-key-sequence icicle-search-key-prefix)
                                (listify-key-sequence (icicle-kbd "m")))))
           (def  (lookup-key dired-mode-map key)))
      (unless (and def  (not (integerp def)))
        (define-key dired-mode-map key 'icicle-search-dired-marked-recursive))) ; `M-s M-s m'
    (let* ((key  (apply 'vector
                        (append (listify-key-sequence icicle-search-key-prefix)
                                (listify-key-sequence (icicle-kbd "M")))))
           (def  (lookup-key dired-mode-map key)))
      (unless (and def  (not (integerp def)))
        (define-key dired-mode-map key 'icicle-occur-dired-marked-recursive))) ; `M-s M-s M'

    ;; `diredp-recursive-map'.
    (define-key diredp-recursive-map (icicle-kbd "C-S-s") ; `M-+ C-S-s', aka `M-+ C-S'
      'icicle-search-dired-marked-recursive) ;     and `C-0 M-s M-s M-s' and `C-0 C-`'
    (define-key diredp-recursive-map (icicle-kbd "C-S-o") ; `M-+ C-S-o', aka `M-+ C-O'
      'icicle-occur-dired-marked-recursive)
    (define-key diredp-recursive-map (icicle-kbd "C-{") ; `M-+ C-{'
      'icicle-dired-project-other-window)
    (define-key diredp-recursive-map (icicle-kbd "C-M->") ; `M-+ C-M->'
      'icicle-dired-save-marked-recursive)
    (define-key diredp-recursive-map (icicle-kbd "C->") ; `M-+ C->'
      'icicle-dired-save-marked-more-recursive)
    (define-key diredp-recursive-map (icicle-kbd "C-M-}") ; `M-+ C-M-}'
      'icicle-dired-save-marked-to-variable-recursive)
    (define-key diredp-recursive-map (icicle-kbd "C-}") ; `M-+ C-}'
      'icicle-dired-save-marked-to-cache-file-recursive)
    (define-key diredp-recursive-map (icicle-kbd "C-S-f") ; `M-+ C-S-f', aka `M-+ C-F'
      'icicle-visit-marked-file-of-content-recursive)
    (define-key diredp-recursive-map (icicle-kbd "C-M-S-f") ; `M-+ C-M-S-f', aka `M-+ C-M-F'
      'icicle-visit-marked-file-of-content-recursive-other-window))

  ;; Bind keys in Ibuffer mode.
  (when (boundp 'ibuffer-mode-map)
    (let* ((key  (apply 'vector         ; Ibuffer `M-s M-s m'
                        (append (listify-key-sequence icicle-search-key-prefix)
                                (listify-key-sequence (icicle-kbd "m")))))
           (def  (lookup-key ibuffer-mode-map icicle-search-key-prefix)))
      (unless (and def  (not (integerp def)))
        (define-key ibuffer-mode-map key 'icicle-search-ibuffer-marked))
      (unless icicle-touche-pas-aux-menus-flag ; Use Ibuffer's `Operate' menu.
        (define-key ibuffer-mode-operate-map [icicle-search-ibuffer-marked]
          '(menu-item "Icicles Search (and Replace)..." icicle-search-ibuffer-marked
            :visible icicle-mode :enable (eq major-mode 'ibuffer-mode))))))

  ;; Bind keys in Buffer Menu mode.
  (when (boundp 'Buffer-menu-mode-map)
    (let* ((key  (apply 'vector         ; Buffer-Menu `M-s M-s m'
                        (append (listify-key-sequence icicle-search-key-prefix)
                                (listify-key-sequence (icicle-kbd "m")))))
           (def  (lookup-key Buffer-menu-mode-map icicle-search-key-prefix)))
      (unless (and def  (not (integerp def)))
        (define-key Buffer-menu-mode-map key 'icicle-search-buff-menu-marked))))

  ;; Bind `S-TAB' in major maps, for key completion.
  (when (fboundp 'map-keymap)           ; Emacs 22+.
    (icicle-bind-key-completion-keys-in-keymaps-from (current-global-map))
    (mapcar #'icicle-bind-key-completion-keys-for-map-var icicle-keymaps-for-key-completion))

  ;; Bind `M-S-TAB' in `minibuffer-local-map', for key completion.
  (when (fboundp 'map-keymap)           ; Emacs 22+.
    (icicle-bind-key-completion-keys-for-map-var 'minibuffer-local-map
                                                 icicle-key-complete-keys-for-minibuffer))

  ;; Prevent `this-command' from being set to `handle-switch-frame'.
  (define-key global-map [handle-switch-frame] 'icicle-skip-this-command)
  (define-key global-map [switch-frame] 'icicle-handle-switch-frame))

(defun icicle-bind-isearch-keys ()
  "Bind Icicles Isearch commands."
  (dolist (key  icicle-search-from-isearch-keys)
    (define-key isearch-mode-map key 'icicle-search-w-isearch-string)) ; In `icicles-cmd2.el'.
  (dolist (key  icicle-isearch-complete-keys)
    (define-key isearch-mode-map key 'icicle-isearch-complete))
  (dolist (key  icicle-isearch-history-insert-keys)
    (define-key isearch-mode-map key 'icicle-isearch-history-insert))
  (cond ((fboundp 'isearch-moccur)      ; In `moccur.el'.
         (define-key isearch-mode-map (icicle-kbd "C-o") 'isearch-moccur)) ; `C-s C-o'
        ((fboundp 'isearch-occur)       ; In `occur-schroeder.el'.
         (define-key isearch-mode-map (icicle-kbd "C-o") 'isearch-occur)))) ; `C-s C-o'

(defun icicle-bind-key-completion-keys-for-map-var (keymap-var &optional keys)
  "Bind `S-TAB' in keymaps accessible from keymap KEYMAP-VAR.
KEYMAP-VAR should be bound to a keymap that has at least one prefix
keymap.  If KEYMAP-VAR is not bound to a keymap, it is ignored.

Actually, by default, it is the keys in `icicle-key-complete-keys'
that are bound, not `S-TAB'.  And if optional arg KEYS is non-nil then
it, not `icicle-key-complete-keys', is the list of keys that are
bound."
  (let ((temp  keymap-var))
    (when (boundp temp)
      (setq temp  (symbol-value temp))
      (when (keymapp temp) (icicle-bind-key-completion-keys-in-keymaps-from temp keys)))))

(defun icicle-bind-key-completion-keys-in-keymaps-from (map &optional keys)
  "Bind keys in `icicle-key-complete-keys' to `icicle-complete-keys'.
Each key in `icicle-complete-keys' (or optional arg KEYS, if non-nil)
is bound in all keymaps accessible from keymap MAP."
  (dolist (key+map  (accessible-keymaps map))
    (let ((map  (cdr key+map)))
      ;; We could try to exclude menu maps, by testing (not (keymap-prompt map)).
      ;; But we want to include at least some menu maps - those, such as `facemenu-keymap',
      ;; that are bound to keyboard keys. (when (and (keymapp map)  (not (keymap-prompt map)))...)
      (when (keymapp map)
        (dolist (key  (or keys  icicle-key-complete-keys))
          (when (or icicle-complete-key-anyway-flag  (not (lookup-key map key)))
            (condition-case nil (define-key map key 'icicle-complete-keys) (error nil))))))))

(defun icicle-restore-other-keymap-keys ()
  "Restore some bindings changed by `icicle-bind-other-keymap-keys'."

  ;; Unbind Isearch keys.
  (icicle-unbind-isearch-keys)

  ;; Unbind keys in Comint mode.
  (when (boundp 'comint-mode-map)
    (define-key comint-mode-map (icicle-kbd "C-c C-i") nil)
    (define-key comint-mode-map (icicle-kbd "C-c tab") nil))

  ;; Unbind keys in Facemenu.
  (when (boundp 'facemenu-keymap)
    (define-key 'facemenu-keymap "n" nil)  ; `M-o n'
    (define-key 'facemenu-keymap "I" nil)) ; `M-o I'

  ;; Unbind keys in Shell mode.
  (when (and (boundp 'shell-mode-map)  (memq 'icicle-comint-dynamic-complete icicle-functions-to-redefine))
    (define-key shell-mode-map (icicle-kbd "C-i") (if (> emacs-major-version 23)
                                                      'completion-at-point
                                                    'comint-dynamic-complete)))

  ;; Unbind keys in Shell Script mode.
  (when (and (boundp 'sh-mode-map)  (memq 'icicle-comint-dynamic-complete icicle-functions-to-redefine))
    (icicle-unmap 'comint-dynamic-complete sh-mode-map 'icicle-comint-dynamic-complete))

  ;; Unbind keys in Ielm mode.
  (when (and (boundp 'ielm-map)  (memq 'icicle-comint-dynamic-complete icicle-functions-to-redefine))
    (define-key ielm-map (icicle-kbd "C-i") 'comint-dynamic-complete))

  ;; Unbind keys in Tcl mode.
  (when (and (boundp 'inferior-tcl-mode-map)  (memq 'icicle-comint-dynamic-complete
                                                    icicle-functions-to-redefine))
    (define-key inferior-tcl-mode-map (icicle-kbd "C-i") 'comint-dynamic-complete))

  ;; Bind keys in GUD (Debugger) mode.
  (when (and (boundp 'gud-minibuffer-local-map)  (memq 'icicle-comint-dynamic-complete-filename
                                                       icicle-functions-to-redefine))
    (define-key gud-minibuffer-local-map (icicle-kbd "C-i") 'comint-dynamic-complete-filename))

  ;; Unbind keys in `bookmark-bmenu-mode' mode (*Bookmark List*) - requires Bookmark+.
  (when (and (featurep 'bookmark+)  (boundp 'bookmark-bmenu-mode-map))
    (define-key bookmark-bmenu-mode-map (icicle-kbd "C-M->")   nil)
    (define-key bookmark-bmenu-mode-map (icicle-kbd "C->")     nil)
    (define-key bookmark-bmenu-mode-map (icicle-kbd "C-M-}")   nil)
    (define-key bookmark-bmenu-mode-map (icicle-kbd "C-}")     nil)
    (define-key bookmark-bmenu-mode-map icicle-search-key-prefix nil))

  ;; Unbind keys in Dired mode.
  (when (boundp 'dired-mode-map)
    (define-key dired-mode-map (icicle-kbd "C-M-<")    nil)
    (define-key dired-mode-map (icicle-kbd "C-{")      nil)
    (define-key dired-mode-map (icicle-kbd "C-M->")    nil)
    (define-key dired-mode-map (icicle-kbd "C->")      nil)
    (define-key dired-mode-map (icicle-kbd "C-M-}")    nil)
    (define-key dired-mode-map (icicle-kbd "C-}")      nil)
    (define-key dired-mode-map (icicle-kbd "C-S-f")    nil)
    (define-key dired-mode-map (icicle-kbd "C-S-o")    nil)
    (define-key dired-mode-map icicle-search-key-prefix  nil))

  ;; Unbind keys in Ibuffer mode.
  (when (boundp 'ibuffer-mode-map)
    (define-key ibuffer-mode-map icicle-search-key-prefix nil))

  ;; Unbind keys in Buffer Menu mode.
  (when (boundp 'Buffer-menu-mode-map)
    (define-key Buffer-menu-mode-map icicle-search-key-prefix nil))

  ;; Unbind `S-TAB' in major maps.
  (when (fboundp 'map-keymap)           ; Emacs 22+.
    (icicle-unbind-key-completion-keys-in-keymaps-from (current-global-map))
    (mapcar #'icicle-unbind-key-completion-keys-for-map-var icicle-keymaps-for-key-completion))

  ;; Unbind `M-S-TAB' in `minibuffer-local-map'.
  (when (fboundp 'map-keymap)           ; Emacs 22+.
    (icicle-unbind-key-completion-keys-for-map-var 'minibuffer-local-map
                                                   icicle-key-complete-keys-for-minibuffer))
  ;; Restore prevention of `this-command' being `handle-switch-frame'.
  (define-key global-map [handle-switch-frame] nil)
  (define-key global-map [switch-frame] 'handle-switch-frame))

(defun icicle-unbind-isearch-keys ()
  "Unbind Icicles Isearch commands."
  (dolist (key  icicle-search-from-isearch-keys) (define-key isearch-mode-map key nil))
  (dolist (key  icicle-isearch-complete-keys) (define-key isearch-mode-map key nil))
  (define-key isearch-mode-map (icicle-kbd "C-M-i") 'isearch-complete)
  (dolist (key  icicle-isearch-history-insert-keys) (define-key isearch-mode-map key nil))
  (when (fboundp 'isearch-moccur)       ; Restore `moccur.el' binding.
    (define-key isearch-mode-map (icicle-kbd "M-o") 'isearch-moccur))
  (define-key isearch-mode-map (icicle-kbd "C-o") nil))

(defun icicle-unbind-key-completion-keys-for-map-var (keymap-var &optional keys)
  "Unbind `S-TAB' in keymaps accessible from keymap KEYMAP-VAR.
KEYMAP-VAR should be bound to a keymap that has at least one prefix
keymap.  If KEYMAP-VAR is not bound to a keymap, it is ignored.

Actually, by default, it is the keys in `icicle-key-complete-keys'
that are unbound, not `S-TAB'.  And if optional arg KEYS is non-nil
then it, not `icicle-key-complete-keys', is the list of keys that are
unbound."
  (let ((temp  keymap-var))
    (when (boundp temp)
      (setq temp  (symbol-value temp))
      (when (keymapp temp) (icicle-unbind-key-completion-keys-in-keymaps-from temp keys)))))

(defun icicle-unbind-key-completion-keys-in-keymaps-from (map &optional keys)
  "Unbind `icicle-key-complete-keys' in keymaps accessible from MAP.
Each key in `icicle-complete-keys' (or optional arg KEYS, if non-nil)
is unbound in all keymaps accessible from keymap MAP."
  (dolist (key+map  (accessible-keymaps map))
    (let ((map  (cdr key+map)))
      (while (and (symbolp map)  (fboundp map)) (setq map  (symbol-function map))) ; Get a list.
      (when (and (keymapp map)
                 (not (eq 'autoload (car-safe map))) ; Skip autoload keymaps.
                 (not (stringp (car-safe (last map))))) ; Try to exclude menu maps.
        (dolist (key  (or keys  icicle-key-complete-keys))
          (when (eq (lookup-key map key) 'icicle-complete-keys)
            (condition-case nil (define-key map key nil) (error nil))))))))
 
;;(@* "Other Icicles functions that define Icicle mode")

;;; Other Icicles functions that define Icicle mode ------------------

(defun icicle-skip-this-command ()
  "Prevent `handle-switch-frame' from being added to `this-command'."
  (interactive)
  (setq this-command  last-command))

(defun icicle-handle-switch-frame (event)
  "Call `handle-switch-frame', but don't add it to `this-command'."
  (interactive "e")
  (handle-switch-frame event)
  (setq this-command  last-command))

(defun icicle-define-minibuffer-maps (turn-on-p)
  "Define keymaps for the minibuffer and buffer `*Completions*'."
  (cond
    (turn-on-p                          ; TURN IT ON ********************************

     ;; `minibuffer-local-map': default minibuffer map.
     (let ((map  minibuffer-local-map))

       ;; Menu-bar `Minibuf' menu.

       ;; First, remove some standard bindings that are on submenus here.
       (define-key map [menu-bar minibuf isearch-forward]  nil)
       (define-key map [menu-bar minibuf isearch-backward] nil)
       (define-key map [menu-bar minibuf next]             nil)
       (define-key map [menu-bar minibuf previous]         nil)

       (define-key-after (lookup-key map [menu-bar minibuf]) [icicle-top-level]
         '(menu-item "Top Level" icicle-top-level
           :help "Cancel all minibuffers and return to the top level")
         'quit)
       (define-key map [menu-bar minibuf quit] ; Replace `keyboard-escape-quit'
         '(menu-item "Quit" icicle-abort-recursive-edit
           :help "Cancel this minibuffer and return to the next higher level"))
       (define-key map [menu-bar minibuf return]
         '(menu-item "Enter" exit-minibuffer
           :help "Terminate input and exit minibuffer" :keys "RET"))
       (define-key map [menu-bar minibuf separator-help] '("--"))

       (when (fboundp 'icicle-complete-keys)
         (define-key map [menu-bar minibuf icicle-complete-keys]
           '(menu-item "Show Available Keys (Complete Key)" icicle-complete-keys
             :help "Show available keys (`C-g') or complete prefix key")))
       (define-key map [menu-bar minibuf completion-help]
         '(menu-item "Icicles Help" icicle-minibuffer-help
           :help "Display help for minibuffer input and completion" :keys "M-?"))
       (define-key map [menu-bar minibuf separator-last] '("--"))

       (define-key map [menu-bar minibuf icicle-toggle-search-complementing-domain]
         '(menu-item "Toggle Searching Complement"
           icicle-toggle-search-complementing-domain
           :help "Toggle `icicle-search-complement-domain-p'" :keys "C-M-~"))
       (define-key map [menu-bar minibuf icicle-toggle-highlight-all-current]
         '(menu-item "Toggle All-Current Icicle-Search Highlighting"
           icicle-toggle-highlight-all-current :enable icicle-searching-p
           :help "Toggle `icicle-search-highlight-all-current-flag'" :keys "C-^"))
       (define-key map [menu-bar minibuf icicle-multi-inputs-save]
         '(menu-item "Save Multiple Inputs for Completion" icicle-multi-inputs-save
           :help "Add inputs in minibuffer to saved candidates set for completion"))
       (define-key map [menu-bar minibuf icicle-multi-inputs-act]
         '(menu-item "Act on Multiple Inputs" icicle-multi-inputs-act
           :help "Parse minibuffer input into a list of candidates, then act on each"))
       (define-key map [menu-bar minibuf separator-misc] '("--"))

       (define-key map [menu-bar minibuf edit]
         (list 'menu-item "Edit" icicle-minibuf-edit-menu-map))

       (define-key map [menu-bar minibuf history]
         (list 'menu-item "History" icicle-minibuf-history-menu-map))

       ;; Keyboard keys
       (icicle-bind-custom-minibuffer-keys map icicle-minibuffer-key-bindings)
       (define-key map (icicle-kbd "C-g")  'icicle-abort-recursive-edit) ; `C-g'
       (dolist (key  icicle-completing-read+insert-keys)
         (define-key map key 'icicle-completing-read+insert)) ; `C-M-S-c'
       (dolist (key  icicle-read+insert-file-name-keys)
         (define-key map key 'icicle-read+insert-file-name)) ; `C-M-S-f'
       )                                ; End `minibuffer-local-map'.

     ;; `minibuffer-local-ns-map': default minibuffer map when spaces are not allowed.
     ;; In Emacs 22+, local is parent of local-ns.
     (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-ns-map))
       (let ((map  minibuffer-local-ns-map))
         (define-key-after (lookup-key map [menu-bar minibuf]) [icicle-top-level]
           '(menu-item "Top Level" icicle-top-level
             :help "Cancel all minibuffers and return to the top level")
           'quit)
         (define-key map [menu-bar minibuf quit] ; Replace `keyboard-escape-quit'
           '(menu-item "Quit" icicle-abort-recursive-edit
             :help "Cancel this minibuffer and return to the next higher level"))
         (define-key map [menu-bar minibuf return]
           '(menu-item "Enter" exit-minibuffer
             :help "Terminate input and exit minibuffer" :keys "RET"))
         (define-key map [menu-bar minibuf separator-help] '("--"))

         (when (fboundp 'icicle-complete-keys)
           (define-key map [menu-bar minibuf icicle-complete-keys]
             '(menu-item "Show Available Keys (Complete Key)" icicle-complete-keys
               :help "Show available keys (`C-g') or complete prefix key")))
         (define-key map [menu-bar minibuf completion-help]
           '(menu-item "Icicles Help" icicle-minibuffer-help
             :help "Display help for minibuffer input and completion" :keys "M-?"))
         (define-key map [menu-bar minibuf separator-last] '("--"))

         (define-key map [menu-bar minibuf icicle-toggle-search-complementing-domain]
           '(menu-item "Toggle Searching Complement"
             icicle-toggle-search-complementing-domain
             :help "Toggle `icicle-search-complement-domain-p'" :keys "C-M-~"))
         (define-key map [menu-bar minibuf icicle-toggle-highlight-all-current]
           '(menu-item "Toggle All-Current Icicle-Search Highlighting"
             icicle-toggle-highlight-all-current :enable icicle-searching-p
             :help "Toggle `icicle-search-highlight-all-current-flag'" :keys "C-^"))
         (define-key map [menu-bar minibuf icicle-multi-inputs-save]
           '(menu-item "Save Multiple Inputs for Completion" icicle-multi-inputs-save
             :help "Add inputs in minibuffer to saved candidates set for completion"))
         (define-key map [menu-bar minibuf icicle-multi-inputs-act]
           '(menu-item "Act on Multiple Inputs" icicle-multi-inputs-act
             :help "Parse minibuffer input into a list of candidates, then act on each"))
         (define-key map [menu-bar minibuf separator-misc] '("--"))

         (define-key map [menu-bar minibuf edit]
           (list 'menu-item "Edit" icicle-minibuf-edit-menu-map))

         (define-key map [menu-bar minibuf history]
           (list 'menu-item "History" icicle-minibuf-history-menu-map))

         ;; Keyboard keys
         (icicle-bind-custom-minibuffer-keys map icicle-minibuffer-key-bindings)
         (define-key map (icicle-kbd "C-g") 'icicle-abort-recursive-edit) ; `C-g'
         (dolist (key  icicle-completing-read+insert-keys)
           (define-key map key 'icicle-completing-read+insert)) ; `C-M-S-c'
         (dolist (key  icicle-read+insert-file-name-keys)
           (define-key map key 'icicle-read+insert-file-name)) ; `C-M-S-f'
         ))                             ; End `minibuffer-local-ns-map'.

     ;; `minibuffer-local-isearch-map': minibuffer map for editing isearch strings.
     ;; In Emacs 21+, local is parent of local-isearch.
     (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-isearch-map))

       (let ((map  minibuffer-local-isearch-map))
         (define-key-after (lookup-key map [menu-bar minibuf]) [icicle-top-level]
           '(menu-item "Top Level" icicle-top-level
             :help "Cancel all minibuffers and return to the top level")
           'quit)
         (define-key map [menu-bar minibuf quit] ; Replace `keyboard-escape-quit'
           '(menu-item "Quit" icicle-abort-recursive-edit
             :help "Cancel this minibuffer and return to the next higher level"))
         (define-key map [menu-bar minibuf return]
           '(menu-item "Enter" exit-minibuffer
             :help "Terminate input and exit minibuffer" :keys "RET"))
         (define-key map [menu-bar minibuf separator-help] '("--"))

         (when (fboundp 'icicle-complete-keys)
           (define-key map [menu-bar minibuf icicle-complete-keys]
             '(menu-item "Show Available Keys (Complete Key)" icicle-complete-keys
               :help "Show available keys (`C-g') or complete prefix key")))
         (define-key map [menu-bar minibuf completion-help]
           '(menu-item "Icicles Help" icicle-minibuffer-help
             :help "Display help for minibuffer input and completion" :keys "M-?"))
         (define-key map [menu-bar minibuf separator-last] '("--"))

         (define-key map [menu-bar minibuf icicle-toggle-search-complementing-domain]
           '(menu-item "Toggle Searching Complement"
             icicle-toggle-search-complementing-domain
             :help "Toggle `icicle-search-complement-domain-p'" :keys "C-M-~"))
         (define-key map [menu-bar minibuf icicle-toggle-highlight-all-current]
           '(menu-item "Toggle All-Current Icicle-Search Highlighting"
             icicle-toggle-highlight-all-current :enable icicle-searching-p
             :help "Toggle `icicle-search-highlight-all-current-flag'" :keys "C-^"))
         (define-key map [menu-bar minibuf icicle-multi-inputs-save]
           '(menu-item "Save Multiple Inputs for Completion" icicle-multi-inputs-save
             :help "Add inputs in minibuffer to saved candidates set for completion"))
         (define-key map [menu-bar minibuf icicle-multi-inputs-act]
           '(menu-item "Act on Multiple Inputs" icicle-multi-inputs-act
             :help "Parse minibuffer input into a list of candidates, then act on each"))
         (define-key map [menu-bar minibuf separator-misc] '("--"))

         (define-key map [menu-bar minibuf edit]
           (list 'menu-item "Edit" icicle-minibuf-edit-menu-map))

         (define-key map [menu-bar minibuf history]
           (list 'menu-item "History" icicle-minibuf-history-menu-map))

         ;; Keyboard keys
         (icicle-bind-custom-minibuffer-keys map icicle-minibuffer-key-bindings)
         (define-key map (icicle-kbd "C-g") 'icicle-abort-recursive-edit) ; `C-g'
         (dolist (key  icicle-completing-read+insert-keys)
           (define-key map key 'icicle-completing-read+insert)) ; `C-M-S-c'
         (dolist (key  icicle-read+insert-file-name-keys)
           (define-key map key 'icicle-read+insert-file-name)) ; `C-M-S-f'
         ))                             ; End `minibuffer-local-isearch-map'.

     ;; `minibuffer-local-completion-map': completion map.
     (icicle-bind-completion-keys minibuffer-local-completion-map)

     ;; `minibuffer-local-must-match-map': must-match map.
     ;; In Emacs 22+, local-completion is parent of local-must-match.
     (let ((map  minibuffer-local-must-match-map))
       (if (not (eq minibuffer-local-completion-map (keymap-parent map)))
           (icicle-bind-completion-keys map)
         ;; Keyboard keys
         (icicle-bind-custom-minibuffer-keys map icicle-minibuffer-key-bindings)
         ;; Need `C-g' anyway, even if `minibuffer-local-must-match-map' inherits completion map.
         (define-key map (icicle-kbd "C-g") 'icicle-abort-recursive-edit) ; `C-g'
         (dolist (key  icicle-completing-read+insert-keys)
           (define-key map key 'icicle-completing-read+insert)) ; `C-M-S-c'
         (dolist (key  icicle-read+insert-file-name-keys)
           (define-key map key 'icicle-read+insert-file-name)) ; `C-M-S-f'
         ))                             ; End `minibuffer-local-must-match-map'.

     ;; `completion-list-mode-map': map for `*Completions*' buffer.
     ;; Abort on `C-g' or `q'.  Switch to minibuffer on `C-insert'.  Do not allow normal input.
     (let ((map  completion-list-mode-map))
       ;; Keyboard keys
       (icicle-bind-custom-minibuffer-keys map icicle-completion-list-key-bindings)
       (dolist (key  icicle-candidate-help-keys) ; `C-M-return', `C-help', `C-M-help', `C-f1',
         (define-key map key 'icicle-help-on-candidate)) ; `C-M-f1'
       (define-key map (icicle-kbd "C-g")      'icicle-abort-recursive-edit) ; `C-g'
       (define-key map (icicle-kbd "C-M-S-t")  'icicle-top-level) ; `C-M-S-t' (aka `C-M-T')
       ))                               ; End `completion-list-mode-map'.

    (t                                  ; TURN IT OFF *******************************

     ;; `minibuffer-local-map': default minibuffer map.
     (let ((map  minibuffer-local-map))

       ;; Menu-bar `Minibuf' menu.
       (define-key map [menu-bar minibuf quit]
         '(menu-item "Quit" keyboard-escape-quit :help "Abort input and exit minibuffer"))
       (define-key map [menu-bar minibuf return]
         '(menu-item "Enter" exit-minibuffer
           :help "Terminate input and exit minibuffer" :keys "RET"))
       (define-key map [menu-bar minibuf separator-help]                            nil)
       (define-key map [menu-bar minibuf completion-help]                           nil)
       (define-key map [menu-bar minibuf separator-last]                            nil)
       (define-key map [menu-bar minibuf icicle-toggle-search-complementing-domain] nil)
       (define-key map [menu-bar minibuf icicle-toggle-highlight-all-current]       nil)
       (define-key map [menu-bar minibuf icicle-regexp-quote-input]                 nil)
       (define-key map [menu-bar minibuf separator-set2]                            nil)
       (define-key map [menu-bar minibuf icicle-clear-current-history]              nil)
       (define-key map [menu-bar minibuf icicle-erase-minibuffer]                   nil)
       (define-key map [menu-bar minibuf icicle-delete-history-element]             nil)
       (define-key map [menu-bar minibuf icicle-insert-list-join-string]            nil)
       (define-key map [menu-bar minibuf icicle-insert-key-description]             nil)
       (define-key map [menu-bar minibuf icicle-insert-history-element]             nil)
       (define-key map [menu-bar minibuf icicle-insert-string-from-a-var]           nil)
       (define-key map [menu-bar minibuf icicle-insert-string-from-std-var]         nil)
       (define-key map [menu-bar minibuf icicle-insert-string-at-point]             nil)
       (define-key map [menu-bar minibuf icicle-completing-read+insert]             nil)
       (define-key map [menu-bar minibuf icicle-read+insert-file-name]              nil)

       ;; Keyboard keys
       (icicle-restore-custom-minibuffer-keys map icicle-minibuffer-key-bindings)
       (dolist (key  icicle-completing-read+insert-keys) (define-key map key nil)) ; `C-M-S-c'
       (dolist (key  icicle-read+insert-file-name-keys) (define-key map key nil)) ; `C-M-S-f'
       ;; Do the non-nil bindings no matter what might be in `icicle-minibuffer-key-bindings'.
       (define-key map (icicle-kbd "C-g")           (if (and (fboundp 'minibuffer-keyboard-quit)
                                                             delete-selection-mode)
                                                        'minibuffer-keyboard-quit
                                                      'abort-recursive-edit)) ; `C-g'
       (define-key map (icicle-kbd "M-r")           'previous-matching-history-element) ; `M-r'
       (define-key map (icicle-kbd "M-s")           'next-matching-history-element) ; `M-s'
       (define-key map (icicle-kbd "C-j")           'exit-minibuffer) ; `C-j'
       )                                ; End `minibuffer-local-map'.

     ;; `minibuffer-local-ns-map': default minibuffer map when spaces are not allowed.
     ;; In Emacs 22+, local is parent of local-ns.
     (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-ns-map))
       (let ((map  minibuffer-local-ns-map))
         (define-key map [menu-bar minibuf quit]
           '(menu-item "Quit" keyboard-escape-quit :help "Abort input and exit minibuffer"))
         (define-key map [menu-bar minibuf return]
           '(menu-item "Enter" exit-minibuffer
             :help "Terminate input and exit minibuffer" :keys "RET"))
         (define-key map [menu-bar minibuf separator-help]                            nil)
         (define-key map [menu-bar minibuf completion-help]                           nil)
         (define-key map [menu-bar minibuf separator-last]                            nil)
         (define-key map [menu-bar minibuf icicle-toggle-search-complementing-domain] nil)
         (define-key map [menu-bar minibuf icicle-toggle-highlight-all-current]       nil)
         (define-key map [menu-bar minibuf icicle-regexp-quote-input]                 nil)
         (define-key map [menu-bar minibuf separator-set2]                            nil)
         (define-key map [menu-bar minibuf icicle-clear-current-history]              nil)
         (define-key map [menu-bar minibuf icicle-erase-minibuffer]                   nil)
         (define-key map [menu-bar minibuf icicle-delete-history-element]             nil)
         (define-key map [menu-bar minibuf icicle-insert-list-join-string]            nil)
         (define-key map [menu-bar minibuf icicle-insert-key-description]             nil)
         (define-key map [menu-bar minibuf icicle-insert-history-element]             nil)
         (define-key map [menu-bar minibuf icicle-insert-string-from-a-var]           nil)
         (define-key map [menu-bar minibuf icicle-insert-string-from-std-var]         nil)
         (define-key map [menu-bar minibuf icicle-insert-string-at-point]             nil)
         (define-key map [menu-bar minibuf icicle-completing-read+insert]             nil)
         (define-key map [menu-bar minibuf icicle-read+insert-file-name]              nil)

         ;; Keyboard keys
         (icicle-restore-custom-minibuffer-keys map icicle-minibuffer-key-bindings)
         (dolist (key  icicle-completing-read+insert-keys) (define-key map key nil)) ; `C-M-S-c'
         (dolist (key  icicle-read+insert-file-name-keys) (define-key map key nil)) ; `C-M-S-f'
         ;; Do the non-nil bindings no matter what might be in `icicle-minibuffer-key-bindings'.
         (define-key map (icicle-kbd "C-g")           (if (and (fboundp 'minibuffer-keyboard-quit)
                                                               delete-selection-mode)
                                                          'minibuffer-keyboard-quit
                                                        'abort-recursive-edit)) ; `C-g'
         (define-key map (icicle-kbd "M-r")           'previous-matching-history-element) ; `M-r'
         (define-key map (icicle-kbd "M-s")           'next-matching-history-element) ; `M-s'
         (define-key map (icicle-kbd "C-j")           'exit-minibuffer) ; `C-j'
         ))                             ; End `minibuffer-local-ns-map'.

     ;; `minibuffer-local-isearch-map': minibuffer map for editing isearch strings.
     ;; In Emacs 21+, local is parent of local-isearch
     (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-isearch-map))
       (let ((map  minibuffer-local-isearch-map))
         (define-key map [menu-bar minibuf quit]
           '(menu-item "Quit" keyboard-escape-quit :help "Abort input and exit minibuffer"))
         (define-key map [menu-bar minibuf return]
           '(menu-item "Enter" exit-minibuffer
             :help "Terminate input and exit minibuffer" :keys "RET"))
         (define-key map [menu-bar minibuf separator-help]                            nil)
         (define-key map [menu-bar minibuf completion-help]                           nil)
         (define-key map [menu-bar minibuf separator-last]                            nil)
         (define-key map [menu-bar minibuf icicle-toggle-search-complementing-domain] nil)
         (define-key map [menu-bar minibuf icicle-toggle-highlight-all-current]       nil)
         (define-key map [menu-bar minibuf icicle-regexp-quote-input]                 nil)
         (define-key map [menu-bar minibuf separator-set2]                            nil)
         (define-key map [menu-bar minibuf icicle-clear-current-history]              nil)
         (define-key map [menu-bar minibuf icicle-erase-minibuffer]                   nil)
         (define-key map [menu-bar minibuf icicle-delete-history-element]             nil)
         (define-key map [menu-bar minibuf icicle-insert-list-join-string]            nil)
         (define-key map [menu-bar minibuf icicle-insert-key-description]             nil)
         (define-key map [menu-bar minibuf icicle-insert-history-element]             nil)
         (define-key map [menu-bar minibuf icicle-insert-string-from-a-var]           nil)
         (define-key map [menu-bar minibuf icicle-insert-string-from-std-var]         nil)
         (define-key map [menu-bar minibuf icicle-insert-string-at-point]             nil)
         (define-key map [menu-bar minibuf icicle-completing-read+insert]             nil)
         (define-key map [menu-bar minibuf icicle-read+insert-file-name]              nil)

         ;; Keyboard keys
         (icicle-restore-custom-minibuffer-keys map icicle-minibuffer-key-bindings)
         (dolist (key  icicle-completing-read+insert-keys) (define-key map key nil)) ; `C-M-S-c'
         (dolist (key  icicle-read+insert-file-name-keys) (define-key map key nil)) ; `C-M-S-f'
         ;; Do the non-nil bindings no matter what might be in `icicle-minibuffer-key-bindings'.
         (define-key map (icicle-kbd "C-g")           (if (and (fboundp 'minibuffer-keyboard-quit)
                                                               delete-selection-mode)
                                                          'minibuffer-keyboard-quit
                                                        'abort-recursive-edit)) ; `C-g'
         (define-key map (icicle-kbd "M-r")           'previous-matching-history-element) ; `M-r'
         (define-key map (icicle-kbd "M-s")           'next-matching-history-element) ; `M-s'
         (define-key map (icicle-kbd "C-j")           'exit-minibuffer) ; `C-j'
         ))                             ; End `minibuffer-local-isearch-map'.

     ;; `minibuffer-local-completion-map': completion map.
     (icicle-restore-completion-keys minibuffer-local-completion-map)

     ;; `minibuffer-local-must-match-map': must-match map.
     ;; In Emacs 22+, local-completion is parent of local-must-match
     (let ((map  minibuffer-local-must-match-map))
       (if (not (eq minibuffer-local-completion-map (keymap-parent map)))
           (icicle-restore-completion-keys map)
         ;; Keyboard keys
         (icicle-restore-custom-minibuffer-keys map icicle-minibuffer-key-bindings)
         (dolist (key  icicle-completing-read+insert-keys) (define-key map key nil))
         (dolist (key  icicle-read+insert-file-name-keys) (define-key map key nil))
         ;; Do the non-nil bindings no matter what might be in `icicle-minibuffer-key-bindings'.
         ;; Need do `C-g' anyway, even if inherit completion map.
         (define-key map (icicle-kbd "C-g")           (if (and (fboundp 'minibuffer-keyboard-quit)
                                                               delete-selection-mode)
                                                          'minibuffer-keyboard-quit
                                                        'abort-recursive-edit)) ; `C-g'
         (define-key map (icicle-kbd "M-r")           'previous-matching-history-element) ; `M-r'
         (define-key map (icicle-kbd "M-s")           'next-matching-history-element) ; `M-s'
         (define-key map (icicle-kbd "C-j")           'minibuffer-complete-and-exit) ; `C-j' (newline)
         ))                             ; End `minibuffer-local-must-match-map'.

     ;; `completion-list-mode-map': map for `*Completions*' buffer.
     (let ((map  completion-list-mode-map))
       ;; Keyboard keys
       (icicle-restore-custom-minibuffer-keys map icicle-completion-list-key-bindings)
       (dolist (key  icicle-candidate-help-keys)       (define-key map key nil))
       (define-key map (icicle-kbd "C-g")              nil)
       (define-key map (icicle-kbd "C-M-S-t")          nil)
       ;; Do these last:
       (define-key map (icicle-kbd "right")            'next-completion)
       (define-key map (icicle-kbd "left")             'previous-completion))))
  (when (and (interactive-p)  turn-on-p)
    (message (substitute-command-keys
              "Use `\\<minibuffer-local-completion-map>\
\\[icicle-minibuffer-help]' in minibuffer for help."))))

(defun icicle-unmap (command map current)
  "In MAP, unbind any keys that are bound to COMMAND.
If command remapping is available, remap COMMAND to nil in MAP,
unbinding it.
Otherwise, bind COMMAND to whatever CURRENT is bound to in MAP."
  (if (fboundp 'command-remapping)
      (define-key map (vector 'remap command) nil)
    (substitute-key-definition current command map)))

(defun icicle-rebind-global (old new map)
  "Bind command NEW in MAP to all keys currently bound globally to OLD."
  (substitute-key-definition old new map (current-global-map)))

(defun icicle-bind-completion-keys (map)
  "Bind keys for minibuffer completion map MAP.
MAP is `minibuffer-local-completion-map' or
`minibuffer-local-must-match-map'."

  (define-key map (icicle-kbd "M-i") 'icicle-toggle-map)

  ;; Menu-bar `Minibuf' menu.

  (define-key map [menu-bar minibuf separator-complete2] '("--"))
  (define-key map [menu-bar minibuf word-complete]
    '(menu-item "Word-Complete" icicle-prefix-word-complete
      :help "Complete at most one word of prefix"))
  (define-key map [menu-bar minibuf prefix-complete]
    '(menu-item "Prefix-Complete" icicle-prefix-complete
      :help "Complete prefix as far as possible"))
  (define-key map [menu-bar minibuf apropos-complete]
    '(menu-item "Apropos-Complete" icicle-apropos-complete :keys "S-TAB"
      :help "Complete regular expression as far as possible and list completions"))
  (define-key map [menu-bar minibuf separator-complete1] '("--"))


  ;; In Emacs 22+, local is parent of local-completion
  (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map))
    (define-key-after (lookup-key map [menu-bar minibuf]) [icicle-top-level]
      '(menu-item "Top Level" icicle-top-level
        :help "Cancel all minibuffers and return to the top level")
      'quit)
    (define-key map [menu-bar minibuf icicle-recomplete-from-original-domain]
      '(menu-item "Recompute Completions" icicle-recomplete-from-original-domain
        :enable (and (active-minibuffer-window)  icicle-completing-p)
        :help "Recomplete your last typed input, using the original domain."))
    (define-key map [menu-bar minibuf quit] ; Replace `keyboard-escape-quit'
      '(menu-item "Quit" icicle-abort-recursive-edit
        :help "Cancel this minibuffer and return to the next higher level"))
    (define-key map [menu-bar minibuf return]
      '(menu-item "Enter" exit-minibuffer
        :help "Terminate input and exit minibuffer" :keys "RET"))
    (define-key map [menu-bar minibuf separator-help] '("--"))

    (when (fboundp 'icicle-complete-keys)
      (define-key map [menu-bar minibuf icicle-complete-keys]
          '(menu-item "Show Available Keys (Complete Key)" icicle-complete-keys
            :help "Show available keys (`C-g') or complete prefix key")))
    (define-key map [menu-bar minibuf completion-help]
      '(menu-item "Icicles Help" icicle-minibuffer-help
        :help "Display help for minibuffer input and completion" :keys "M-?"))
    (define-key map [menu-bar minibuf separator-last] '("--"))

    (define-key map [menu-bar minibuf icicle-toggle-search-complementing-domain]
      '(menu-item "Toggle Searching Complement"
        icicle-toggle-search-complementing-domain
        :help "Toggle `icicle-search-complement-domain-p'" :keys "C-M-~"))
    (define-key map [menu-bar minibuf icicle-toggle-highlight-all-current]
      '(menu-item "Toggle All-Current Icicle-Search Highlighting"
        icicle-toggle-highlight-all-current :enable icicle-searching-p
        :help "Toggle `icicle-search-highlight-all-current-flag'" :keys "C-^"))
    (define-key map [menu-bar minibuf icicle-multi-inputs-save]
      '(menu-item "Save Multiple Inputs for Completion" icicle-multi-inputs-save
        :help "Add inputs in minibuffer to saved candidates set for completion"))
    (define-key map [menu-bar minibuf icicle-multi-inputs-act]
      '(menu-item "Act on Multiple Inputs" icicle-multi-inputs-act
        :help "Parse minibuffer input into a list of candidates, then act on each"))
    (define-key map [menu-bar minibuf separator-misc] '("--"))

    ;; Submenus.
    (define-key map [menu-bar minibuf edit]
      (list 'menu-item "Edit" icicle-minibuf-edit-menu-map))
    (define-key map [menu-bar minibuf history]
      (list 'menu-item "History" icicle-minibuf-history-menu-map))

    )

  ;; Remove some standard menu items.
  (define-key map [menu-bar minibuf ?\?] nil)
  (define-key map [menu-bar minibuf space] nil)
  (define-key map [menu-bar minibuf tab] nil)

  ;; Submenus.
  (define-key map [menu-bar minibuf candidate-action]
    (list 'menu-item "Act on All Candidates" icicle-minibuf-act-on-all-menu-map))
  (define-key map [menu-bar minibuf candidate-set]
    (list 'menu-item "Candidate Set" icicle-minibuf-candidate-set-menu-map))
  (define-key map [menu-bar minibuf save/retrieve]
    (list 'menu-item "Save/Retrieve Candidates" icicle-minibuf-save-retrieve-menu-map))

  ;; Remap some commands for completion.
  (icicle-remap 'self-insert-command           'icicle-self-insert map (current-global-map))
  (icicle-remap 'universal-argument            'icicle-universal-argument ; `C-u'
                map (current-global-map))
  (icicle-remap 'negative-argument             'icicle-negative-argument ; `M--'
                map (current-global-map))
  (icicle-remap 'digit-argument                'icicle-digit-argument ; `C-9'
                map (current-global-map))
  (icicle-remap 'backward-delete-char-untabify 'icicle-backward-delete-char-untabify ; `DEL'
                map (current-global-map))
  (icicle-remap 'delete-backward-char          'icicle-delete-backward-char ; `DEL'
                map (current-global-map))
  (icicle-remap 'delete-char                   'icicle-delete-char ; `C-d', `deletechar'
                map (current-global-map))
  (icicle-remap 'backward-kill-word            'icicle-backward-kill-word ; `M-DEL'
                map (current-global-map))
  (icicle-remap 'kill-word                     'icicle-kill-word ; `M-d'
                map (current-global-map))
  (icicle-remap 'backward-kill-sexp            'icicle-backward-kill-sexp ; `C-M-backspace'
                map (current-global-map))
  (icicle-remap 'kill-sexp                     'icicle-kill-sexp ; `C-M-k', `C-M-delete'
                map (current-global-map))
  (icicle-remap 'backward-kill-sentence        'icicle-backward-kill-sentence ; `C-x DEL'
                map (current-global-map))
  (icicle-remap 'backward-kill-paragraph       'icicle-backward-kill-paragraph ; `C-backspace'
                map (current-global-map))
  (icicle-remap 'kill-paragraph                'icicle-kill-paragraph ; `C-delete'
                map (current-global-map))
  (icicle-remap 'kill-line                     'icicle-kill-line ; `C-k', `deleteline'
                map (current-global-map))
  (icicle-remap 'reposition-window             'icicle-goto/kill-failed-input ; `C-M-l'
                map (current-global-map))
  (icicle-remap 'transpose-chars               'icicle-transpose-chars ; `C-t'
                map (current-global-map))
  (icicle-remap 'transpose-words               'icicle-transpose-words ; `M-t'
                map (current-global-map))
  (icicle-remap 'transpose-sexps               'icicle-transpose-sexps ; `C-M-t'
                map (current-global-map))
  (icicle-remap 'yank-pop                      'icicle-yank-pop ; `M-y', `M-insert'
                map (current-global-map))
  (icicle-remap 'mouse-yank-secondary          'icicle-mouse-yank-secondary ; `M-mouse-2'
                map (current-global-map))

  ;; Bind additional keys.
  (dolist (key  icicle-candidate-action-keys)
    (define-key map key 'icicle-candidate-action)) ; `C-return', `C-RET'
  (dolist (key  icicle-candidate-help-keys) ; `C-M-return', `C-M-RET', `C-help', `C-M-help',
    (define-key map key 'icicle-help-on-candidate)) ;  `C-f1', `C-M-f1'

  (dolist (key  icicle-word-completion-keys)
    (define-key map key 'icicle-prefix-word-complete)) ; `M-SPC'
  (dolist (key  icicle-apropos-complete-keys)
    (define-key map key 'icicle-apropos-complete)) ; `S-TAB'
  (dolist (key  icicle-prefix-complete-keys) (define-key map key 'icicle-prefix-complete)) ; `TAB'
  (dolist (key  icicle-apropos-complete-no-display-keys)
    (define-key map key 'icicle-apropos-complete-no-display)) ; `C-M-S-TAB'
  (dolist (key  icicle-prefix-complete-no-display-keys)
    (define-key map key 'icicle-prefix-complete-no-display)) ; `C-M-TAB'
  (icicle-define-cycling-keys map)      ;     `up',     `down',     `prior',     `next',
                                        ;   `C-up',   `C-down',   `C-prior',   `C-next',
                                        ; `C-M-up', `C-M-down', `C-M-prior', `C-M-next',
                                        ; `C-S-up', `C-S-down', `C-S-prior', `C-S-next',
  (icicle-bind-custom-minibuffer-keys map icicle-completion-key-bindings))

(defun icicle-restore-completion-keys (map)
  "Restore standard keys for minibuffer completion map MAP.
MAP is `minibuffer-local-completion-map',
`minibuffer-local-filename-completion-map', or
`minibuffer-local-must-match-map'."

  (define-key map (icicle-kbd "M-i") nil)

  ;; Menu-bar `Minibuf' menu.
  ;; In Emacs 22+, local is parent of local-completion
  (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map))
    (define-key map [menu-bar minibuf quit]
      '(menu-item "Quit" keyboard-escape-quit :help "Abort input and exit minibuffer"))
    (define-key map [menu-bar minibuf return]
      '(menu-item "Enter" exit-minibuffer
        :help "Terminate input and exit minibuffer" :keys "RET"))
    (define-key map [menu-bar minibuf separator-help]                            nil)
    (define-key map [menu-bar minibuf completion-help]                           nil)
    (define-key map [menu-bar minibuf separator-last]                            nil)
    (define-key map [menu-bar minibuf icicle-clear-current-history]              nil)
    (define-key map [menu-bar minibuf icicle-toggle-search-complementing-domain] nil)
    (define-key map [menu-bar minibuf icicle-toggle-highlight-all-current]       nil)
    (define-key map [menu-bar minibuf icicle-regexp-quote-input]                 nil)
    (define-key map [menu-bar minibuf separator-set2]                            nil)
    (define-key map [menu-bar minibuf icicle-clear-current-history]              nil)
    (define-key map [menu-bar minibuf icicle-erase-minibuffer]                   nil)
    (define-key map [menu-bar minibuf icicle-delete-history-element]             nil)
    (define-key map [menu-bar minibuf icicle-insert-list-join-string]            nil)
    (define-key map [menu-bar minibuf icicle-insert-key-description]             nil)
    (define-key map [menu-bar minibuf icicle-insert-history-element]             nil)
    (define-key map [menu-bar minibuf icicle-insert-string-from-a-var]           nil)
    (define-key map [menu-bar minibuf icicle-insert-string-from-std-var]         nil)
    (define-key map [menu-bar minibuf icicle-insert-string-at-point]             nil)
    (define-key map [menu-bar minibuf icicle-completing-read+insert]             nil)
    (define-key map [menu-bar minibuf icicle-read+insert-file-name]              nil)
    )
  (define-key map [menu-bar minibuf icicle-goto/kill-failed-input]           nil)
  (define-key map [menu-bar minibuf icicle-retrieve-next-input]              nil)
  (define-key map [menu-bar minibuf icicle-retrieve-previous-input]          nil)
  (define-key map [menu-bar minibuf separator-C-l]                           nil)
  (define-key map [menu-bar minibuf alt-action-list-all]                     nil)
  (define-key map [menu-bar minibuf alt-action-all]                          nil)
  (define-key map [menu-bar minibuf action-list-all]                         nil)
  (define-key map [menu-bar minibuf action-all]                              nil)
  (define-key map [menu-bar minibuf separator-actions]                       nil)
  (define-key map [menu-bar minibuf set-define]                              nil)
  (define-key map [menu-bar minibuf icicle-keep-only-past-inputs]            nil)
  (define-key map [menu-bar minibuf set-union]                               nil)
  (define-key map [menu-bar minibuf set-difference]                          nil)
  (define-key map [menu-bar minibuf set-intersection]                        nil)
  (define-key map [menu-bar minibuf icicle-save-predicate-to-variable]       nil)
  (define-key map [menu-bar minibuf icicle-narrow-candidates-with-predicate] nil)
  (define-key map [menu-bar minibuf icicle-narrow-candidates]                nil)
  (define-key map [menu-bar minibuf icicle-widen-candidates]                 nil)
  (define-key map [menu-bar minibuf set-complement]                          nil)
  (define-key map [menu-bar minibuf separator-set1]                          nil)
  (define-key map [menu-bar minibuf set-swap]                                nil)
  (define-key map [menu-bar minibuf icicle-candidate-set-save-more-selected] nil)
  (define-key map [menu-bar minibuf icicle-candidate-set-save-selected]      nil)
  (define-key map [menu-bar minibuf icicle-candidate-set-save-more]          nil)
  (define-key map [menu-bar minibuf set-retrieve-from-cache-file]            nil)
  (define-key map [menu-bar minibuf set-retrieve-from-variable]              nil)
  (define-key map [menu-bar minibuf set-retrieve]                            nil)
  (define-key map [menu-bar minibuf set-save-to-cache-file]                  nil)
  (define-key map [menu-bar minibuf set-save-to-variable]                    nil)
  (define-key map [menu-bar minibuf set-save]                                nil)
  (define-key map [menu-bar minibuf separator-set2]                          nil)
  (define-key map [menu-bar minibuf word-complete]                           nil)
  (define-key map [menu-bar minibuf prefix-complete]                         nil)
  (define-key map [menu-bar minibuf apropos-complete]                        nil)
  (define-key map [menu-bar minibuf ?\?]
    '(menu-item "List Completions" minibuffer-completion-help
      :help "Display all possible completions"))
  (define-key map [menu-bar minibuf space]
    '(menu-item "Complete Word" minibuffer-complete-word :help "Complete at most one word"))
  (define-key map [menu-bar minibuf tab]
    '(menu-item "Complete" minibuffer-complete :help "Complete as far as possible"))

  ;; Unmap commands that were bound for completion.
  (icicle-unmap 'self-insert-command           map 'icicle-self-insert)
  (icicle-unmap 'universal-argument            map 'icicle-universal-argument)
  (icicle-unmap 'negative-argument             map 'icicle-negative-argument)
  (icicle-unmap 'digit-argument                map 'icicle-digit-argument)
  (icicle-unmap 'backward-delete-char-untabify map 'icicle-backward-delete-char-untabify)
  (icicle-unmap 'delete-backward-char          map 'icicle-delete-backward-char)
  (icicle-unmap 'delete-char                   map 'icicle-delete-char)
  (icicle-unmap 'backward-kill-word            map 'icicle-backward-kill-word)
  (icicle-unmap 'kill-word                     map 'icicle-kill-word)
  (icicle-unmap 'backward-kill-sexp            map 'icicle-backward-kill-sexp)
  (icicle-unmap 'kill-sexp                     map 'icicle-kill-sexp)
  (icicle-unmap 'backward-kill-sentence        map 'icicle-backward-kill-sentence)
  (icicle-unmap 'backward-kill-paragraph       map 'icicle-backward-kill-paragraph)
  (icicle-unmap 'kill-paragraph                map 'icicle-kill-paragraph)
  (icicle-unmap 'kill-line                     map 'icicle-kill-line)
  (icicle-unmap 'reposition-window             map 'icicle-goto/kill-failed-input)
  (icicle-unmap 'transpose-chars               map 'icicle-transpose-chars)
  (icicle-unmap 'transpose-words               map 'icicle-transpose-words)
  (icicle-unmap 'transpose-sexps               map 'icicle-transpose-sexps)
  (icicle-unmap 'yank-pop                      map 'icicle-yank-pop)
  (icicle-unmap 'mouse-yank-secondary          map 'icicle-mouse-yank-secondary)

  ;; Restore additional bindings.
  ;; Do the option keys first, so they can be rebound as needed.
  (dolist (key  icicle-candidate-action-keys)                  (define-key map key nil))
  (dolist (key  icicle-candidate-help-keys)                    (define-key map key nil))
  (dolist (key  icicle-word-completion-keys)                   (define-key map key nil))
  (dolist (key  icicle-apropos-complete-keys)                  (define-key map key nil))
  (dolist (key  icicle-prefix-complete-keys)                   (define-key map key nil))
  (dolist (key  icicle-apropos-complete-no-display-keys)       (define-key map key nil))
  (dolist (key  icicle-prefix-complete-no-display-keys)        (define-key map key nil))
  (dolist (key  icicle-prefix-cycle-previous-keys)             (define-key map key nil))
  (dolist (key  icicle-prefix-cycle-next-keys)                 (define-key map key nil))
  (dolist (key  icicle-apropos-cycle-previous-keys)            (define-key map key nil))
  (dolist (key  icicle-apropos-cycle-next-keys)                (define-key map key nil))
  (dolist (key  icicle-prefix-cycle-previous-action-keys)      (define-key map key nil))
  (dolist (key  icicle-prefix-cycle-previous-alt-action-keys)  (define-key map key nil))
  (dolist (key  icicle-prefix-cycle-next-action-keys)          (define-key map key nil))
  (dolist (key  icicle-prefix-cycle-next-alt-action-keys)      (define-key map key nil))
  (dolist (key  icicle-apropos-cycle-previous-action-keys)     (define-key map key nil))
  (dolist (key  icicle-apropos-cycle-previous-alt-action-keys) (define-key map key nil))
  (dolist (key  icicle-apropos-cycle-next-action-keys)         (define-key map key nil))
  (dolist (key  icicle-apropos-cycle-next-alt-action-keys)     (define-key map key nil))
  (dolist (key  icicle-modal-cycle-up-keys)                    (define-key map key nil))
  (dolist (key  icicle-modal-cycle-down-keys)                  (define-key map key nil))
  (dolist (key  icicle-modal-cycle-up-action-keys)             (define-key map key nil))
  (dolist (key  icicle-modal-cycle-up-alt-action-keys)         (define-key map key nil))
  (dolist (key  icicle-modal-cycle-down-action-keys)           (define-key map key nil))
  (dolist (key  icicle-modal-cycle-down-alt-action-keys)       (define-key map key nil))
  (dolist (key  icicle-modal-cycle-up-help-keys)               (define-key map key nil))
  (dolist (key  icicle-modal-cycle-down-help-keys)             (define-key map key nil))
  (dolist (key  icicle-prefix-cycle-previous-help-keys)        (define-key map key nil))
  (dolist (key  icicle-prefix-cycle-next-help-keys)            (define-key map key nil))
  (dolist (key  icicle-apropos-cycle-previous-help-keys)       (define-key map key nil))
  (dolist (key  icicle-apropos-cycle-next-help-keys)           (define-key map key nil))

  (icicle-restore-custom-minibuffer-keys map icicle-completion-key-bindings)

  ;; Do these last. -----------------
  (define-key map (icicle-kbd "C-i")       'minibuffer-complete)
  (define-key map (icicle-kbd "tab")       'minibuffer-complete)
  (define-key map (icicle-kbd "?")         'minibuffer-completion-help)
  (define-key map (icicle-kbd "SPC")       'minibuffer-complete-word)
  (define-key map (icicle-kbd "C-g")       (if (and (fboundp 'minibuffer-keyboard-quit)
                                                    delete-selection-mode)
                                               'minibuffer-keyboard-quit
                                             'abort-recursive-edit))
  ;; In Emacs 22+, local is parent of local-completion
  (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map))
    (define-key map (icicle-kbd "C-j")     'exit-minibuffer))
  (define-key map (icicle-kbd "M-p")       'previous-history-element)
  (define-key map (icicle-kbd "M-n")       'next-history-element)
  (define-key map (icicle-kbd "up")        'previous-history-element)
  (define-key map (icicle-kbd "down")      'next-history-element)
  (define-key map (icicle-kbd "M-v")       'switch-to-completions)
  (define-key map (icicle-kbd "prior")     'switch-to-completions)
  (define-key map (icicle-kbd "next")      'next-history-element))

(defun icicle-bind-custom-minibuffer-keys (map option &optional defs)
  "Bind customizable keys for minibuffer completion map MAP.
These are the keys defined by option OPTION."
  (let (key command condition)
    (unless defs  (setq defs  option))
    (dolist (key-def  defs)
      (setq key        (car key-def)
            command    (cadr key-def)
            condition  (car (cddr key-def)))
      (when (eval condition)
        (if (symbolp key)
            (icicle-remap key command map)
          (define-key map key command))))))

(defun icicle-restore-custom-minibuffer-keys (map option)
  "Restore customizable keys for minibuffer map MAP.
These are the keys defined by option OPTION."
  (let (key condition)
    (dolist (key-def  option)
      (setq key        (car key-def)
            condition  (car (cddr key-def)))
      (when (eval condition)
        (if (symbolp key)
            (icicle-remap key nil map)
          (define-key map key nil))))))

(defun icicle-minibuffer-setup ()
  "Run in minibuffer on activation, to enable completion cycling.
Usually run by inclusion in `minibuffer-setup-hook'."
  (when (and icicle-mode  (window-minibuffer-p (selected-window))  (not executing-kbd-macro))
    ;; The pre- and post-command hooks are local to the
    ;; minibuffer, so they are added here, not in `icicle-mode'.
    ;; They are removed in `icicle-mode' when mode is exited.
    (unless (fboundp 'define-minor-mode) (make-local-hook 'pre-command-hook))
    (add-hook 'pre-command-hook  'icicle-top-level-prep) ; This must not be LOCAL (nil LOCAL arg).
    (add-hook 'pre-command-hook  'icicle-run-icicle-pre-command-hook nil t)
    (unless (fboundp 'define-minor-mode) (make-local-hook 'post-command-hook))
    (add-hook 'post-command-hook 'icicle-run-icicle-post-command-hook nil t)
    ;; Change the region background here dynamically.  For Emacs 23+ we can do it for just the minibuffer.
    (when (= 1 (recursion-depth))
      (setq icicle-saved-region-background  (face-background 'region))
      (when (and icicle-change-region-background-flag  (not (fboundp 'face-remap-add-relative))) ; Emacs < 23.
        (set-face-background 'region icicle-region-background)))
    (when (and icicle-change-region-background-flag  (fboundp 'face-remap-add-relative))
      (face-remap-add-relative 'region :background icicle-region-background)) ; Emacs 23+
    ;; Reset prompt, because some commands (e.g. `find-file') don't use `read-file-name'
    ;; or `completing-read'.  Reset other stuff too.
    (setq icicle-candidate-nb                    nil
          icicle-completion-candidates           nil
          ;; This is so that cycling works right initially, without first hitting `TAB' or `S-TAB'.
          icicle-current-completion-mode         (and (< (minibuffer-depth) 2)
                                                      (case icicle-default-cycling-mode
                                                        ((nil)      nil)
                                                        (apropos    'apropos)
                                                        (prefix     'prefix)
                                                        (otherwise  nil)))
          icicle-mode-line-help                  nil
          icicle-next-apropos-complete-cycles-p  nil
          icicle-next-prefix-complete-cycles-p   nil
          icicle-incremental-completion-p        icicle-incremental-completion
          icicle-initial-value                   nil
          icicle-cmd-reading-input               this-command
          icicle-last-completion-command         nil
          icicle-last-completion-candidate       nil
          icicle-last-input                      nil
          icicle-input-fail-pos                  nil
          icicle-saved-proxy-candidates          nil
          icicle-auto-no-icomplete-mode-p        nil
          icicle-auto-no-sort-p                  nil
          ;; Neither of these is OK:
          ;;  `other-buffer' doesn't work, because it looks for a buffer only from the same frame.
          ;;  And cadr of `buffer-list' could be just a higher-level minibuffer.
          ;;    icicle-pre-minibuffer-buffer  (other-buffer nil t)
          ;;    icicle-pre-minibuffer-buffer  (cadr (buffer-list)
          icicle-pre-minibuffer-buffer           (icicle-last-non-minibuffer-buffer)
          )
    (when (and (icicle-completing-p)    ; Function initializes variable `icicle-completing-p'.
               (> emacs-major-version 20))
      (let ((prompt-prefix  (if icicle-candidate-action-fn "+ " ". ")))
        (put-text-property 0 1 'face
                           (cond ((and icicle-candidate-action-fn  (icicle-require-match-p))
                                  '(icicle-multi-command-completion icicle-mustmatch-completion))
                                 (icicle-candidate-action-fn 'icicle-multi-command-completion)
                                 ((icicle-require-match-p)
                                  '(icicle-completion icicle-mustmatch-completion))
                                 (t 'icicle-completion))
                           prompt-prefix)
        (if (overlayp icicle-completion-prompt-overlay)
            (move-overlay icicle-completion-prompt-overlay (point-min) (point-min))
          (setq icicle-completion-prompt-overlay  (make-overlay (point-min) (point-min))))
        (overlay-put icicle-completion-prompt-overlay 'before-string prompt-prefix)))
    (unless icicle-add-proxy-candidates-flag
      (setq icicle-saved-proxy-candidates  (prog1 icicle-proxy-candidates
                                             (setq icicle-proxy-candidates
                                                   icicle-saved-proxy-candidates))))
    (while icicle-saved-candidate-overlays
      (delete-overlay (car icicle-saved-candidate-overlays))
      (setq icicle-saved-candidate-overlays  (cdr icicle-saved-candidate-overlays)))
    (icicle-update-ignored-extensions-regexp)
    (when (memq icicle-default-value '(preselect-start preselect-end))
      (icicle-select-minibuffer-contents))
    (when (and icicle-show-Completions-initially-flag
               ;; $$$$$$$$ (not icicle-progressive-completing-p) ; If narrowed, we have already completed.
               icicle-completing-p      ; Var already initialized, above.
               (sit-for icicle-incremental-completion-delay)) ; Let user interrupt.
      (case icicle-default-cycling-mode
        (apropos    (icicle-apropos-complete))
        (otherwise  (icicle-prefix-complete)))) ; Prefix completion, by default.
    (run-hooks 'icicle-minibuffer-setup-hook)))

(defun icicle-last-non-minibuffer-buffer ()
  "Return the most recently used non-minibuffer buffer."
  (if (fboundp 'minibufferp)            ; Emacs 22+
      (let ((bufs  (icicle-remove-if 'minibufferp (buffer-list))))
        (or (car bufs)  (car (buffer-list))))
    (cadr (buffer-list))))              ; Punt - but could be just a higher-level minibuffer.

(defun icicle-define-cycling-keys (map)
  "Define keys for cycling candidates.
The modal keys are defined first, then the non-modal keys.
That means that in case of conflict mode-specific cyling wins.
For example, if you define both `icicle-modal-cycle-up-keys' and
`icicle-prefix-cycle-previous-keys' as ([up]), the latter gets the
binding."
  (cond (icicle-use-C-for-actions-flag  ; Use `C-' for actions, no `C-' for plain cycling.
         ;; Modal cycling keys.
         (dolist (key  icicle-modal-cycle-up-keys)
           (define-key map key 'icicle-previous-candidate-per-mode)) ; `up'
         (dolist (key  icicle-modal-cycle-down-keys)
           (define-key map key 'icicle-next-candidate-per-mode)) ; `down'
         (dolist (key  icicle-modal-cycle-up-action-keys)
           (define-key map key 'icicle-previous-candidate-per-mode-action)) ; `C-up'
         (dolist (key  icicle-modal-cycle-down-action-keys)
           (define-key map key 'icicle-next-candidate-per-mode-action)) ; `C-down'
         ;; Non-modal cycling keys.  In case of conflict, these will prevail over modal keys.
         (dolist (key  icicle-prefix-cycle-previous-keys)
           (define-key map key 'icicle-previous-prefix-candidate)) ; `home'
         (dolist (key  icicle-prefix-cycle-next-keys)
           (define-key map key 'icicle-next-prefix-candidate)) ; `end'
         (dolist (key  icicle-apropos-cycle-previous-keys)
           (define-key map key 'icicle-previous-apropos-candidate)) ; `prior'
         (dolist (key  icicle-apropos-cycle-next-keys)
           (define-key map key 'icicle-next-apropos-candidate)) ; `next'
         (dolist (key  icicle-prefix-cycle-previous-action-keys)
           (define-key map key 'icicle-previous-prefix-candidate-action)) ; `C-home'
         (dolist (key  icicle-prefix-cycle-next-action-keys)
           (define-key map key 'icicle-next-prefix-candidate-action)) ; `C-end'
         (dolist (key  icicle-apropos-cycle-previous-action-keys)
           (define-key map key 'icicle-previous-apropos-candidate-action)) ; `C-prior'
         (dolist (key  icicle-apropos-cycle-next-action-keys)
           (define-key map key 'icicle-next-apropos-candidate-action))) ; `C-next'

        (t                              ; Use `C-' for plain cycling, NO `C-' for action.
         ;; Modal cycling keys.  At least some of these will overwrite non-modal keys.
         (dolist (key  icicle-modal-cycle-up-keys)
           (define-key map key 'icicle-previous-candidate-per-mode-action)) ; `up'
         (dolist (key  icicle-modal-cycle-down-keys)
           (define-key map key 'icicle-next-candidate-per-mode-action)) ; `down'
         (dolist (key  icicle-modal-cycle-up-action-keys)
           (define-key map key 'icicle-previous-candidate-per-mode)) ; `C-up'
         (dolist (key  icicle-modal-cycle-down-action-keys)
           (define-key map key 'icicle-next-candidate-per-mode)) ; `C-down'
         ;; Non-modal cycling keys.  In case of conflict, these will prevail over modal keys.
         (dolist (key  icicle-prefix-cycle-previous-keys)
           (define-key map key 'icicle-previous-prefix-candidate-action)) ; `home'
         (dolist (key  icicle-prefix-cycle-next-keys)
           (define-key map key 'icicle-next-prefix-candidate-action)) ; `end'
         (dolist (key  icicle-apropos-cycle-previous-keys)
           (define-key map key 'icicle-previous-apropos-candidate-action)) ; `prior'
         (dolist (key  icicle-apropos-cycle-next-keys)
           (define-key map key 'icicle-next-apropos-candidate-action)) ; `next'
         (dolist (key  icicle-prefix-cycle-previous-action-keys)
           (define-key map key 'icicle-previous-prefix-candidate)) ; `C-home'
         (dolist (key  icicle-prefix-cycle-next-action-keys)
           (define-key map key 'icicle-next-prefix-candidate)) ; `C-end'
         (dolist (key  icicle-apropos-cycle-previous-action-keys)
           (define-key map key 'icicle-previous-apropos-candidate)) ; `C-prior'
         (dolist (key  icicle-apropos-cycle-next-action-keys)
           (define-key map key 'icicle-next-apropos-candidate))))

  ;; Help and alternative-action keys are NOT controlled by `icicle-use-C-for-actions-flag'.
  ;;
  ;; Define modal cycling help and alternative action keys.
  (dolist (key  icicle-modal-cycle-up-help-keys)
    (define-key map key 'icicle-previous-candidate-per-mode-help)) ; `C-M-up'
  (dolist (key  icicle-modal-cycle-down-help-keys)
    (define-key map key 'icicle-next-candidate-per-mode-help)) ; `C-M-down'
  (dolist (key  icicle-modal-cycle-up-alt-action-keys)
    (define-key map key 'icicle-previous-candidate-per-mode-alt-action)) ; `C-S-up'
  (dolist (key  icicle-modal-cycle-down-alt-action-keys)
    (define-key map key 'icicle-next-candidate-per-mode-alt-action)) ; `C-S-down'
  ;; Define non-modal cycling help and alternative action keys.
  (dolist (key  icicle-prefix-cycle-previous-help-keys)
    (define-key map key 'icicle-help-on-previous-prefix-candidate)) ; `C-M-home'
  (dolist (key  icicle-prefix-cycle-next-help-keys)
    (define-key map key 'icicle-help-on-next-prefix-candidate)) ; `C-M-end'
  (dolist (key  icicle-apropos-cycle-previous-help-keys)
    (define-key map key 'icicle-help-on-previous-apropos-candidate)) ; `C-M-prior'
  (dolist (key  icicle-apropos-cycle-next-help-keys)
    (define-key map key 'icicle-help-on-next-apropos-candidate)) ; `C-M-next'
  (dolist (key  icicle-prefix-cycle-previous-alt-action-keys)
    (define-key map key 'icicle-previous-prefix-candidate-alt-action)) ; `C-S-home'
  (dolist (key  icicle-prefix-cycle-next-alt-action-keys)
    (define-key map key 'icicle-next-prefix-candidate-alt-action)) ; `C-S-end'
  (dolist (key  icicle-apropos-cycle-previous-alt-action-keys)
    (define-key map key 'icicle-previous-apropos-candidate-alt-action)) ; `C-S-prior'
  (dolist (key  icicle-apropos-cycle-next-alt-action-keys)
    (define-key map key 'icicle-next-apropos-candidate-alt-action))) ; `C-S-next'

(defun icicle-select-minibuffer-contents ()
  "Select minibuffer contents and leave point at its beginning."
  (let ((min  (icicle-minibuffer-prompt-end)))
    (set-mark (if (eq 'preselect-start icicle-default-value) (point-max) min))
    (goto-char (if (eq 'preselect-start icicle-default-value) min (point-max)))))

;; $$$ (defadvice next-history-element (after icicle-select-minibuffer-contents activate)
;;   "Select minibuffer contents and leave point at its beginning."
;;   (when (and icicle-mode  (memq icicle-default-value '(preselect-start preselect-end)))
;;     (icicle-select-minibuffer-contents)
;;     (setq deactivate-mark  nil)))

(defun icicle-cancel-Help-redirection ()
  "Cancel redirection of focus from *Help* buffer to minibuffer.
Focus was redirected during `icicle-help-on-candidate'."
  (let* ((help-window  (get-buffer-window "*Help*" 0))
         (help-frame   (and help-window  (window-frame help-window))))
    (when help-frame (redirect-frame-focus help-frame))))

(defun icicle-run-icicle-pre-command-hook ()
  "Run `icicle-pre-command-hook' functions.
Used in `pre-command-hook'."
  (run-hooks 'icicle-pre-command-hook))

(defun icicle-run-icicle-post-command-hook ()
  "Run `icicle-post-command-hook' functions.
Used in `post-command-hook'."
  (run-hooks 'icicle-post-command-hook))

(defun icicle-set-calling-cmd ()
  "Remember last command that called for completion.
Used in `completion-setup-hook'."
  (setq icicle-cmd-calling-for-completion  this-command))

(defun icicle-update-ignored-extensions-regexp ()
  "Update ignored extensions if `completion-ignored-extensions' changed."
  (when (and (icicle-file-name-input-p) ; In `icicles-fn.el'.
             (not (equal icicle-ignored-extensions completion-ignored-extensions)))
    (setq icicle-ignored-extensions-regexp ; Make regexp for ignored file extensions.
          (concat "\\(" (mapconcat #'regexp-quote completion-ignored-extensions "\\|") "\\)\\'")

          ;; Flag to prevent updating `icicle-ignored-extensions-regexp' unless
          ;; `completion-ignored-extensions' changes.
          icicle-ignored-extensions  completion-ignored-extensions)))

;; We change the region background here dynamically.
;; It would be better to just use a buffer-local face, but those don't yet exist.
(defun icicle-restore-region-face ()
  "Restore region face.  It was changed during minibuffer activity
if `icicle-change-region-background-flag' is non-nil."
  (when (and icicle-change-region-background-flag  (= 1 (recursion-depth)))
    (if (fboundp 'face-remap-remove-relative) ; Emacs 23+
        (face-remap-remove-relative icicle-face-remapping-region)
      (set-face-background 'region icicle-saved-region-background))))

(defun icicle-activate-mark ()
  "Prevent region from being deactivated.  Used in `icicle-post-command-hook'."
  (when (and (window-minibuffer-p (selected-window))
             icicle-completing-p
             (not executing-kbd-macro))
    (setq deactivate-mark  nil)))

(defun icicle-show-current-help-in-mode-line ()
  "Show `icicle-mode-line-help'.  Used in `icicle-post-command-hook'."
  (when icicle-mode-line-help (icicle-show-help-in-mode-line icicle-mode-line-help)))

;; Note: Property `icicle-mode-line-help' with a function value is not used yet in Icicles code.
(defun icicle-show-help-in-mode-line (candidate)
  "If short help for CANDIDATE is available, show it in the mode-line.
Do this only if `icicle-help-in-mode-line-delay' is positive and the
last command was not one that exits the minibuffer.

For a string or symbol CANDIDATE: Use the help from property
`icicle-mode-line-help', if that is non-nil, or the help from
property `help-echo' if that is non-nil.  For a string CANDIDATE,
check only the first char for the property.

The value of property `icicle-mode-line-help' can be a string or a
function.  If a string, use that as the help.  If a function, apply
the function to the candidate and use the result as the help."
  (when (and (> icicle-help-in-mode-line-delay 0)
             (not (memq last-command '(exit-minibuffer minibuffer-complete-and-exit))))
    (let* ((cand       (cond (;; Call to `lacarte-execute(-menu)-command' (in `lacarte.el').
                              ;; Use command associated with menu item.
                              (consp lacarte-menu-items-alist)
                              (cdr (assoc candidate lacarte-menu-items-alist)))
                             (;; Key-completion candidate.  Get command from candidate.
                              icicle-completing-keys-p
                              (if (string= ".." candidate)
                                  "GO UP"
                                (let ((cmd-name  (save-match-data
                                                   (string-match (concat "\\(.+\\)"
                                                                         icicle-complete-keys-separator
                                                                         "\\(.+\\)")
                                                                 candidate)
                                                   (substring candidate (match-beginning 2)
                                                              (match-end 2)))))
                                  (if (string= "..." cmd-name) "Prefix key" (intern-soft cmd-name)))))
                             (;; Buffer or file name.
                              (or (get-buffer candidate)
                                  (icicle-file-name-input-p)
                                  icicle-abs-file-candidates)
                              (icicle-transform-multi-completion candidate))
                             (t         ; Convert to symbol or nil.
                              (intern-soft (icicle-transform-multi-completion candidate)))))
           (doc        (progn (when (stringp candidate)
                                (setq candidate  (icicle-transform-multi-completion candidate)))
                              (cond ((and (stringp candidate) ; String with help as property.
                                          (let ((prop  (or (get-text-property 0 'icicle-mode-line-help
                                                                              candidate)
                                                           (get-text-property 0 'help-echo candidate))))
                                            (and prop
                                                 (icicle-propertize
                                                  (if (functionp prop) (funcall prop candidate) prop)
                                                  'face 'icicle-mode-line-help)))))
                                    ((and cand
                                          (symbolp cand) ; Symbol.
                                          (let ((doc2
                                                 (cond ((get cand 'icicle-mode-line-help)) ; Icicles help prop.
                                                       ((get cand 'help-echo)) ; General help prop.
                                                       ((fboundp cand) ; Function.
                                                        (or (documentation cand t) ; Functon's doc string.
                                                            (if (string-match ; Easy-menu item.
                                                                 "^menu-function-[0-9]+$" (symbol-name cand))
                                                                (format "%s" (symbol-function cand))
                                                              (format "Command `%s'" cand))))
                                                       ((facep cand) (face-documentation cand)) ; Face.
                                                       (t (documentation-property ; Variable.
                                                           cand 'variable-documentation t)))))
                                            (and doc2
                                                 (icicle-propertize doc2  'face 'icicle-mode-line-help)))))
                                    ((and (consp cand)  (eq (car cand) 'lambda)) ; Lambda form.
                                     (icicle-propertize (format "%s" cand) 'face 'icicle-mode-line-help))
                                    ((and (stringp cand) ; Prefix key, `..'.
                                          (member cand '("Prefix key" "GO UP")))
                                     (icicle-propertize cand 'face 'icicle-mode-line-help))
                                    ((stringp candidate) ; String without help property.
                                     (cond ((and (or (icicle-file-name-input-p) ; File name.
                                                     icicle-abs-file-candidates)
                                                 (or (icicle-file-remote-p candidate) ; Avoid Tramp.
                                                     (file-exists-p candidate)))
                                            (if (get-file-buffer candidate)
                                                (concat (icicle-help-line-buffer (get-file-buffer candidate)
                                                                                 'NO-BYTES-P
                                                                                 'NO-FILE-P)
                                                        " "
                                                        (icicle-help-line-file cand))
                                              (icicle-help-line-file candidate)))
                                           ((get-buffer candidate) ; Non-file buffer.
                                            (icicle-help-line-buffer candidate))
                                           (t nil)))))) ; Punt.
           (doc-line1  (and (stringp doc)  (string-match ".+$" doc)  (match-string 0 doc))))
      (when doc-line1
        (icicle-show-in-mode-line
         doc-line1
         (cond ((get-buffer-window "*Completions*" 'visible) "*Completions*")
               ((eq (current-buffer) (window-buffer (minibuffer-window))) (cadr (buffer-list)))
               (t (current-buffer))))))))

(defun icicle-show-in-mode-line (text &optional buffer)
  "Display TEXT in BUFFER's mode line.
The text is shown for `icicle-help-in-mode-line-delay' seconds, or
until a user event.  So call this last in a sequence of user-visible
actions."
  (message nil)                         ; Remove any msg, such as "Computing completion candidates...".
  (with-current-buffer (or buffer  (current-buffer))
    (make-local-variable 'mode-line-format) ; Needed for Emacs 21+.
    (let ((mode-line-format  text))  (force-mode-line-update) (sit-for icicle-help-in-mode-line-delay))
    (force-mode-line-update)))

(defun icicle-help-line-buffer (buffer &optional no-bytes-p no-file-p)
  "Simple help string for BUFFER.
Non-nil NO-BYTES-P means do not include the number of bytes.
Non-nil NO-FILE-P means do not include the buffer's file name."
  (with-current-buffer buffer
    (let* ((mode   (format "Mode: %s"
                           (icicle-propertize
                            (if (fboundp 'format-mode-line) (format-mode-line mode-name) mode-name)
                            'face 'icicle-mode-line-help)))
           (bytes  (format "Bytes: %s"
                           (icicle-propertize (let ((size  (buffer-size)))
                                                (if (> size most-positive-fixnum)
                                                    (format "> %d" most-positive-fixnum)
                                                  size))
                                              'face 'icicle-mode-line-help)))
           (file   (or (buffer-file-name)
                       (and (eq major-mode 'dired-mode)  default-directory))))
      (cond ((and no-bytes-p  no-file-p)  mode)
            ((or no-file-p  (not file))   (concat mode ", " bytes))
            (t
             (setq file  (format "File: %s" (icicle-propertize (icicle-abbreviate-or-expand-file-name file)
                                                               'face 'icicle-mode-line-help)))
             (if no-bytes-p (concat mode ", " file) (concat mode ", " bytes ", " file)))))))

(defun icicle-help-line-file (file)
  "Simple help string for FILE."
  (let ((attrs  (file-attributes file)))
    (and attrs  (format "Bytes: %s, Saved: %s, Access: %s, Perm: %s"
                        (let ((size  (nth 7 attrs)))
                          (icicle-propertize (if (> size most-positive-fixnum)
                                                 (format "> %d" most-positive-fixnum)
                                               size)
                                             'face 'icicle-mode-line-help))
                        (icicle-propertize (format-time-string  "%c" (nth 5 attrs)) ; "%Y-%m-%d %H"
                                           'face 'icicle-mode-line-help)
                        (icicle-propertize (format-time-string  "%c" (nth 4 attrs)) ; "%Y-%m-%d %H"
                                           'face 'icicle-mode-line-help)
                        (icicle-propertize (nth 8 attrs) 'face 'icicle-mode-line-help)))))

(defun icicle-redefine-standard-functions ()
  "Alias the functions in `icicle-functions-to-redefine' to Icicles versions."
  (when (fboundp 'icicle-completing-read)
    (dolist (fn  icicle-functions-to-redefine)
      (when (fboundp (intern (concat "icicle-ORIG-" (symbol-name fn))))
        (fset fn (intern (concat "icicle-" (symbol-name fn))))))))

(defun icicle-restore-standard-functions ()
  "Restore original versions of functions in `icicle-functions-to-redefine'."
  (when (fboundp 'icicle-ORIG-completing-read)
    (let (orig-fn)
      (dolist (fn  icicle-functions-to-redefine)
        (when (fboundp (setq orig-fn  (intern (concat "icicle-ORIG-" (symbol-name fn)))))
          (fset fn orig-fn))))))

(defun icicle-redefine-standard-widgets ()
  "Alias the widgets in `icicle-widgets-to-redefine' to Icicles versions."
  (when (fboundp 'icicle-completing-read)
    (let (ici-wid)
      (dolist (wid  icicle-widgets-to-redefine)
        (when (icicle-widgetp (intern (concat "icicle-ORIG-" (symbol-name wid))))
          (setq ici-wid  (intern (concat "icicle-" (symbol-name wid))))
          (put wid 'widget-type (get ici-wid 'widget-type))
          (put wid 'widget-documentation (get ici-wid 'widget-documentation)))))))

(defun icicle-restore-standard-widgets ()
  "Restore original versions of widgets in `icicle-widgets-to-redefine'."
  (when (fboundp 'icicle-ORIG-completing-read)
    (let (orig-wid)
      (dolist (wid  icicle-widgets-to-redefine)
        (when (icicle-widgetp (setq orig-wid  (intern (concat "icicle-ORIG-" (symbol-name wid)))))
          (put wid 'widget-type (get orig-wid 'widget-type))
          (put wid 'widget-documentation (get orig-wid 'widget-documentation)))))))

;;; In Emacs versions before 22:
;;; Save original `read-file-name'.  We redefine it as `icicle-read-file-name' (which calls it).
;;; Then we restore it when you quit Icicle mode.  (In Emacs 22+, no redefinition is needed.)
(unless (or (boundp 'read-file-name-function)  (fboundp 'icicle-ORIG-read-file-name))
  (defalias 'icicle-ORIG-read-file-name (symbol-function 'read-file-name)))

(defun icicle-redefine-std-completion-fns ()
  "Replace some standard functions with versions for Icicle mode."
  (when (fboundp 'icicle-completing-read)
    (fset 'choose-completion            'icicle-choose-completion)
    (fset 'choose-completion-string     'icicle-choose-completion-string)
    (fset 'completing-read              'icicle-completing-read)
    (when (fboundp 'icicle-completing-read-multiple)
      (fset 'completing-read-multiple   'icicle-completing-read-multiple)
      (setq crm-local-completion-map  icicle-crm-local-completion-map
            crm-local-must-match-map  icicle-crm-local-must-match-map))
    (fset 'completion-setup-function    'icicle-completion-setup-function)
    (unless (> emacs-major-version 22)
      (fset 'dired-smart-shell-command  'icicle-dired-smart-shell-command))
    (fset 'display-completion-list      'icicle-display-completion-list)
    (fset 'exit-minibuffer              'icicle-exit-minibuffer)
    (when (fboundp 'face-valid-attribute-values)
      (fset 'face-valid-attribute-values 'icicle-face-valid-attribute-values))
    (fset 'minibuffer-complete-and-exit 'icicle-minibuffer-complete-and-exit)
    (unless (or (> emacs-major-version 23)  (and (= emacs-major-version 23)
                                                 (> emacs-minor-version 1)))
      (fset 'mouse-choose-completion    'icicle-mouse-choose-completion)) ; Emacs < 23.2
    (fset 'next-history-element         'icicle-next-history-element)
    (fset 'read-buffer                  'icicle-read-buffer)
    (fset 'read-face-name               'icicle-read-face-name)
    (if (boundp 'read-file-name-function) ; Emacs 22+
        (setq icicle-orig-read-file-name-fn  (prog1 (and (not (eq read-file-name-function
                                                                  'icicle-read-file-name))
                                                         read-file-name-function)
                                               (setq read-file-name-function
                                                     'icicle-read-file-name)))
      (fset 'read-file-name             'icicle-read-file-name)) ; Emacs 20, 21
    (when (fboundp 'icicle-read-number)
      (fset 'read-number                'icicle-read-number))
    (unless (> emacs-major-version 22)
      (fset 'shell-command              'icicle-shell-command))
    (unless (> emacs-major-version 22)
      (fset 'shell-command-on-region    'icicle-shell-command-on-region))
    (when (> emacs-major-version 22)
      (fset 'sit-for                    'icicle-sit-for))
    (fset 'switch-to-completions        'icicle-switch-to-completions)
    ))

(defun icicle-restore-std-completion-fns ()
  "Restore some standard functions that were replaced in Icicle mode."
  (when (fboundp 'icicle-ORIG-completing-read)
    (fset 'choose-completion            'icicle-ORIG-choose-completion)
    (fset 'choose-completion-string     'icicle-ORIG-choose-completion-string)
    (fset 'completing-read              'icicle-ORIG-completing-read)
    (when (fboundp 'icicle-ORIG-completing-read-multiple)
      (fset 'completing-read-multiple   'icicle-ORIG-completing-read-multiple)
      (setq crm-local-completion-map  icicle-ORIG-crm-local-completion-map
            crm-local-must-match-map  icicle-ORIG-crm-local-must-match-map))
    (fset 'completion-setup-function    'icicle-ORIG-completion-setup-function)
    (when (fboundp 'icicle-ORIG-dired-smart-shell-command) ; Emacs 23
      (fset 'dired-smart-shell-command  'icicle-ORIG-dired-smart-shell-command))
    (fset 'display-completion-list      'icicle-ORIG-display-completion-list)
    (fset 'exit-minibuffer              'icicle-ORIG-exit-minibuffer)
    (when (fboundp 'icicle-ORIG-face-valid-attribute-values)
      (fset 'face-valid-attribute-values 'icicle-ORIG-face-valid-attribute-values))
    (fset 'minibuffer-complete-and-exit 'icicle-ORIG-minibuffer-complete-and-exit)
    (unless (or (> emacs-major-version 23)  (and (= emacs-major-version 23)
                                                 (> emacs-minor-version 1)))
      (fset 'mouse-choose-completion    'icicle-ORIG-mouse-choose-completion)) ; Emacs < 23.2
    (fset 'next-history-element         'icicle-ORIG-next-history-element)
    (fset 'read-buffer                  'icicle-ORIG-read-buffer)
    (fset 'read-face-name               'icicle-ORIG-read-face-name)
    (if (boundp 'read-file-name-function) ; Emacs 22+
        (setq read-file-name-function  (and (not (eq icicle-orig-read-file-name-fn
                                                     'icicle-read-file-name))
                                            icicle-orig-read-file-name-fn))
      (fset 'read-file-name             'icicle-ORIG-read-file-name)) ; Emacs 20, 21
    (when (fboundp 'icicle-ORIG-read-number)
      (fset 'read-number                'icicle-ORIG-read-number))
    (when (fboundp 'icicle-ORIG-shell-command) ; Emacs < 23
      (fset 'shell-command              'icicle-ORIG-shell-command))
    (when (fboundp 'icicle-ORIG-shell-command-on-region) ; Emacs < 23
      (fset 'shell-command-on-region    'icicle-ORIG-shell-command-on-region))
    (when (> emacs-major-version 22)
      (fset 'sit-for                    'icicle-ORIG-sit-for))
    (fset 'switch-to-completions        'icicle-ORIG-switch-to-completions)
    ))

;; Free vars here: `icicle-saved-kmacro-ring-max' is bound in `icicles-var.el'.
(defun icicle-redefine-standard-options ()
  "Replace certain standard Emacs options with Icicles versions."
  (when (boundp 'icicle-search-ring-max)
    (setq icicle-saved-search-ring-max         search-ring-max ; Save it.
          search-ring-max                      icicle-search-ring-max
          icicle-saved-regexp-search-ring-max  regexp-search-ring-max ; Save it.
          regexp-search-ring-max               icicle-regexp-search-ring-max))
  (when (boundp 'icicle-kmacro-ring-max) ; Emacs 22+
    (setq icicle-saved-kmacro-ring-max  kmacro-ring-max ; Save it.
          kmacro-ring-max               icicle-kmacro-ring-max)))

(defun icicle-restore-standard-options ()
  "Restore standard Emacs options replaced in Icicle mode."
  (when (boundp 'icicle-saved-search-ring-max)
    (setq search-ring-max         icicle-saved-search-ring-max
          regexp-search-ring-max  icicle-saved-regexp-search-ring-max)))

;; This is used only in Emacs 22+, but we define it always anyway.
(defun icicle-undo-std-completion-faces ()
  "Get rid of standard completion-root highlighting in `*Completions*'."
  ;; Do this because the standard Emacs 22 highlighting can interfere with
  ;; apropos-completion highlighting.
  (when (fboundp 'face-spec-reset-face)
    (when (facep 'completions-common-part)
      (face-spec-reset-face 'completions-common-part)
      (set-face-attribute 'completions-common-part nil :inherit nil))
    (when (facep 'completions-first-difference)
      (face-spec-reset-face 'completions-first-difference)
      (set-face-attribute 'completions-first-difference nil :inherit nil))))


;;; Save original functions, so they can be restored when leave Icicle mode.
;;; Toggle Icicle mode after loading the library (and `icicles-mode.el'),
;;; to pick up the original definition.
;;;
;;; Note: The `boundp' test for `icicle-mode' is just in case the form gets evaluated while
;;; loading `icicles-mode.el' (e.g. the library gets loaded while loading `icicles-mode.el').

;;; `comint.el' - `comint-dynamic-complete', `comint-dynamic-complete-filename',
;;;               `comint-replace-by-expanded-filename', `comint-completion-at-point'.
(let ((form  '(let ((icyp  (and (boundp 'icicle-mode)  icicle-mode)))
               (when icyp (icicle-mode -1))
               (when (and (fboundp 'comint-dynamic-complete)
                          (not (fboundp 'icicle-ORIG-comint-dynamic-complete)))
                 (fset 'icicle-ORIG-comint-dynamic-complete
                       (symbol-function 'comint-dynamic-complete)))
               (when (and (fboundp 'comint-dynamic-complete-filename)
                          (not (fboundp 'icicle-ORIG-comint-dynamic-complete-filename)))
                 (fset 'icicle-ORIG-comint-dynamic-complete-filename
                       (symbol-function 'comint-dynamic-complete-filename)))
               (when (and (fboundp 'comint-replace-by-expanded-filename)
                          (not (fboundp 'icicle-ORIG-comint-replace-by-expanded-filename)))
                 (fset 'icicle-ORIG-comint-replace-by-expanded-filename
                       (symbol-function 'comint-replace-by-expanded-filename)))
	       (when (and (fboundp 'comint-completion-at-point) ; Emacs 24+
                          (not (fboundp 'icicle-ORIG-comint-completion-at-point)))
                 (fset 'icicle-ORIG-comint-completion-at-point
                       (symbol-function 'comint-completion-at-point)))
               (when icyp (icicle-mode 1)))))
  (if (featurep 'comint) (eval-after-load "icicles-mode" form) (eval-after-load "comint" form)))

;;; `ess-site.el' - `ess-complete-object-name'.
(let ((form  '(let ((icyp  (and (boundp 'icicle-mode)  icicle-mode)))
               (when icyp (icicle-mode -1))
               (when (and (fboundp 'ess-complete-object-name)
                          (not (fboundp 'icicle-ORIG-ess-complete-object-name)))
                 (fset 'icicle-ORIG-ess-complete-object-name
                       (symbol-function 'ess-complete-object-name)))
               (when icyp (icicle-mode 1)))))
  (if (featurep 'ess-site) (eval-after-load "icicles-mode" form) (eval-after-load "ess-site" form)))

;;; `gud.el' - `gud-gdb-complete-command'.
(let ((form  '(let ((icyp  (and (boundp 'icicle-mode)  icicle-mode)))
               (when icyp (icicle-mode -1))
               (when (and (fboundp 'gud-gdb-complete-command)
                          (not (fboundp 'icicle-ORIG-gud-gdb-complete-command)))
                 (fset 'icicle-ORIG-gud-gdb-complete-command
                       (symbol-function 'gud-gdb-complete-command)))
               (when icyp (icicle-mode 1)))))
  (if (featurep 'gud) (eval-after-load "icicles-mode" form) (eval-after-load "gud" form)))

;;; `info.el' - `Info-goto-node', `Info-index', `Info-menu'.
(let ((form  '(let ((icyp  (and (boundp 'icicle-mode)  icicle-mode)))
               (when icyp (icicle-mode -1))
               (when (and (featurep 'info)  (not (fboundp 'icicle-ORIG-Info-goto-node)))
                 (fset 'icicle-ORIG-Info-goto-node (symbol-function 'Info-goto-node))
                 (fset 'icicle-ORIG-Info-index     (symbol-function 'Info-index))
                 (fset 'icicle-ORIG-Info-menu      (symbol-function 'Info-menu)))
               (when icyp (icicle-mode 1)))))
  (if (featurep 'info) (eval-after-load "icicles-mode" form) (eval-after-load "info" form)))

;;; `bbdb-com.el' version 2.35 - `bbdb-complete-name'.
(let ((form  '(let ((icyp  (and (boundp 'icicle-mode)  icicle-mode)))
               (when icyp (icicle-mode -1))
               (when (and (fboundp 'bbdb-complete-name)
                          (not (fboundp 'bbdb-complete-mail))
                          (not (fboundp 'icicle-ORIG-bbdb-complete-name)))
                 (fset 'icicle-ORIG-bbdb-complete-name (symbol-function 'bbdb-complete-name)))
               (when icyp (icicle-mode 1)))))
  (if (and (featurep 'bbdb-com)  (fboundp 'bbdb-complete-name)  (not (fboundp 'bbdb-complete-mail)))
      (eval-after-load "icicles-mode" form)
    (eval-after-load "bbdb-com" form)))

;;; `bbdb-com.el' version 3.02 and later - `bbdb-complete-mail'.
(let ((form  '(let ((icyp  (and (boundp 'icicle-mode)  icicle-mode)))
               (when icyp (icicle-mode -1))
               (when (and (fboundp 'bbdb-complete-mail)  (not (fboundp 'icicle-ORIG-bbdb-complete-mail)))
                 (fset 'icicle-ORIG-bbdb-complete-mail (symbol-function 'bbdb-complete-mail)))
               (when icyp (icicle-mode 1)))))
  (if (and (featurep 'bbdb-com)  (fboundp 'bbdb-complete-mail))
      (eval-after-load "icicles-mode" form)
    (eval-after-load "bbdb-com" form)))

;;; `complete.el' - `complete'.
(let ((form  '(let ((icyp  (and (boundp 'icicle-mode)  icicle-mode)))
               (when icyp (icicle-mode -1))
               (when (and (fboundp 'complete)  (not (fboundp 'icicle-ORIG-complete)))
                 (fset 'icicle-ORIG-complete (symbol-function 'complete)))
               (when icyp (icicle-mode 1)))))
  (if (featurep 'completion) (eval-after-load "icicles-mode" form) (eval-after-load "completion" form)))

;;; `dired-aux.el' - `dired-read-shell-command'.
(let ((form  '(let ((icyp  (and (boundp 'icicle-mode)  icicle-mode)))
               (when icyp (icicle-mode -1))
               (when (and (fboundp 'dired-read-shell-command)
                          (not (fboundp 'icicle-ORIG-dired-read-shell-command)))
                 (fset 'icicle-ORIG-dired-read-shell-command
                       (symbol-function 'dired-read-shell-command)))
               (when icyp (icicle-mode 1)))))
  (if (featurep 'dired-aux)
      (eval-after-load "icicles-mode" form)
    (eval-after-load "dired-aux" form)))

;;; `dired-x.el' - `dired-read-shell-command', `dired-smart-shell-command'.
(let ((form  '(let ((icyp  (and (boundp 'icicle-mode)  icicle-mode)))
               (when icyp (icicle-mode -1))
               (when (and (fboundp 'dired-read-shell-command)
                          (not (fboundp 'icicle-ORIG-dired-read-shell-command)))
                 (fset 'icicle-ORIG-dired-read-shell-command
                       (symbol-function 'dired-read-shell-command)))
               (unless (fboundp 'read-shell-command) ; `dired-smart-shell-command' in Emacs < 23.
                 (when (and (fboundp 'dired-smart-shell-command)
                            (not (fboundp 'icicle-ORIG-dired-smart-shell-command)))
                   (fset 'icicle-ORIG-dired-smart-shell-command
                         (symbol-function 'dired-smart-shell-command))))
               (when icyp (icicle-mode 1)))))
  (if (featurep 'dired-x) (eval-after-load "icicles-mode" form) (eval-after-load "dired-x" form)))

;;; `simple.el' - `read-shell-command' - Emacs 23+.
(when (> emacs-major-version 22)
  ;; `simple.el' is preloaded for Emacs 23+, so just do it now.
  (let ((icyp  (and (boundp 'icicle-mode)  icicle-mode)))
    (when icyp (icicle-mode -1))
    (when (and (fboundp 'read-shell-command)  (not (fboundp 'icicle-ORIG-read-shell-command)))
      (defalias 'icicle-ORIG-read-shell-command (symbol-function 'read-shell-command)))
    (when icyp (icicle-mode 1))))

;;; `recentf.el' - `recentf-make-menu-items'.
(let ((form  '(let ((icyp  (and (boundp 'icicle-mode)  icicle-mode)))
               (when icyp (icicle-mode -1))
               (when (and (fboundp 'recentf-make-menu-items)
                          (not (fboundp 'icicle-ORIG-recentf-make-menu-items)))
                 (fset 'icicle-ORIG-recentf-make-menu-items
                       (symbol-function 'recentf-make-menu-items)))
               (when icyp (icicle-mode 1)))))
  (if (featurep 'recentf) (eval-after-load "icicles-mode" form) (eval-after-load "recentf" form)))

;;; `icomplete.el'.  Reset `icicle-last-icomplete-mode-value', so it gets reinitialized properly.
(eval-after-load "icomplete"
  (defadvice icomplete-mode (after icicle-reset-last-icomplete-mode activate)
    (when (interactive-p) (setq icicle-last-icomplete-mode-value  nil))))

;; Do this last.
;;
;; When these libraries are first loaded, toggle Icicle mode to pick up the definitions
(dolist (library  '("bookmark+" "buff-menu" "comint" "dired" "ess-site" "gud" "ibuffer"
                    "idlw-shell"         ; (untested - I don't have an `idl' program)
                    "ielm" "info" "net-utils" "rlogin" "shell" "sh-script" "tcl"))
  (unless (if (fboundp 'load-history-regexp) ; Emacs 22+
              (load-history-filename-element (load-history-regexp library))
            (assoc library load-history))
    (eval-after-load library '(icicle-toggle-icicle-mode-twice))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-mode.el ends here

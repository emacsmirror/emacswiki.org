;;; icicles-mode.el --- Icicle Mode definition for Icicles
;;
;; Filename: icicles-mode.el
;; Description: Icicle Mode definition for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2012, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 10:21:10 2006
;; Version: 22.0
;; Last-Updated: Thu Dec  6 10:41:44 2012 (-0800)
;;           By: dradams
;;     Update #: 9225
;; URL: http://www.emacswiki.org/icicles-mode.el
;; Doc URL: http://www.emacswiki.org/Icicles
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x
;;
;; Features that might be required by this library:
;;
;;   `advice', `advice-preload', `apropos', `apropos+',
;;   `apropos-fn+var', `avoid', `bookmark', `bookmark+',
;;   `bookmark+-1', `bookmark+-bmu', `bookmark+-key',
;;   `bookmark+-lit', `cl', `cus-edit', `cus-face', `cus-load',
;;   `cus-start', `dired', `dired+', `dired-aux', `dired-x',
;;   `doremi', `easymenu', `el-swank-fuzzy', `ffap', `ffap-',
;;   `fit-frame', `frame-cmds', `frame-fns', `fuzzy', `fuzzy-match',
;;   `help+20', `hexrgb', `icicles-cmd1', `icicles-cmd2',
;;   `icicles-face', `icicles-fn', `icicles-mcmd', `icicles-opt',
;;   `icicles-var', `image-dired', `info', `info+', `kmacro',
;;   `levenshtein', `menu-bar', `menu-bar+', `misc-cmds', `misc-fns',
;;   `mouse3', `mwheel', `naked', `pp', `pp+', `regexp-opt', `ring',
;;   `ring+', `second-sel', `strings', `thingatpt', `thingatpt+',
;;   `unaccent', `w32-browser', `w32browser-dlgopen', `wid-edit',
;;   `wid-edit+', `widget'.
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
;;    `icicle-ORIG-dired-read-shell-command',
;;    `icicle-ORIG-ess-complete-object-name',
;;    `icicle-ORIG-gud-gdb-complete-command',
;;    `icicle-ORIG-read-file-name', `icicle-ORIG-read-shell-command',
;;    `icicle-skip-this-command'.
;;
;;  Non-interactive functions defined here:
;;
;;    `icicle-activate-mark', `icicle-add-menu-item-to-cmd-history',
;;    `icicle-bind-completion-keys',
;;    `icicle-bind-custom-completion-keys',
;;    `icicle-bind-isearch-keys',
;;    `icicle-bind-key-completion-keys-for-map-var',
;;    `icicle-bind-key-completion-keys-in-keymaps-from',
;;    `icicle-bind-other-keymap-keys',
;;    `icicle-cancel-Help-redirection', `icicle-define-cycling-keys',
;;    `icicle-define-icicle-maps', `icicle-define-minibuffer-maps',
;;    `icicle-minibuffer-setup', `icicle-rebind-global',
;;    `icicle-redefine-standard-functions',
;;    `icicle-redefine-standard-options',
;;    `icicle-redefine-std-completion-fns',
;;    `icicle-restore-completion-keys',
;;    `icicle-restore-custom-completion-keys',
;;    `icicle-restore-other-keymap-keys',
;;    `icicle-restore-region-face',
;;    `icicle-restore-standard-functions',
;;    `icicle-restore-standard-options',
;;    `icicle-restore-std-completion-fns',
;;    `icicle-run-icicle-post-command-hook',
;;    `icicle-run-icicle-pre-command-hook',
;;    `icicle-select-minibuffer-contents', `icicle-set-calling-cmd',
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
;;    `icicle-frames-menu-map', `icicle-info-menu-map',
;;    `icicle-mode-map', `icicle-options-menu-map',
;;    `icicle-search-menu-map', `icicle-search-tags-menu-map'.
;;
;;  For descriptions of changes to this file, see `icicles-chg.el'.
 
;;(@> "Index")
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  http://dto.freeshell.org/notebook/Linkd.html.
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

;;;###autoload (autoload 'icicle-mode "icicles" "Toggle Icicle mode." t nil)
;;;###autoload (autoload 'icy-mode    "icicles" "Toggle Icicle mode." t nil)

(eval-when-compile (require 'cl)) ;; pushnew, case
                                  ;; plus, for Emacs < 21: push, dolist

(require 'advice)
  ;; ad-activate, ad-copy-advice-info, ad-deactivate, ad-disable-advice, ad-enable-advice,
  ;; ad-find-some-advice, ad-get-arg, ad-is-active, ad-set-advice-info

(require 'icicles-opt)                  ; (This is required anyway by `icicles-var.el'.)
  ;; icicle-buffer-configs, icicle-buffer-extras, icicle-change-region-background-flag,
  ;; icicle-default-cycling-mode, icicle-incremental-completion, icicle-default-value, icicle-kbd,
  ;; icicle-kmacro-ring-max, icicle-minibuffer-setup-hook, icicle-modal-cycle-down-keys,
  ;; icicle-modal-cycle-up-keys, icicle-functions-to-redefine, icicle-regexp-search-ring-max,
  ;; icicle-region-background, icicle-search-ring-max, icicle-show-Completions-initially-flag,
  ;; icicle-top-level-key-bindings, icicle-touche-pas-aux-menus-flag,
  ;; icicle-word-completion-keys, icicle-yank-function
(require 'icicles-fn)                   ; (This is required anyway by `icicles-cmd1.el'.)
  ;; assq-delete-all, icicle-completing-p, icicle-isearch-complete-past-string,
  ;; icicle-toggle-icicle-mode-twice, icicle-unhighlight-lighter
(require 'icicles-var)                  ; (This is required anyway by `icicles-fn.el'.)
  ;; icicle-candidate-action-fn, icicle-candidate-nb, icicle-cmd-calling-for-completion,
  ;; icicle-completing-p, icicle-completion-candidates,
  ;; icicle-current-completion-mode, icicle-ignored-extensions, icicle-ignored-extensions-regexp,
  ;; icicle-incremental-completion-p, icicle-initial-value, icicle-last-completion-candidate,
  ;; icicle-last-completion-command, icicle-last-input, icicle-menu-map, icicle-pre-minibuffer-buffer,
  ;; icicle-minor-mode-map-entry, icicle-saved-completion-candidates, icicle-saved-kmacro-ring-max,
  ;; icicle-saved-regexp-search-ring-max, icicle-saved-region-background,
  ;; icicle-saved-search-ring-max, icicle-search-current-overlay, icicle-search-overlays,
  ;; icicle-search-refined-overlays
(require 'icicles-cmd1)                 ; (This is required anyway by `icicles-cmd2.el'.)
  ;; icicle-add-buffer-candidate, icicle-add-buffer-config, icicle-bbdb-complete-mail,
  ;; icicle-bbdb-complete-name, icicle-customize-face, icicle-customize-face-other-window,
  ;; icicle-dabbrev-completion, icicle-select-bookmarked-region
(require 'icicles-cmd2)
  ;; icicle-imenu, icicle-occur, icicle-search, icicle-search-bookmark,
  ;; icicle-search-bookmarks-together, icicle-search-buffer, icicle-search-file,
  ;; icicle-search-w-isearch-string

;; Use `condition-case' because if `mb-depth.el' can't be found, `mb-depth+.el' is not provided.
(when (>= emacs-major-version 22) (condition-case nil (require 'mb-depth+ nil t) (error nil)))
  ;; (no error if not found): minibuffer-depth-indicate-mode

(require 'dired+ nil t) ;; (no error if not found):
                        ;; diredp-menu-bar-operate-menu, diredp-menu-bar-recursive-marked-menu,
                        ;; diredp-menu-bar-subdir-menu
(require 'dired) ;; dired-mode-map
(require 'menu-bar+ nil t) ;; (no error if not found):
  ;; menu-bar-apropos-menu, menu-bar-describe-menu, menu-bar-edit-menu,
  ;; menu-bar-file-menu, menu-bar-frames-menu, menu-bar-options-menu, menu-bar-search-tags-menu

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

;;;###autoload
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

;; Main command.  Inspired from `icomplete-mode'.
;;;###autoload (autoload 'icy-mode "icicles")
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
                                       savehist-minibuffer-history-variables))))))
  (when (> emacs-major-version 21)
    (defadvice describe-face (before icicle-respect-WYSIWYG activate)
      "`read-face-name' respects `icicle-WYSIWYG-Completions-flag'.
If non-nil, then it does not use `completing-read-multiple' (which
cannot take advantage of WYSIWYG)."
      (interactive (list (read-face-name "Describe face" "= `default' face"
                                         (not icicle-WYSIWYG-Completions-flag))))))

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

Icicle mode defines the following top-level commands.  In many cases
there are also `-other-window' versions.

`clear-option' (alias)                 - Set binary option(s) to nil
`icicle-add-buffer-candidate'          - Add always-candidate buffer
`icicle-add-buffer-config'             - To `icicle-buffer-configs'
`icicle-add-entry-to-saved-completion-set' - Add completion to a set
`icicle-add-file-to-fileset'           - Add a file to a fileset
`icicle-add/update-saved-completion-set' - To
                                        `icicle-saved-completion-sets'
`icicle-apply'                         - Apply function to alist items
`icicle-apropos'                       - `apropos', but shows matches
`icicle-apropos-command'               - Enhanced `apropos-command'
`icicle-apropos-options-of-type'       - Show options of a given type
`icicle-apropos-variable'              - Enhanced `apropos-variable'
`icicle-apropos-vars-w-val-satisfying' - Show vars of given values
`icicle-bbdb-complete-mail'            - Complete a user name/address
`icicle-bbdb-complete-name'            - Complete a user name/address
`icicle-bookmark'                      - Jump to a bookmark
`icicle-bookmark-a-file'               - Create an autofile bookmark
`icicle-bookmark-all-tags'             - Jump to tagged bookmark
`icicle-bookmark-all-tags-regexp'      - Jump to tagged bookmark
`icicle-bookmark-autofile'             - Jump to an autofile bookmark
`icicle-bookmark-autofile-all-tags'    - Jump to a tagged autofile
`icicle-bookmark-autofile-all-tags-regexp'
`icicle-bookmark-autofile-some-tags'
`icicle-bookmark-autofile-some-tags-regexp'
`icicle-bookmark-autonamed'            - Jump to an autonamed bookmark
`icicle-bookmark-autonamed-this-buffer'
`icicle-bookmark-bookmark-file'        - Load a bookmark file bookmark
`icicle-bookmark-bookmark-list'        - Jump to a bookmark list
`icicle-bookmark-cmd'                  - Set or jump to a bookmark
`icicle-bookmark-desktop'              - Jump to a desktop bookmark
`icicle-bookmark-dired'                - Jump to a Dired bookmark
`icicle-bookmarked-buffer-list'        - Choose bookmarked buffers
`icicle-bookmarked-file-list'          - Choose bookmarked files
`icicle-bookmark-file'                 - Jump to a file bookmark
`icicle-bookmark-file-all-tags'        - Jump to tagged file bookmark
`icicle-bookmark-file-all-tags-regexp'
`icicle-bookmark-file-some-tags'
`icicle-bookmark-file-some-tags-regexp'
`icicle-bookmark-file-this-dir'        - Jump to file in directory
`icicle-bookmark-file-this-dir-all-tags'
`icicle-bookmark-file-this-dir-all-tags-regexp'
`icicle-bookmark-file-this-dir-some-tags'
`icicle-bookmark-file-this-dir-some-tags-regexp'
`icicle-bookmark-gnus'                 - Jump to a Gnus bookmark
`icicle-bookmark-image'                - Jump to an image bookmark
`icicle-bookmark-info'                 - Jump to an Info bookmark
`icicle-bookmark-jump'                 - Jump to any bookmark
`icicle-bookmark-list'                 - Choose a list of bookmarks
`icicle-bookmark-local-file'           - Jump to local-file bookmark
`icicle-bookmark-man'                  - Jump to a `man'-page bookmark
`icicle-bookmark-non-file'             - Jump to a buffer bookmark
`icicle-bookmark-region'               - Jump to a region bookmark
`icicle-bookmark-remote-file'          - Jump to a remote file
`icicle-bookmark-save-marked-files'    - Save file names as candidates
`icicle-bookmark-save-marked-files-persistently'
`icicle-bookmark-save-marked-files-to-variable'
`icicle-bookmark-set'                  - Set a bookmark
`icicle-bookmark-some-tags'            - Jump to tagged bookmark
`icicle-bookmark-some-tags-regexp'     - Jump to tagged bookmark
`icicle-bookmark-specific-buffers'     - Jump to a bookmarked buffer
`icicle-bookmark-specific-files'       - Jump to a bookmarked file
`icicle-bookmark-temporary'            - Jump to a temporary bookmark
`icicle-bookmark-this-buffer'          - Jump to bookmark for this buf
`icicle-bookmark-url'                  - Jump to a URL bookmark
`icicle-bookmark-w3m'                  - Jump to a W3M (URL) bookmark
`icicle-buffer'                        - Switch to buffer
`icicle-buffer-config'                 - Pick `icicle-buffer' options
`icicle-buffer-list'                   - Choose a list of buffer names
`icicle-change-alternative-sort-order' - Choose an alternative sort
`icicle-change-sort-order'             - Choose a sort order
`icicle-choose-faces'                  - Choose a list of face names
`icicle-choose-invisible-faces'        - Choose faces now invisible
`icicle-choose-visible-faces'          - Choose faces now visible
`icicle-clear-history'                 - Clear entries from a history
`icicle-color-theme'                   - Change color theme
`icicle-comint-command'                - Reuse shell etc. command
`icicle-comint-dynamic-complete'       - Text completion in shell
`icicle-comint-dynamic-complete-filename'
`icicle-comint-replace-by-expanded-filename'
`icicle-comint-search'                 - Reuse shell etc. command
`icicle-command-abbrev'                - Multi-command `M-x' + abbrevs
`icicle-compilation-search'            - `icicle-search' and show hits
`icicle-complete-keys'                 - Complete keys
`icicle-complete-thesaurus-entry'      - Complete word using thesaurus
`icicle-completing-yank'               - `yank' using completion
`icicle-customize-apropos'             - Enhanced `customize-apropos'
`icicle-customize-apropos-faces',
`icicle-customize-apropos-groups'
`icicle-customize-apropos-options'
`icicle-customize-apropos-options-of-type'
`icicle-customize-apropos-opts-w-val-satisfying'
`icicle-customize-face'                - Multi-`customize-face'
`icicle-customize-icicles-group'       - Customize options and faces
`icicle-cycle-expand-to-common-match'  - Cycle input ECM expansion
`icicle-cycle-image-file-thumbnail'    - Toggle images in Completions
`icicle-cycle-incremental-completion'  - Cycle incremental completion
`icicle-dabbrev-completion'            - Enhanced `dabbrev-completion'
`icicle-delete-file'                   - Delete file/directory
`icicle-delete-window'                 - Delete window (`C-u': buffer)
`icicle-delete-windows'                - Delete all windows for buffer
`icicle-describe-file'                 - Describe a file
`icicle-describe-option-of-type'       - Describe option of given type
`icicle-describe-process'              - Describe a computer process
`icicle-describe-var-w-val-satisfying' - Describe var satisfying pred
`icicle-directory-list'                - Choose a list of directories
`icicle-dired'                         - Multi-command Dired
`icicle-dired-chosen-files'            - Dired a set of files & dirs
`icicle-dired-project'                 - Dired a saved project
`icicle-dired-saved-file-candidates'   - Dired set of saved file names
`icicle-dired-save-marked'             - Save marked file names
`icicle-dired-save-marked-more'
`icicle-dired-save-marked-more-recursive'
`icicle-dired-save-marked-recursive'
`icicle-dired-save-marked-persistently'  ... to cache file or fileset
`icicle-dired-save-marked-to-cache-file-recursive' ... to cache-file
`icicle-dired-save-marked-to-variable'   ... to variable
`icicle-dired-save-marked-to-variable-recursive'
`icicle-dired-smart-shell-command'     - Enhanced version of vanilla
`icicle-doc'                           - Show doc for fn, var, or face
`icicle-doremi-candidate-width-factor+' - +/- candidate column width
`icicle-doremi-increment-max-candidates+' - +/- max number candidates
`icicle-doremi-increment-swank-prefix-length+' - +/- swank prefix
`icicle-doremi-increment-swank-timeout+' - +/- swank completion msec
`icicle-doremi-increment-variable+'    - Increment var using Do Re Mi
`icicle-doremi-inter-candidates-min-spaces+' - +/- candidate spacing
`icicle-doremi-zoom-Completions+'      - +/- `*Completions*' text size
`icicle-ess-complete-object-name'      - Complete an ESS object
`icicle-ess-R-complete-object-name'    - Complete an ESS object in R
`icicle-exchange-point-and-mark'       - Flip, save, or select region
`icicle-execute-extended-command'      - Multi-command `M-x'
`icicle-execute-named-keyboard-macro'  - Execute named keyboard macro
`icicle-face-list'                     - Choose a list of face names
`icicle-file-list'                     - Choose a list of file names
`icicle-file'                          - Visit file/directory
`icicle-find-file'                     -       same: relative only
`icicle-find-file-absolute'            -       same: absolute only
`icicle-find-file-read-only'           -       same: read-only
`icicle-find-file-all-tags'            - Visit Emacs-tagged file
`icicle-find-file-all-tags-regexp'
`icicle-find-file-some-tags'
`icicle-find-file-some-tags-regexp'
`icicle-find-file-handle-bookmark'     - Find file handling bookmark
`icicle-find-file-in-tags-table'       - File in Emacs tags table
`icicle-find-file-tagged'              - Visit tagged file
`icicle-find-first-tag'                - Visit definition with tag
`icicle-find-tag'                      - Visit definition with tag
`icicle-font'                          - Change font of frame
`icicle-frame-bg'                      - Change background of frame
`icicle-frame-fg'                      - Change foreground of frame
`icicle-fundoc'                        - Show function description
`icicle-goto-global-marker'            - Go to a global marker
`icicle-goto-global-marker-or-pop-global-mark'
`icicle-goto-marker'                   - Go to a marker in this buffer
`icicle-grep-saved-file-candidates'    - Grep saved file candidates
`icicle-gud-gdb-complete-command'      - Enhanced version of vanilla
`icicle-hide-faces'                    - Hide chosen visible faces
`icicle-hide-only-faces'               - Show all but chosen faces
`icicle-hide/show-comments'            - Hide or show comments
`icicle-ido-like-mode'                 - Ido-like mode for Icicles
`icicle-imenu*'                        - Navigate among Imenu entries
`icicle-imenu-full'                            same: full definitions
`icicle-imenu-command'                 - Navigate among command defs
`icicle-imenu-command-full'                    same: full definitions
`icicle-imenu-face'                    - Navigate among face defs
`icicle-imenu-face-full'                       same: full definitions
`icicle-imenu-key-explicit-map'        - Navigate among key defs
`icicle-imenu-key-explicit-map-full'           same: full definitions
`icicle-imenu-key-implicit-map'                same: no explicit map
`icicle-imenu-key-implicit-map-full'           same: full definitions
`icicle-imenu-macro'                   - Navigate among macro defs
`icicle-imenu-macro-full'                      same: full definitions
`icicle-imenu-non-interactive-function' - Navigate among function defs
`icicle-imenu-non-interactive-function-full'   same: full definitions
`icicle-imenu-user-option'             - Navigate among option defs
`icicle-imenu-user-option-full'                same: full definitions
`icicle-imenu-variable'                - Navigate among variable defs
`icicle-imenu-variable-full'                   same: full definitions
`icicle-increment-option'              - Increment numeric option
`icicle-increment-variable'            - Increment numeric variable
`icicle-Info-goto-node'                - Multi-cmd `Info-goto-node'
`icicle-Info-index'                    - Multi-command `Info-index'
`icicle-Info-menu'                     - Multi-command `Info-menu'
`icicle-Info-virtual-book'             - Open a virtual Info book
`icicle-insert-buffer'                 - Multi-command `insert-buffer'
`icicle-insert-thesaurus-entry'        - Insert thesaurus entry(s)
`icicle-keyword-list'                  - Choose a list of keywords
`icicle-kill-buffer'                   - Kill buffer
`icicle-kmacro'                        - Execute a keyboard macro
`icicle-lisp-complete-symbol'          - Enhanced version of vanilla
`icicle-locate'                        - Run `locate' then visit files
`icicle-locate-file'                   - Visit file(s) in a directory
`icicle-locate-file-no-symlinks'               same, but do not follow
`icicle-minibuffer-default-add-dired-shell-commands' - Enhanced
`icicle-minibuffer-help'               - Show Icicles minibuffer help
`icicle-mode' or `icy-mode'            - Toggle Icicle mode
`icicle-next-S-TAB-completion-method'  - Next S-TAB completion method
`icicle-next-TAB-completion-method'    - Next TAB completion method
`icicle-next-visible-thing'            - Go to the next visible THING
`icicle-object-action'                 - Act on an object of some type
`icicle-occur'                         - Incremental `occur'
`icicle-other-window-or-frame'         - Other window/frame or select
`icicle-pick-color-by-name'            - Set palette color by name
`icicle-plist'                         - Show symbols, property lists
`icicle-pp-eval-expression'            - Enhanced version of vanilla
`icicle-previous-visible-thing'        - Go to previous visible THING
`icicle-read-color'                    - Read a color name or hex RGB
`icicle-read-color-wysiwyg'
`icicle-read-kbd-macro'                - Like vanilla but no <>
`icicle-recent-file'                   - Open recently used file(s)
`icicle-recompute-shell-command-candidates' - Update from search path
`icicle-regexp-list'                   - Choose a list of regexps
`icicle-remove-buffer-candidate'       - Remove always-candidate buf
`icicle-remove-buffer-config'          - From `icicle-buffer-configs'
`icicle-remove-entry-from-saved-completion-set' - From a saved set
`icicle-remove-file-from-recentf-list' - Remove from recent files list
`icicle-remove-saved-completion-set'   - From
                                        `icicle-saved-completion-sets'
`icicle-repeat-complex-command'        - Enhanced version of vanilla
`icicle-reset-option-to-nil'           - Set binary option(s) to nil
`icicle-resolve-file-name'             - Resolve file name at point
`icicle-save-string-to-variable'       - Save text for use with `C-='
`icicle-search'                        - Search with regexps & cycling
`icicle-search-all-tags-bookmark'      - Search tagged bookmarks 
`icicle-search-all-tags-regexp-bookmark'
`icicle-search-autofile-bookmark'      - Search autofile bookmark text
`icicle-search-autonamed-bookmark'     - Search autonamed bookmarks
`icicle-search-bookmark'               - Search bookmarks separately
`icicle-search-bookmark-list-bookmark' - Search bookmark list bookmark
`icicle-search-bookmark-list-marked'   - Search marked bookmarks
`icicle-search-bookmarks-together'     - Search bookmarks together
`icicle-search-buffer'                 - Search buffers
`icicle-search-buff-menu-marked'       - Search marked buffers
`icicle-search-char-property'          - Search for overlay/text props
`icicle-search-dired-bookmark'         - Search Dired bookmarks
`icicle-search-dired-marked-recursive' - Search marked files in Dired
`icicle-search-file'                   - Search multiple files
`icicle-search-file-bookmark'          - Search bookmarked files
`icicle-search-gnus-bookmark'          - Search bookmarked Gnus msgs
`icicle-search-highlight-cleanup'      - Remove search highlighting
`icicle-search-ibuffer-marked'         - Search marked bufs in Ibuffer
`icicle-search-info-bookmark'          - Search bookmarked Info nodes
`icicle-search-keywords'               - Search with regexp keywords
`icicle-search-lines'                  - Same as `icicle-occur'
`icicle-search-local-file-bookmark'    - Search bookmarked local files
`icicle-search-man-bookmark'           - Search bookmarked `man' pages
`icicle-search-non-file-bookmark'      - Search bookmarked buffers
`icicle-search-overlay-property'       - Search for overlay properties
`icicle-search-pages'                  - Search Emacs pages
`icicle-search-paragraphs'             - Search Emacs paragraphs
`icicle-search-region-bookmark'        - Search bookmarked regions
`icicle-search-remote-file-bookmark'   - Search remote bookmarks
`icicle-search-sentences'              - Search sentences as contexts
`icicle-search-some-tags-bookmark'     - Search tagged bookmarks 
`icicle-search-some-tags-regexp-bookmark'
`icicle-search-specific-buffers-bookmark' - Search bookmarked buffers
`icicle-search-specific-files-bookmark' - Search bookmarked files
`icicle-search-temporary-bookmark'     - Search temporary bookmarks
`icicle-search-text-property'          - Search for faces etc.
`icicle-search-thing'                  - Searh with THINGs as contexts
`icicle-search-this-buffer-bookmark'   - Search bookmarks for buffer
`icicle-search-url-bookmark'           - Search bookmarked URLs
`icicle-search-w3m-bookmark'           - Search w3m bookmark text
`icicle-search-w-isearch-string'       - Search using Isearch string
`icicle-search-word'                   - Whole-word search
`icicle-search-xml-element'            - Search XML element contexts
`icicle-search-xml-element-text-node'  - Search XML text() nodes
`icicle-select-bookmarked-region'      - Select bookmarked regions
`icicle-select-frame'                  - Select a frame by name
`icicle-select-window'                 - Select window by buffer name
`icicle-send-bug-report'               - Send Icicles bug report
`icicle-send-signal-to-process'        - Send signals to processes
`icicle-set-option-to-t'               - Set binary option(s) to t
`icicle-set-S-TAB-methods-for-command' - Set `S-TAB' methods for a cmd
`icicle-set-TAB-methods-for-command'   - Set `TAB' methods for a cmd
`icicle-sexp-list'                     - Choose a list of sexps
`icicle-shell-command'                 - Enhanced vanilla version
`icicle-shell-command-on-region'
`icicle-shell-dynamic-complete-command'
`icicle-shell-dynamic-complete-environment-variable'
`icicle-shell-dynamic-complete-filename'
`icicle-show-faces'                    - Show chosen visible faces
`icicle-show-only-faces'               - Hide all but chosen faces
`icicle-string-list'                   - Choose a list of strings
`icicle-synonyms'                      - Show synonyms matching regexp
`icicle-tag-a-file'                    - Tag a file a la delicious
`icicle-tags-search'                   - Search files in tags tables
`icicle-toggle-~-for-home-dir'         - Toggle using `~' for $HOME
`icicle-toggle-alternative-sorting'    - Swap alternative sort
`icicle-toggle-angle-brackets'         - Toggle using angle brackets
`icicle-toggle-annotation'             - Toggle candidate annotations
`icicle-toggle-case-sensitivity'       - Toggle case sensitivity
`icicle-toggle-C-for-actions'          - Toggle using `C-' for actions
`icicle-toggle-completions-format'     - Toggle horizontal/vertical
`icicle-toggle-dot'                    - Toggle `.' matching newlines
`icicle-toggle-expand-to-common-match' - Toggle input ECM expansion
`icicle-toggle-hiding-common-match'    - Toggle match, `*Completions*'
`icicle-toggle-hiding-non-matching-lines'- Toggle no-match lines
`icicle-toggle-highlight-all-current'  - Toggle max search highlight
`icicle-toggle-highlight-historical-candidates'
                                       - Toggle past-input highlight
`icicle-toggle-highlight-saved-candidates'
                                       - Toggle highlighting saved
`icicle-toggle-ignored-extensions'     - Toggle ignored files
`icicle-toggle-ignored-space-prefix'   - Toggle ignoring space prefix
`icicle-toggle-ignoring-comments'      - Toggle ignoring comments
`icicle-toggle-literal-replacement'    - Toggle escaping regexp chars
`icicle-toggle-option'                 - Toggle binary user option
`icicle-toggle-proxy-candidates'       - Toggle proxy candidates
`icicle-toggle-regexp-quote'           - Toggle regexp escaping
`icicle-toggle-remote-file-testing'    - Toggle testing whether remote
`icicle-toggle-search-cleanup'         - Toggle search highlighting
`icicle-toggle-search-complementing-domain' - Toggle complement search
`icicle-toggle-search-replace-common-match' - Toggle ECM replacement
`icicle-toggle-search-replace-whole'   - Toggle replacing whole hit
`icicle-toggle-search-whole-word'      - Toggle whole-word searching
`icicle-toggle-show-multi-completion'  - Toggle multi-completions
`icicle-toggle-sorting'                - Toggle sorting of completions
`icicle-toggle-transforming'           - Toggle duplicate removal
`icicle-toggle-WYSIWYG-Completions'   - Toggle WYSIWYG `*Completions*'
`icicle-untag-a-file'                  - Remove some tags from a file
`icicle-vardoc'                        - Show variable description
`icicle-where-is'                      - `where-is' multi-command
`icicle-yank-maybe-completing'         - `yank' maybe using completion
`icicle-yank-pop-commands'             - Yank DWIM, per context
`icicle-zap-to-char'                   - Kill through N CHARs by name
`toggle' (alias)                       - Toggle binary user option"
          :global t :group 'Icicles :lighter " Icy" :init-value nil
          (cond (icicle-mode
                 ;; (when (interactive-p)
                 ;;   (unless (or window-system  (and (fboundp 'daemonp)  (daemonp)))
                 ;;     (with-output-to-temp-buffer "*Attention*"
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
                   (ad-enable-advice 'describe-face 'before 'icicle-respect-WYSIWYG))
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

Icicle mode defines the following top-level commands.  In many cases
there are also `-other-window' versions.

`clear-option' (alias)                 - Set binary option(s) to nil
`icicle-add-buffer-candidate'          - Add always-candidate buffer
`icicle-add-buffer-config'             - To `icicle-buffer-configs'
`icicle-add-entry-to-saved-completion-set' - Add completion to a set
`icicle-add-file-to-fileset'           - Add a file to a fileset
`icicle-add/update-saved-completion-set' - To
                                        `icicle-saved-completion-sets'
`icicle-apply'                         - Apply function to alist items
`icicle-apropos'                       - `apropos', but shows matches
`icicle-apropos-command'               - Enhanced `apropos-command'
`icicle-apropos-options-of-type'       - Show options of a given type
`icicle-apropos-variable'              - Enhanced `apropos-variable'
`icicle-apropos-vars-w-val-satisfying' - Show vars of given values
`icicle-bbdb-complete-mail'            - Complete a user name/address
`icicle-bbdb-complete-name'            - Complete a user name/address
`icicle-bookmark'                      - Jump to a bookmark
`icicle-bookmark-a-file'               - Create an autofile bookmark
`icicle-bookmark-all-tags'             - Jump to tagged bookmark
`icicle-bookmark-all-tags-regexp'      - Jump to tagged bookmark
`icicle-bookmark-autofile'             - Jump to an autofile bookmark
`icicle-bookmark-autofile-all-tags'    - Jump to a tagged autofile
`icicle-bookmark-autofile-all-tags-regexp'
`icicle-bookmark-autofile-some-tags'
`icicle-bookmark-autofile-some-tags-regexp'
`icicle-bookmark-autonamed'            - Jump to an autonamed bookmark
`icicle-bookmark-autonamed-this-buffer'
`icicle-bookmark-bookmark-file'        - Load a bookmark file bookmark
`icicle-bookmark-bookmark-list'        - Jump to a bookmark list
`icicle-bookmark-cmd'                  - Set or jump to a bookmark
`icicle-bookmark-desktop'              - Jump to a desktop bookmark
`icicle-bookmark-dired'                - Jump to a Dired bookmark
`icicle-bookmarked-buffer-list'        - Choose bookmarked buffers
`icicle-bookmarked-file-list'          - Choose bookmarked files
`icicle-bookmark-file'                 - Jump to a file bookmark
`icicle-bookmark-file-all-tags'        - Jump to tagged file bookmark
`icicle-bookmark-file-all-tags-regexp'
`icicle-bookmark-file-some-tags'
`icicle-bookmark-file-some-tags-regexp'
`icicle-bookmark-file-this-dir'        - Jump to file in directory
`icicle-bookmark-file-this-dir-all-tags'
`icicle-bookmark-file-this-dir-all-tags-regexp'
`icicle-bookmark-file-this-dir-some-tags'
`icicle-bookmark-file-this-dir-some-tags-regexp'
`icicle-bookmark-gnus'                 - Jump to a Gnus bookmark
`icicle-bookmark-info'                 - Jump to an Info bookmark
`icicle-bookmark-jump'                 - Jump to any bookmark
`icicle-bookmark-list'                 - Choose a list of bookmarks
`icicle-bookmark-local-file'           - Jump to local-file bookmark
`icicle-bookmark-man'                  - Jump to a `man'-page bookmark
`icicle-bookmark-non-file'             - Jump to a buffer bookmark
`icicle-bookmark-region'               - Jump to a region bookmark
`icicle-bookmark-remote-file'          - Jump to a remote file
`icicle-bookmark-save-marked-files'    - Save file names as candidates
`icicle-bookmark-save-marked-files-persistently'
`icicle-bookmark-save-marked-files-to-variable'
`icicle-bookmark-set'                  - Set a bookmark
`icicle-bookmark-some-tags'            - Jump to tagged bookmark
`icicle-bookmark-some-tags-regexp'     - Jump to tagged bookmark
`icicle-bookmark-specific-buffers'     - Jump to a bookmarked buffer
`icicle-bookmark-specific-files'       - Jump to a bookmarked file
`icicle-bookmark-temporary'            - Jump to a temporary bookmark
`icicle-bookmark-this-buffer'          - Jump to bookmark for this buf
`icicle-bookmark-url'                  - Jump to a URL bookmark
`icicle-bookmark-w3m'                  - Jump to a W3M (URL) bookmark
`icicle-buffer'                        - Switch to buffer
`icicle-buffer-config'                 - Pick `icicle-buffer' options
`icicle-buffer-list'                   - Choose a list of buffer names
`icicle-change-alternative-sort-order' - Choose an alternative sort
`icicle-change-sort-order'             - Choose a sort order
`icicle-clear-history'                 - Clear entries from a history
`icicle-color-theme'                   - Change color theme
`icicle-comint-command'                - Reuse shell etc. command
`icicle-comint-dynamic-complete'       - Text completion in shell
`icicle-comint-dynamic-complete-filename'
`icicle-comint-replace-by-expanded-filename'
`icicle-comint-search'                 - Reuse shell etc. command
`icicle-command-abbrev'                - Multi-command `M-x' + abbrevs
`icicle-compilation-search'            - `icicle-search' and show hits
`icicle-complete-thesaurus-entry'      - Complete word using thesaurus
`icicle-completing-yank'               - `yank' using completion
`icicle-customize-apropos'             - Enhanced `customize-apropos'
`icicle-customize-apropos-faces',
`icicle-customize-apropos-groups'
`icicle-customize-apropos-options'
`icicle-customize-apropos-options-of-type'
`icicle-customize-apropos-opts-w-val-satisfying'
`icicle-customize-face'                - Multi-`customize-face'
`icicle-customize-icicles-group'       - Customize options and faces
`icicle-cycle-expand-to-common-match'  - Cycle input ECM expansion
`icicle-cycle-incremental-completion'  - Cycle incremental completion
`icicle-dabbrev-completion'            - Enhanced `dabbrev-completion'
`icicle-delete-file'                   - Delete file/directory
`icicle-delete-window'                 - Delete window (`C-u': buffer)
`icicle-delete-windows'                - Delete all windows for buffer
`icicle-describe-file'                 - Describe a file
`icicle-describe-option-of-type'       - Describe option of given type
`icicle-describe-var-w-val-satisfying' - Describe var satisfying pred
`icicle-directory-list'                - Choose a list of directories
`icicle-dired'                         - Multi-command Dired
`icicle-dired-chosen-files'            - Dired a set of files & dirs
`icicle-dired-project'                 - Dired a saved project
`icicle-dired-saved-file-candidates'   - Dired set of saved file names
`icicle-dired-save-marked'             - Save marked file names
`icicle-dired-save-marked-more'
`icicle-dired-save-marked-more-recursive'
`icicle-dired-save-marked-recursive'
`icicle-dired-save-marked-persistently'  ... to cache file or fileset
`icicle-dired-save-marked-to-cache-file-recursive' ... to cache-file
`icicle-dired-save-marked-to-variable'   ... to variable
`icicle-dired-save-marked-to-variable-recursive'
`icicle-dired-smart-shell-command'     - Enhanced version of vanilla
`icicle-doc'                           - Show doc for fn, var, or face
`icicle-doremi-candidate-width-factor+' - +/- candidate column width
`icicle-doremi-increment-max-candidates+' - +/- max number candidates
`icicle-doremi-increment-swank-prefix-length+' - +/- swank prefix
`icicle-doremi-increment-swank-timeout+' - +/- swank completion msec
`icicle-doremi-increment-variable+'    - Increment var using Do Re Mi
`icicle-doremi-inter-candidates-min-spaces+' - +/- candidate spacing
`icicle-doremi-zoom-Completions+'      - +/- `*Completions*' text size
`icicle-ess-complete-object-name'      - Complete an ESS object
`icicle-ess-R-complete-object-name'    - Complete an ESS object in R
`icicle-exchange-point-and-mark'       - Flip, save, or select region
`icicle-execute-extended-command'      - Multi-command `M-x'
`icicle-execute-named-keyboard-macro'  - Execute named keyboard macro
`icicle-face-list'                     - Choose a list of face names
`icicle-file-list'                     - Choose a list of file names
`icicle-file'                          - Visit file/directory
`icicle-find-file'                     -       same: relative only
`icicle-find-file-absolute'            -       same: absolute only
`icicle-find-file-read-only'           -       same: read-only
`icicle-find-file-all-tags'            - Visit Emacs-tagged file
`icicle-find-file-all-tags-regexp'
`icicle-find-file-some-tags'
`icicle-find-file-some-tags-regexp'
`icicle-find-file-handle-bookmark'     - Find file handling bookmark
`icicle-find-file-in-tags-table'       - File in Emacs tags table
`icicle-find-file-tagged'              - Visit tagged file
`icicle-find-first-tag'                - Visit definition with tag
`icicle-find-tag'                      - Visit definition with tag
`icicle-font'                          - Change font of frame
`icicle-frame-bg'                      - Change background of frame
`icicle-frame-fg'                      - Change foreground of frame
`icicle-fundoc'                        - Show function description
`icicle-goto-global-marker'            - Go to a global marker
`icicle-goto-global-marker-or-pop-global-mark'
`icicle-goto-marker'                   - Go to a marker in this buffer
`icicle-grep-saved-file-candidates'    - Grep saved file candidates
`icicle-gud-gdb-complete-command'      - Enhanced version of vanilla
`icicle-hide/show-comments'            - Hide or show comments
`icicle-imenu*'                        - Navigate among Imenu entries
`icicle-imenu-full'                            same: full definitions
`icicle-imenu-command'                 - Navigate among command defs
`icicle-imenu-command-full'                    same: full definitions
`icicle-imenu-face'                    - Navigate among face defs
`icicle-imenu-face-full'                       same: full definitions
`icicle-imenu-key-explicit-map'        - Navigate among key defs
`icicle-imenu-key-explicit-map-full'           same: full definitions
`icicle-imenu-key-implicit-map'                same: no explicit map
`icicle-imenu-key-implicit-map-full'           same: full definitions
`icicle-imenu-macro'                   - Navigate among macro defs
`icicle-imenu-macro-full'                      same: full definitions
`icicle-imenu-non-interactive-function' - Navigate among function defs
`icicle-imenu-non-interactive-function-full'   same: full definitions
`icicle-imenu-user-option'             - Navigate among option defs
`icicle-imenu-user-option-full'                same: full definitions
`icicle-imenu-variable'                - Navigate among variable defs
`icicle-imenu-variable-full'                   same: full definitions
`icicle-increment-option'              - Increment numeric option
`icicle-increment-variable'            - Increment numeric variable
`icicle-Info-goto-node'                - Multi-cmd `Info-goto-node'
`icicle-Info-index'                    - Multi-command `Info-index'
`icicle-Info-menu'                     - Multi-command `Info-menu'
`icicle-insert-buffer'                 - Multi-command `insert-buffer'
`icicle-insert-thesaurus-entry'        - Insert thesaurus entry(s)
`icicle-keyword-list'                  - Choose a list of keywords
`icicle-kill-buffer'                   - Kill buffer
`icicle-lisp-complete-symbol'          - Enhanced version of vanilla
`icicle-locate'                        - Run `locate' then visit files
`icicle-locate-file'                   - Visit file(s) in a directory
`icicle-locate-file-no-symlinks'               same, but do not follow
`icicle-minibuffer-default-add-dired-shell-commands' - Enhanced
`icicle-minibuffer-help'               - Show Icicles minibuffer help
`icicle-mode' or `icy-mode'            - Toggle Icicle mode
`icicle-next-S-TAB-completion-method'  - Next S-TAB completion method
`icicle-next-TAB-completion-method'    - Next TAB completion method
`icicle-next-visible-thing'            - Go to the next visible THING
`icicle-object-action'                 - Act on an object of some type
`icicle-occur'                         - Incremental `occur'
`icicle-other-window-or-frame'         - Other window/frame or select
`icicle-plist'                         - Show symbols, property lists
`icicle-pp-eval-expression'            - Enhanced version of vanilla
`icicle-previous-visible-thing'        - Go to previous visible THING
`icicle-read-color'                    - Read a color name or hex RGB
`icicle-read-color-wysiwyg'
`icicle-read-kbd-macro'                - Like vanilla but no <>
`icicle-recent-file'                   - Open recently used file(s)
`icicle-recompute-shell-command-candidates' - Update from search path
`icicle-regexp-list'                   - Choose a list of regexps
`icicle-remove-buffer-candidate'       - Remove always-candidate buf
`icicle-remove-buffer-config'          - From `icicle-buffer-configs'
`icicle-remove-entry-from-saved-completion-set' - From a saved set
`icicle-remove-file-from-recentf-list' - Remove from recent files list
`icicle-remove-saved-completion-set'   - From
                                        `icicle-saved-completion-sets'
`icicle-repeat-complex-command'        - Enhanced version of vanilla
`icicle-reset-option-to-nil'           - Set binary option(s) to nil
`icicle-resolve-file-name'             - Resolve file name at point
`icicle-save-string-to-variable'       - Save text for use with `C-='
`icicle-search'                        - Search with regexps & cycling
`icicle-search-all-tags-bookmark'      - Search tagged bookmarks 
`icicle-search-all-tags-regexp-bookmark'
`icicle-search-autofile-bookmark'      - Search autofile bookmark text
`icicle-search-autonamed-bookmark'     - Search autonamed bookmarks
`icicle-search-bookmark'               - Search bookmarks separately
`icicle-search-bookmark-list-bookmark' - Search bookmark list bookmark
`icicle-search-bookmark-list-marked'   - Search marked bookmarks
`icicle-search-bookmarks-together'     - Search bookmarks together
`icicle-search-buffer'                 - Search buffers
`icicle-search-buff-menu-marked'       - Search marked buffers
`icicle-search-char-property'          - Search for overlay/text props
`icicle-search-dired-bookmark'         - Search Dired bookmarks
`icicle-search-dired-marked-recursive' - Search marked files in Dired
`icicle-search-file'                   - Search multiple files
`icicle-search-file-bookmark'          - Search bookmarked files
`icicle-search-gnus-bookmark'          - Search bookmarked Gnus msgs
`icicle-search-highlight-cleanup'      - Remove search highlighting
`icicle-search-ibuffer-marked'         - Search marked bufs in Ibuffer
`icicle-search-info-bookmark'          - Search bookmarked Info nodes
`icicle-search-keywords'               - Search with regexp keywords
`icicle-search-lines'                  - Same as `icicle-occur'
`icicle-search-local-file-bookmark'    - Search bookmarked local files
`icicle-search-man-bookmark'           - Search bookmarked `man' pages
`icicle-search-non-file-bookmark'      - Search bookmarked buffers
`icicle-search-overlay-property'       - Search for overlay properties
`icicle-search-pages'                  - Search Emacs pages
`icicle-search-paragraphs'             - Search Emacs paragraphs
`icicle-search-region-bookmark'        - Search bookmarked regions
`icicle-search-remote-file-bookmark'   - Search remote bookmarks
`icicle-search-sentences'              - Search sentences as contexts
`icicle-search-some-tags-bookmark'     - Search tagged bookmarks 
`icicle-search-some-tags-regexp-bookmark'
`icicle-search-specific-buffers-bookmark' - Search bookmarked buffers
`icicle-search-specific-files-bookmark' - Search bookmarked files
`icicle-search-temporary-bookmark'     - Search temporary bookmarks
`icicle-search-text-property'          - Search for faces etc.
`icicle-search-thing'                  - Searh with THINGs as contexts
`icicle-search-this-buffer-bookmark'   - Search bookmarks for buffer
`icicle-search-url-bookmark'           - Search bookmarked URLs
`icicle-search-w3m-bookmark'           - Search w3m bookmark text
`icicle-search-w-isearch-string'       - Search using Isearch string
`icicle-search-word'                   - Whole-word search
`icicle-search-xml-element'            - Search XML element contexts
`icicle-search-xml-element-text-node'  - Search XML text() nodes
`icicle-select-bookmarked-region'      - Select bookmarked regions
`icicle-select-frame'                  - Select a frame by name
`icicle-select-window'                 - Select window by buffer name
`icicle-send-bug-report'               - Send Icicles bug report
`icicle-set-option-to-t'               - Set binary option(s) to t
`icicle-set-S-TAB-methods-for-command' - Set `S-TAB' methods for a cmd
`icicle-set-TAB-methods-for-command'   - Set `TAB' methods for a cmd
`icicle-sexp-list'                     - Choose a list of sexps
`icicle-shell-command'                 - Enhanced vanilla version
`icicle-shell-command-on-region'
`icicle-shell-dynamic-complete-command'
`icicle-shell-dynamic-complete-environment-variable'
`icicle-shell-dynamic-complete-filename'
`icicle-string-list'                   - Choose a list of strings
`icicle-synonyms'                      - Show synonyms matching regexp
`icicle-tag-a-file'                    - Tag a file a la delicious
`icicle-tags-search'                   - Search files in tags tables
`icicle-toggle-~-for-home-dir'         - Toggle using `~' for $HOME
`icicle-toggle-alternative-sorting'    - Swap alternative sort
`icicle-toggle-angle-brackets'         - Toggle using angle brackets
`icicle-toggle-annotation'             - Toggle candidate annotations
`icicle-toggle-case-sensitivity'       - Toggle case sensitivity
`icicle-toggle-C-for-actions'          - Toggle using `C-' for actions
`icicle-toggle-completions-format'     - Toggle horizontal/vertical
`icicle-toggle-dot'                    - Toggle `.' matching newlines
`icicle-toggle-expand-to-common-match' - Toggle input ECM expansion
`icicle-toggle-hiding-common-match'    - Toggle match, `*Completions*'
`icicle-toggle-hiding-non-matching-lines'- Toggle no-match lines
`icicle-toggle-highlight-all-current'  - Toggle max search highlight
`icicle-toggle-highlight-historical-candidates'
                                       - Toggle past-input highlight
`icicle-toggle-highlight-saved-candidates'
                                       - Toggle highlighting saved
`icicle-toggle-ignored-extensions'     - Toggle ignored files
`icicle-toggle-ignored-space-prefix'   - Toggle ignoring space prefix
`icicle-toggle-ignoring-comments'      - Toggle ignoring comments
`icicle-toggle-literal-replacement'    - Toggle escaping regexp chars
`icicle-toggle-option'                 - Toggle binary user option
`icicle-toggle-proxy-candidates'       - Toggle proxy candidates
`icicle-toggle-regexp-quote'           - Toggle regexp escaping
`icicle-toggle-remote-file-testing'    - Toggle testing whether remote
`icicle-toggle-search-cleanup'         - Toggle search highlighting
`icicle-toggle-search-complementing-domain' - Toggle complement search
`icicle-toggle-search-replace-common-match' - Toggle ECM replacement
`icicle-toggle-search-replace-whole'   - Toggle replacing whole hit
`icicle-toggle-search-whole-word'      - Toggle whole-word searching
`icicle-toggle-show-multi-completion'  - Toggle multi-completions
`icicle-toggle-sorting'                - Toggle sorting of completions
`icicle-toggle-transforming'           - Toggle duplicate removal
`icicle-toggle-WYSIWYG-Completions'   - Toggle WYSIWYG `*Completions*'
`icicle-untag-a-file'                  - Remove some tags from a file
`icicle-vardoc'                        - Show variable description
`icicle-where-is'                      - `where-is' multi-command
`icicle-yank-maybe-completing'         - `yank' maybe using completion
`icicle-yank-pop-commands'             - Yank DWIM, per context
`toggle' (alias)                       - Toggle binary user option"
    (interactive "P")
    (setq icicle-mode  (if arg (> (prefix-numeric-value arg) 0) (not icicle-mode)))
    (icicle-define-minibuffer-maps icicle-mode)
    (cond (icicle-mode
           ;; (when (interactive-p)
           ;;   (unless (or window-system  (and (fboundp 'daemonp)  (daemonp)))
           ;;     (with-output-to-temp-buffer "*Attention*"
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
  ;; Reset `icicle-current-TAB-method' and `icicle-apropos-complete-match-fn' if temporary.
  ;; Save this top-level command as `icicle-last-top-level-command'
  ;; Reset `icicle-candidates-alist' to ().
  (when (= 0 (recursion-depth))
    (let ((TAB-method  (get 'icicle-last-top-level-command 'icicle-current-TAB-method))
          (apropos-fn  (get 'icicle-last-top-level-command 'icicle-apropos-complete-match-fn)))
      (when TAB-method (setq icicle-current-TAB-method  TAB-method))
      (when apropos-fn (setq icicle-apropos-complete-match-fn  apropos-fn)))
    (setq icicle-last-top-level-command   this-command
          icicle-candidates-alist         ())))

(defun icicle-define-icicle-maps ()
  "Define `icicle-mode-map' and `icicle-menu-map'."
  (setq icicle-mode-map  (make-sparse-keymap)) ; Recreate it each time, to capture latest bindings.

  ;; Define `Icicles' menu-bar menu.  Create it only once: sacrifice any new bindings for speed.
  (unless icicle-menu-map
    (setq icicle-menu-map  (make-sparse-keymap "Icicles"))
    (define-key icicle-menu-map [icicle-mode] '(menu-item "Turn Off Icicle Mode" icicle-mode))

    ;; End of `Icicles' menu -----------------------------------------
    (define-key icicle-menu-map [icicle-report-bug]
      '(menu-item "Send Icicles Bug Report" icicle-send-bug-report))
    (define-key icicle-menu-map [icicle-customize-icicles-group]
      '(menu-item "Customize Icicles" icicle-customize-icicles-group))


    ;; `Options' -----------------------------------------------------
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
    (define-key icicle-options-menu-map [icicle-toggle-C-for-actions]
      '(menu-item "Toggle Using `C-' for Actions" icicle-toggle-C-for-actions :keys "M-g"
        :help "Toggle option `icicle-use-C-for-actions-flag'"))
    (define-key icicle-options-menu-map [icicle-toggle-~-for-home-dir]
      '(menu-item "Toggle Using `~' for $HOME" icicle-toggle-~-for-home-dir :keys "M-~"
        :help "Toggle option `icicle-use-~-for-home-dir-flag'"))
    (define-key icicle-options-menu-map [icicle-toggle-WYSIWYG-Completions]
      '(menu-item "Toggle WYSIWYG For `*Completions*'" icicle-toggle-WYSIWYG-Completions
        :help "Toggle option `icicle-WYSIWYG-Completions-flag'"))
    (define-key icicle-options-menu-map [icicle-next-TAB-completion-method]
      '(menu-item "Next `TAB' Completion Method" icicle-next-TAB-completion-method
        :keys "C-(" :help "Cycle to the next `TAB' completion method (C-u: ONE-OFF)"))
    (define-key icicle-options-menu-map [icicle-next-S-TAB-completion-method]
      '(menu-item "Next `S-TAB' Completion Method" icicle-next-S-TAB-completion-method
        :keys "M-(" :help "Cycle to the next `S-TAB' completion method (C-u: ONE-OFF)"))
    (when (fboundp 'icicle-cycle-image-file-thumbnail)
      (define-key icicle-options-menu-map [icicle-cycle-image-file-thumbnail]
        '(menu-item "Next Image-File Thumbnail Setting" icicle-cycle-image-file-thumbnail
          :keys "C-x t" :help "Cycle Thumbnail Image File Setting")))
    (define-key icicle-options-menu-map [icicle-toggle-search-cleanup]
      '(menu-item "Toggle Icicle-Search Highlighting Cleanup" icicle-toggle-search-cleanup
        :keys "C-." :help "Toggle option `icicle-search-cleanup-flag'"))
    (define-key icicle-options-menu-map [icicle-toggle-search-replace-common-match]
      '(menu-item "Toggle Replacing Longest Common Match"
        icicle-toggle-search-replace-common-match :enable icicle-searching-p :keys "M-;"
        :help "Toggle option `icicle-search-replace-common-match-flag'"))
    (define-key icicle-options-menu-map [icicle-toggle-search-replace-whole]
      '(menu-item "Toggle Replacing Whole Search Hit" icicle-toggle-search-replace-whole
        :enable icicle-searching-p :keys "M-_"
        :help "Toggle option `icicle-search-replace-whole-candidate-flag'"))
    (define-key icicle-options-menu-map [icicle-toggle-search-whole-word]
      '(menu-item "Toggle Whole-Word Searching (Icicles Search)"
        icicle-toggle-search-whole-word
        :enable icicle-searching-p :keys "M-q"
        :help "Toggle `icicle-search-whole-word-flag'"))
    (define-key icicle-options-menu-map [icicle-toggle-regexp-quote]
      '(menu-item "Toggle Escaping Special Chars" icicle-toggle-regexp-quote :keys "C-`"
        :help "Toggle option `icicle-regexp-quote-flag'"))
    (define-key icicle-options-menu-map [icicle-toggle-dot]
      '(menu-item "Toggle `.' Matching Newlines Too" icicle-toggle-dot :keys "C-M-."
        :help "Toggle `icicle-dot-string' between `.' and `icicle-anychar-regexp'"))
    (define-key icicle-options-menu-map [icicle-cycle-incremental-completion]
      '(menu-item "Cycle Incremental Completion" icicle-cycle-incremental-completion
        :keys "C-#" :help "Cycle option `icicle-incremental-completion'"))
    (define-key icicle-options-menu-map [icicle-toggle-show-multi-completion]
      '(menu-item "Toggle Showing Multi-Completions" icicle-toggle-show-multi-completion
        :help "Toggle option `icicle-show-multi-completion-flag'"))
    (define-key icicle-options-menu-map [icicle-toggle-completions-format]
      '(menu-item "Toggle Horizontal/Vertical Layout" icicle-toggle-completions-format
        :help "Toggle option `icicle-hide-non-matching-lines-flag'"))
    (define-key icicle-options-menu-map [icicle-toggle-hiding-non-matching-lines]
      '(menu-item "Toggle Hiding Non-Matching Lines"
        icicle-toggle-hiding-non-matching-lines
        :keys "C-u C-x ." :help "Toggle option `icicle-hide-non-matching-lines-flag'"))
    (define-key icicle-options-menu-map [icicle-toggle-hiding-common-match]
      '(menu-item "Toggle Hiding Common Match" icicle-toggle-hiding-common-match
        :keys "C-x ." :help "Toggle option `icicle-hide-common-match-in-Completions-flag'"))
    (define-key icicle-options-menu-map [icicle-toggle-expand-to-common-match]
      '(menu-item "Toggle Expansion to Common Match" icicle-toggle-expand-to-common-match
        :keys "C-\"" :help "Toggle option `icicle-expand-input-to-common-match'"))
    (define-key icicle-options-menu-map [icicle-toggle-ignoring-comments]
      '(menu-item "Toggle Ignoring Comments" icicle-toggle-ignoring-comments
        :keys "C-M-;" :help "Toggle option `icicle-ignore-comments-flag'"))
    (define-key icicle-options-menu-map [icicle-toggle-ignored-space-prefix]
      '(menu-item "Toggle Ignoring Space Prefix" icicle-toggle-ignored-space-prefix
        :keys "M-_" :help "Toggle option `icicle-buffer-ignore-space-prefix-flag'"))
    (define-key icicle-options-menu-map [icicle-toggle-ignored-extensions]
      '(menu-item "Toggle Ignored File Extensions" icicle-toggle-ignored-extensions
        :keys "C-." :help "Toggle respect of `completion-ignored-extensions'"))
    (define-key icicle-options-menu-map [icicle-toggle-remote-file-testing]
      '(menu-item "Toggle Remote File Handling" icicle-toggle-remote-file-testing
        :enable (not icicle-searching-p) :keys "C-^"
        :help "Toggle option `icicle-test-for-remote-files-flag'"))
    (when (> emacs-major-version 20)
      (define-key icicle-options-menu-map [icicle-toggle-angle-brackets]
        '(menu-item "Toggle Angle Brackets" icicle-toggle-angle-brackets
          :help "Toggle option `icicle-key-descriptions-use-<>-flag'")))
    (define-key icicle-options-menu-map [icicle-toggle-annotation]
      '(menu-item "Toggle Candidate Annotation"
        icicle-toggle-annotation :keys "C-x C-a"
        :help "Toggle option `icicle-show-annotations-flag': hide/show annotations"))
    (define-key icicle-options-menu-map [icicle-toggle-highlight-saved-candidates]
      '(menu-item "Toggle Highlighting Saved Candidates"
        icicle-toggle-highlight-saved-candidates :keys "S-pause"
        :help "Toggle option `icicle-highlight-saved-candidates-flag'"))
    (define-key icicle-options-menu-map [icicle-toggle-highlight-historical-candidates]
      '(menu-item "Toggle Highlighting Past Inputs"
        icicle-toggle-highlight-historical-candidates :keys "C-pause"
        :help "Toggle option `icicle-highlight-historical-candidates-flag'"))
    (define-key icicle-options-menu-map [icicle-toggle-case-sensitivity]
      '(menu-item "Toggle Case Sensitivity" icicle-toggle-case-sensitivity :keys "C-A"
        :help "Toggle `case-fold-search', `completion-ignore-case' (C-u: file & buffer too)"))
    (define-key icicle-options-menu-map [icicle-toggle-proxy-candidates]
      '(menu-item "Toggle Including Proxy Candidates" icicle-toggle-proxy-candidates
        :keys "C-M-_" :help "Toggle option `icicle-add-proxy-candidates-flag'"))
    (define-key icicle-options-menu-map [icicle-toggle-transforming]
      '(menu-item "Toggle Duplicate Removal" icicle-toggle-transforming :keys "C-$"
        :help "Toggle use of `icicle-transform-function' (default: remove dups)"))
    (define-key icicle-options-menu-map [icicle-toggle-alternative-sorting]
      '(menu-item "Swap Alternative Sort" icicle-toggle-alternative-sorting :keys "C-M-,"
        :help "Swap current sort order for current alternative sort order"))
    (define-key icicle-options-menu-map [icicle-change-alternative-sort-order]
      '(menu-item "Change Alternative Sort Order" icicle-change-alternative-sort-order
        :keys "M-," :help "Choose alt sort order (C-9: reverse, C-u: cyle/complete)"))
    (define-key icicle-options-menu-map [icicle-change-sort-order]
      '(menu-item "Change Sort Order" icicle-change-sort-order
        :enable (not icicle-inhibit-sort-p) :keys "C-,"
        :help "Choose sort order (C-9: reverse, C-u: cyle/complete)"))
    (when (fboundp 'doremi)
      (when (fboundp 'text-scale-increase) ; Emacs 23+.
        (define-key icicle-options-menu-map [icicle-doremi-zoom-Completions+]
          '(menu-item "*Completions* Zoom Factor - Do Re Mi"
            icicle-doremi-zoom-Completions+
            :visible (get-buffer-window "*Completions*" 'visible) :keys "C-x -"
            :help "Zoom text in `*Completions*' incrementally")))
      (define-key icicle-options-menu-map [icicle-doremi-inter-candidates-min-spaces+]
        '(menu-item "*Completions* Candidate Spacing - Do Re Mi"
          icicle-doremi-inter-candidates-min-spaces+
          :visible (get-buffer-window "*Completions*" 'visible) :keys "C-x |"
          :help "Change `icicle-inter-candidates-min-spaces' incrementally"))
      (define-key icicle-options-menu-map [icicle-doremi-candidate-width-factor+]
        '(menu-item "*Completions* Column Width - Do Re Mi"
          icicle-doremi-candidate-width-factor+
          :visible (get-buffer-window "*Completions*" 'visible) :keys "C-x w"
          :help "Change `icicle-candidate-width-factor' incrementally"))
      (define-key icicle-options-menu-map [icicle-doremi-increment-swank-prefix-length+]
        '(menu-item "Swank Min Match Chars - Do Re Mi"
          icicle-doremi-increment-swank-prefix-length+
          :visible (eq (icicle-current-TAB-method) 'swank) :keys "C-x 2"
          :help "Change `icicle-swank-prefix-length' incrementally"))
      (define-key icicle-options-menu-map [icicle-doremi-increment-swank-timeout+]
        '(menu-item "Swank Timeout - Do Re Mi"
          icicle-doremi-increment-swank-timeout+
          :visible  (eq (icicle-current-TAB-method) 'swank) :keys "C-x 1"
          :help "Change `icicle-swank-timeout' incrementally"))
      (define-key icicle-options-menu-map [icicle-doremi-increment-max-candidates+]
        '(menu-item "Max # of Completions - Do Re Mi"
          icicle-doremi-increment-max-candidates+
          :visible (active-minibuffer-window) :keys "C-x #"
          :help "Change `icicle-max-candidates' incrementally")))


    ;; Beginning of non-submenu `Icicles' menu -----------------------
    (define-key icicle-menu-map [icicle-help]
      '(menu-item "Icicles Help" icicle-minibuffer-help
        :help "Display help for minibuffer input and completion" :keys "M-? in minibuf"))
    (define-key icicle-menu-map [icicle-abort]
      '(menu-item "Cancel Minibuffer" icicle-abort-recursive-edit
        :enable (active-minibuffer-window)
        :help "Cancel minibuffer input and return to higher level"))
    (define-key icicle-menu-map [icicle-separator-last] '("--"))

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
    (when (fboundp 'icicle-kmacro)
      (define-key icicle-menu-map [icicle-kmacro]
        '(menu-item "+ Execute Nth Keyboard Macro..." icicle-kmacro
          :enable (or (kmacro-ring-head)  kmacro-ring)
          :help "Execute a keyboard macro according to its position in `kmacro-ring'")))
    (define-key icicle-menu-map [icicle-execute-named-keyboard-macro]
      '(menu-item "+ Execute Named Keyboard Macro..." icicle-execute-named-keyboard-macro
        :help "Read the name of a keyboard macro, then execute it"))


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
      '(menu-item "Options Matching with a Value Satisfying..."
        icicle-customize-apropos-opts-w-val-satisfying
        :help "Customize options whose values satisfy a predicate that match a regexp"))
    (define-key icicle-custom-menu-map [icicle-customize-apropos-options-of-type]
      '(menu-item "Options of Type Matching..."
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
             '(menu-item "Variables with a Value Satisfying..." icicle-apropos-vars-w-val-satisfying
               :help "Show variables whose values satisfy a given predicate"))
           (define-key icicle-apropos-menu-map [icicle-apropos-value]
             '(menu-item "Variables with Values..." icicle-apropos-value
               :help "Show variables that match by name and/or value"))
           (define-key icicle-apropos-menu-map [icicle-apropos-variable]
             '(menu-item "Variables..." icicle-apropos-variable
               :help "Show variables that match PATTERN"))
           (define-key icicle-apropos-menu-map [icicle-apropos-options-of-type]
             '(menu-item "Options of Type..." icicle-apropos-options-of-type
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


    ;; `Search' and `Emacs Tags' (or `Tags') -------------------------
    (cond ((not icicle-touche-pas-aux-menus-flag)
           (cond ((boundp 'menu-bar-search-tags-menu) ; `Tags' menu defined in `menu-bar+.el'.
                  (defvar icicle-search-menu-map (make-sparse-keymap)
                    "`Search' > `Icicles' menu.")
                  (define-key menu-bar-search-menu [icicles]
                    (list 'menu-item "Icicles" icicle-search-menu-map :visible 'icicle-mode))
                  (defvar icicle-search-tags-menu-map (make-sparse-keymap)
                    "`Search' >  `Tags' > `Icicles' menu.")
                  (define-key menu-bar-search-tags-menu [icicles]
                    (list 'menu-item "Icicles" icicle-search-tags-menu-map :visible 'icicle-mode)))
                 (t
                  (defvar icicle-search-menu-map (make-sparse-keymap)
                    "`Search' > `Icicles' menu.")
                  (define-key menu-bar-search-menu [icicles]
                    (list 'menu-item "Icicles" icicle-search-menu-map :visible 'icicle-mode))
                  (defvar icicle-search-tags-menu-map (make-sparse-keymap)
                    "`Search' > `Icicles' > `Emacs Tags' menu.")
                  (define-key icicle-search-menu-map [emacs-tags]
                    (list 'menu-item "Emacs Tags" icicle-search-tags-menu-map
                          :visible 'icicle-mode)))))
          (t
           (defvar icicle-search-menu-map (make-sparse-keymap)
             "`Icicles' > `Search' menu.")
           (define-key icicle-menu-map [search]
             (list 'menu-item "Search" icicle-search-menu-map))
           (defvar icicle-search-tags-menu-map (make-sparse-keymap)
             "`Icicles' > `Search' > `Emacs Tags' menu.")
           (define-key icicle-search-menu-map [emacs-tags]
             (list 'menu-item "Emacs Tags" icicle-search-tags-menu-map :visible 'icicle-mode))))

    ;; `Go To' -------------------------------------------------------
    (cond ((not icicle-touche-pas-aux-menus-flag)
           (cond ((boundp 'menu-bar-goto-menu)
                  (defvar icicle-search-goto-menu-map (make-sparse-keymap)
                    "`Go To' > `Icicles' menu.")
                  (define-key menu-bar-goto-menu [icicles]
                    (list 'menu-item "Icicles" icicle-search-goto-menu-map)))
                 (t
                  (defvar icicle-search-goto-menu-map (make-sparse-keymap)
                    "Icicles `Go To' submenu of `Search' menu.")
                  (define-key icicle-search-menu-map [goto]
                    (list 'menu-item "Go To" icicle-search-goto-menu-map)))))
          (t
           (defvar icicle-search-goto-menu-map (make-sparse-keymap)
             "`Icicles' > `Go To' menu.")
           (define-key icicle-menu-map [search]
             (list 'menu-item "Go To" icicle-search-menu-map))))

    (define-key icicle-search-goto-menu-map [icicle-goto-global-marker]
      '(menu-item "+ Global Marker..." icicle-goto-global-marker
        :enable (consp (icicle-markers global-mark-ring)) :keys "C-- C-x C-SPC"
        :help "Go to a global marker, choosing it by the line that includes it"))
    (define-key icicle-search-goto-menu-map [icicle-goto-marker]
      '(menu-item "+ Marker..." icicle-goto-marker
        :enable (mark t) :keys "C-- C-SPC"
        :help "Go to a marker in this buffer, choosing it by the line that includes it"))
    (define-key icicle-search-goto-menu-map [icicle-select-bookmarked-region]
      '(menu-item "+ Select Bookmarked Region..." icicle-select-bookmarked-region
        :enable (featurep 'bookmark+) :keys "C-u C-x C-x"
        :help "Jump to a bookmarked region in other window, and select (activate) it"))

    ;; `Go To' > `Definition' menu.
    (defvar icicle-search-goto-imenu-menu-map (make-sparse-keymap)
      "Icicles `Definition' submenu of `Go To' menu.")
    (define-key icicle-search-goto-menu-map [imenu]
      (list 'menu-item "Definition" icicle-search-goto-imenu-menu-map
            :visible 'imenu-generic-expression))

    (define-key icicle-search-goto-imenu-menu-map [icicle-imenu-key-explicit-map]
      '(menu-item "+ Key in Map..." icicle-imenu-key-explicit-map
        :enable (and imenu-generic-expression  (eq major-mode 'emacs-lisp-mode))
        :help "Go to a key definition in some map, using `icicle-search'"))
    (define-key icicle-search-goto-imenu-menu-map [icicle-imenu-key-implicit-map]
      '(menu-item "+ Key..." icicle-imenu-key-implicit-map
        :enable (and imenu-generic-expression  (eq major-mode 'emacs-lisp-mode))
        :help "Go to a (global) key definition using `icicle-search'"))
    (define-key icicle-search-goto-imenu-menu-map [icicle-imenu-variable]
      '(menu-item "+ Variable..." icicle-imenu-variable
        :enable (and imenu-generic-expression  (eq major-mode 'emacs-lisp-mode))
        :help "Go to a variable definition using `icicle-search'"))
    (define-key icicle-search-goto-imenu-menu-map [icicle-imenu-user-option]
      '(menu-item "+ User Option..." icicle-imenu-user-option
        :enable (and imenu-generic-expression  (eq major-mode 'emacs-lisp-mode))
        :help "Go to an option definition using `icicle-search'"))
    (define-key icicle-search-goto-imenu-menu-map [icicle-imenu-face]
      '(menu-item "+ Face..." icicle-imenu-face
        :enable (and imenu-generic-expression  (eq major-mode 'emacs-lisp-mode))
        :help "Go to a face definition using `icicle-search'"))
    (define-key icicle-search-goto-imenu-menu-map [icicle-imenu-macro]
      '(menu-item "+ Macro..." icicle-imenu-macro
        :enable (and imenu-generic-expression  (eq major-mode 'emacs-lisp-mode))
        :help "Go to a Lisp macro definition using `icicle-search'"))
    (define-key icicle-search-goto-imenu-menu-map [icicle-imenu-non-interactive-function]
      '(menu-item "+ Non-Interactive Function..." icicle-imenu-non-interactive-function
        :enable (and imenu-generic-expression  (eq major-mode 'emacs-lisp-mode))
        :help "Go to a non-command function definition using `icicle-search'"))
    (define-key icicle-search-goto-imenu-menu-map [icicle-imenu-command]
      '(menu-item "+ Command..." icicle-imenu-command
        :enable (and imenu-generic-expression  (eq major-mode 'emacs-lisp-mode))
        :help "Go to a command definition using `icicle-search'"))
    (define-key icicle-search-goto-imenu-menu-map [icicle-imenu]
      '(menu-item "+ Any..." icicle-imenu
        :enable imenu-generic-expression :help "Go to a definition using `icicle-search'"))

    ;; `Search' > `Emacs Tags' (or `Tags') menu
    (define-key icicle-search-tags-menu-map [icicle-tags-search]
      '(menu-item "+ Search Tagged Files ..." icicle-tags-search
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Search all source files listed in tags tables for matches for a regexp"))
    (define-key icicle-search-tags-menu-map [icicle-pop-tag-mark]
      '(menu-item "+ Back (Pop Tag Mark)" icicle-pop-tag-mark
        :enable (and (boundp 'find-tag-marker-ring)
                 (not (ring-empty-p find-tag-marker-ring))
                 (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))
        :help "Pop back to where `M-.' was last invoked"))
    (define-key icicle-search-tags-menu-map [icicle-find-first-tag-other-window]
      '(menu-item "+ Find First Tag ..." icicle-find-first-tag-other-window
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Find first tag in current tags table whose name matches your input"))
    (define-key icicle-search-tags-menu-map [icicle-find-tag]
      '(menu-item "+ Find Tag ..." icicle-find-tag
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Navigate among all tags that match a regexp"))

    ;; `Search' menu
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
    (define-key icicle-search-menu-map [icicle-tags-search]
      '(menu-item "+ Search Tagged Files ..." icicle-tags-search
        :help "Search all source files listed in tags tables for matches for a regexp"))
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

    ;; `Search' > `Bookmarks' menu.
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


    ;; `Bookmarks' ---------------------------------------------------
    (require 'bookmark)                 ; `bookmark-buffer-name' is not autoloaded.
    (cond ((not icicle-touche-pas-aux-menus-flag)
           (cond ((boundp 'bmkp-jump-menu)

                  ;; Bookmark+: `Bookmarks' > `Jump To'.
                  ;; Use `copy-keymap' so that turning off Icicle mode restores the ordinary commands.
                  (defvar icicle-bookmark-menu-map (copy-keymap bmkp-jump-menu)
                    "`Bookmarks' > `Jump To' > `Icicles' submenu.")

                  ;; Bookmark+: `Bookmarks' > `Jump To' > `With Tags'.
                  ;; Use `copy-keymap' so that turning off Icicle mode restores the ordinary commands.
                  (defvar icicle-bookmark-with-tags-menu-map (copy-keymap bmkp-jump-tags-menu)
                    "`Bookmarks' > `Jump To' > `With Tags' > `Icicles' submenu."))
                 (t                     ; Vanilla `bookmark.el' only, not Bookmark+.
                  (defvar icicle-bookmark-menu-map (make-sparse-keymap)
                    "`Bookmarks' > `Icicles' submenu.")
                  (define-key menu-bar-bookmark-map [icicles]
                    (list 'menu-item "Icicles" icicle-bookmark-menu-map :visible 'icicle-mode))
                  (defvar icicle-bookmark-with-tags-menu-map (make-sparse-keymap)
                    "For tags commands on `icicle-bookmark-menu-map'.")
                  (setq icicle-bookmark-with-tags-menu-map  icicle-bookmark-menu-map))))
          (t
           (defvar icicle-bookmark-menu-map (make-sparse-keymap)
             "`Icicles' > `Bookmarks' submenu.")
           (define-key icicle-menu-map [bookmarks]
             (list 'menu-item "Bookmarks" icicle-bookmark-menu-map))
           (defvar icicle-bookmark-with-tags-menu-map (make-sparse-keymap)
             "For tags commands on `icicle-bookmark-menu-map'.")
           (setq icicle-bookmark-with-tags-menu-map  icicle-bookmark-menu-map)))

    (when (featurep 'bookmark+)
      (define-key icicle-bookmark-with-tags-menu-map
          [bmkp-file-this-dir-all-tags-regexp-jump-other-window]
        '(menu-item "File This Dir, All Tags Matching Regexp..."
          icicle-bookmark-file-this-dir-all-tags-regexp-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a file bookmark for this dir, where each tag matches a regexp"))
      (define-key icicle-bookmark-with-tags-menu-map
          [bmkp-file-this-dir-some-tags-regexp-jump-other-window]
        '(menu-item "File This Dir, Any Tag Matching Regexp..."
          icicle-bookmark-file-this-dir-some-tags-regexp-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a file bookmark for this dir, where some tag matches a regexp"))
      (define-key icicle-bookmark-with-tags-menu-map [bmkp-file-this-dir-all-tags-jump-other-window]
        '(menu-item "File This Dir, All Tags in Set..."
          icicle-bookmark-file-this-dir-all-tags-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a file bookmark for this dir, which has all of a set of tags"))
      (define-key icicle-bookmark-with-tags-menu-map [bmkp-file-this-dir-some-tags-jump-other-window]
        '(menu-item "File This Dir, Any Tag in Set..."
          icicle-bookmark-file-this-dir-some-tags-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a file bookmark for this dir, which has some of a set of tags"))
      (define-key icicle-bookmark-with-tags-menu-map [bmkp-file-all-tags-regexp-jump-other-window]
        '(menu-item "File, All Tags Matching Regexp..."
          icicle-bookmark-file-all-tags-regexp-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a file or dir bookmark where each tag matches a regexp"))
      (define-key icicle-bookmark-with-tags-menu-map [bmkp-file-some-tags-regexp-jump-other-window]
        '(menu-item "File, Any Tag Matching Regexp..."
          icicle-bookmark-file-some-tags-regexp-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a file or dir bookmark where at least one tag matches a regexp"))
      (define-key icicle-bookmark-with-tags-menu-map [bmkp-file-all-tags-jump-other-window]
        '(menu-item "File, All Tags in Set..." icicle-bookmark-file-all-tags-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a file or dir bookmark that has all of a set of tags"))
      (define-key icicle-bookmark-with-tags-menu-map [bmkp-file-some-tags-jump-other-window]
        '(menu-item "File, Any Tag in Set..." icicle-bookmark-file-some-tags-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a file or dir bookmark that has some of a set of tags"))
      (define-key icicle-bookmark-with-tags-menu-map [bmkp-autofile-all-tags-regexp-jump-other-window]
        '(menu-item "Autofile, All Tags Matching Regexp..."
          icicle-bookmark-autofile-all-tags-regexp-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to an autofile bookmark where each tag matches a regexp"))
      (define-key icicle-bookmark-with-tags-menu-map [bmkp-autofile-some-tags-regexp-jump-other-window]
        '(menu-item "Autofile, Any Tag Matching Regexp..."
          icicle-bookmark-autofile-some-tags-regexp-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to an autofile bookmark where at least one tag matches a regexp"))
      (define-key icicle-bookmark-with-tags-menu-map [bmkp-autofile-all-tags-jump-other-window]
        '(menu-item "Autofile, All Tags in Set..."
          icicle-bookmark-autofile-all-tags-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to an autofile bookmark that has all of a set of tags"))
      (define-key icicle-bookmark-with-tags-menu-map [bmkp-autofile-some-tags-jump-other-window]
        '(menu-item "Autofile, Any Tag in Set..."
          icicle-bookmark-autofile-some-tags-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to an autofile bookmark that has some of a set of tags"))
      (define-key icicle-bookmark-with-tags-menu-map [bmkp-all-tags-regexp-jump-other-window]
        '(menu-item "All Tags Matching Regexp..." icicle-bookmark-all-tags-regexp-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a bookmark that has each tag matching a regexp that you enter"))
      (define-key icicle-bookmark-with-tags-menu-map [bmkp-some-tags-regexp-jump-other-window]
        '(menu-item "Any Tag Matching Regexp..." icicle-bookmark-some-tags-regexp-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a bookmark with at least one tag matching a regexp"))
      (define-key icicle-bookmark-with-tags-menu-map [bmkp-all-tags-jump-other-window]
        '(menu-item "All Tags in Set..." icicle-bookmark-all-tags-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a bookmark that has all of a set of tags that you enter"))
      (define-key icicle-bookmark-with-tags-menu-map [bmkp-some-tags-jump-other-window]
        '(menu-item "Any Tag in Set..." icicle-bookmark-some-tags-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a bookmark that has some of a set of tags that you enter"))
      (define-key icicle-bookmark-menu-map [bmkp-temporary-jump-other-window]
        '(menu-item "+ Jump to Temporary Bookmark..." icicle-bookmark-temporary-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a temporary bookmark"))
      (define-key icicle-bookmark-menu-map [bmkp-autofile-jump-other-window]
        '(menu-item "+ Jump to Autofile Bookmark..." icicle-bookmark-autofile-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to an autofile bookmark"))
      (define-key icicle-bookmark-menu-map [bmkp-autonamed-this-buffer-jump]
        '(menu-item "+ Jump to Autonamed Bookmark for This Buffer..."
          icicle-bookmark-autonamed-this-buffer-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to an autonamed bookmark for this buffer"))
      (define-key icicle-bookmark-menu-map [bmkp-autonamed-jump-other-window]
        '(menu-item "+ Jump to Autonamed Bookmark..."
          icicle-bookmark-autonamed-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to an autonamed bookmark"))
      (define-key icicle-bookmark-menu-map [bmkp-specific-files-jump-other-window]
        '(menu-item "+ Jump to Bookmark for Specific Files..."
          icicle-bookmark-specific-files-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to bookmarks for specific files that you choose"))
      (define-key icicle-bookmark-menu-map [bmkp-specific-buffers-jump-other-window]
        '(menu-item "+ Jump to Bookmark for Specific Buffers..."
          icicle-bookmark-specific-buffers-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to bookmarks for specific buffers that you choose"))
      (define-key icicle-bookmark-menu-map [bmkp-this-buffer-jump]
        '(menu-item "+ Jump to Bookmark for This Buffer..."
          icicle-bookmark-this-buffer-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a bookmark for this buffer"))
      (define-key icicle-bookmark-menu-map [bmkp-url-jump-other-window]
        '(menu-item "+ Jump to URL Bookmark..." icicle-bookmark-url-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a URL bookmark"))
      (define-key icicle-bookmark-menu-map [bmkp-gnus-jump-other-window]
        '(menu-item "+ Jump to Gnus Bookmark..." icicle-bookmark-gnus-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a Gnus bookmark"))
      (define-key icicle-bookmark-menu-map [bmkp-man-jump-other-window]
        '(menu-item "+ Jump to `man' Bookmark..." icicle-bookmark-man-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a `man'-page bookmark"))
      (define-key icicle-bookmark-menu-map [bmkp-info-jump-other-window]
        '(menu-item "+ Jump to Info Bookmark..." icicle-bookmark-info-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to an Info bookmark"))
      (define-key icicle-bookmark-menu-map [bmkp-image-jump-other-window]
        '(menu-item "+ Jump to Image Bookmark..."
          icicle-bookmark-image-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to an image bookmark"))
      (define-key icicle-bookmark-menu-map [bmkp-non-file-jump-other-window]
        '(menu-item "+ Jump to Buffer (Non-File) Bookmark..."
          icicle-bookmark-non-file-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a buffer (i.e., a non-file) bookmark"))
      (define-key icicle-bookmark-menu-map [bmkp-region-jump-other-window]
        '(menu-item "+ Jump to Region Bookmark..." icicle-bookmark-region-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a bookmark and activate its recorded region"))
      (define-key icicle-bookmark-menu-map [bmkp-remote-file-jump-other-window]
        '(menu-item "+ Jump to Remote-File Bookmark..."
          icicle-bookmark-remote-file-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a remote-file bookmark"))
      (define-key icicle-bookmark-menu-map [bmkp-local-file-jump-other-window]
        '(menu-item "+ Jump to Local-File Bookmark..."
          icicle-bookmark-local-file-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a local-file bookmark"))
      (define-key icicle-bookmark-menu-map [bmkp-file-jump-other-window]
        '(menu-item "+ Jump to File Bookmark..." icicle-bookmark-file-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a file bookmark"))
      (define-key icicle-bookmark-menu-map [bmkp-dired-jump-other-window]
        '(menu-item "+ Jump to Dired Bookmark..." icicle-bookmark-dired-other-window
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a Dired bookmark"))
      (define-key icicle-bookmark-menu-map [bmkp-bookmark-file-jump]
        '(menu-item "+ Jump to Bookmark-File Bookmark..."
          icicle-bookmark-bookmark-file
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a bookmark-file bookmark"))
      (define-key icicle-bookmark-menu-map [bmkp-bookmark-list-jump]
        '(menu-item "+ Jump to Bookmark-List Bookmark..."
          icicle-bookmark-bookmark-list
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to a bookmark-list bookmark"))
      (define-key icicle-bookmark-menu-map [bmkp-desktop-jump]
        '(menu-item "+ Jump to Desktop Bookmark..." icicle-bookmark-desktop
          :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
          :help "Jump to an Emacs desktop bookmark")))

    (define-key icicle-bookmark-menu-map [bookmark-jump-other-window]
      '(menu-item "+ Jump to Bookmark..." icicle-bookmark-other-window
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Jump to a bookmark (C-u: reverse `icicle-bookmark-refresh-cache-flag')"))
    (define-key icicle-bookmark-menu-map [bookmark-jump]
      '(menu-item "+ Jump to Bookmark (Same Window)..." icicle-bookmark
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
        :help "Jump to a bookmark (C-u: reverse `icicle-bookmark-refresh-cache-flag')"))


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
           (defvar icicle-buffers-menu-map (make-sparse-keymap)
             "`File' > `Icicles' > `Buffers' submenu.")
           (define-key icicle-file-menu-map [buffers]
             (list 'menu-item "Buffers" icicle-buffers-menu-map)))
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
           (if (boundp 'diredp-menu-bar-operate-menu) ; In `dired+.el'.
               (define-key diredp-menu-bar-operate-menu [icicles]
                 (list 'menu-item "Icicles" icicle-dired-multiple-menu-map :visible 'icicle-mode))
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
      (define-key icicle-dired-recursive-marked-menu-map [icicle-search-dired-marked-recursive]
        '(menu-item "Icicles Search (and Replace)..." icicle-search-dired-marked-recursive
          :help "Search the marked files, including those in marked subdirs"))
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
          :help "Save the marked file names in Dired, including those in marked subdirs")))

    ;; `Dired Dirs' ------------------------------------------------
    (cond ((not icicle-touche-pas-aux-menus-flag)
           (defvar icicle-dired-dir-menu-map (make-sparse-keymap)
             "`Icicles' submenu for Dired's `Dir' (or `Subdir') menu.")
           (if (boundp 'diredp-menu-bar-subdir-menu) ; In `dired+.el'.
               (define-key diredp-menu-bar-subdir-menu [icicles]
                 (list 'menu-item "Icicles" icicle-dired-dir-menu-map :visible 'icicle-mode))
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
    (define-key comint-mode-map (icicle-kbd "C-c C-i") 'icicle-comint-command) ; `C-c TAB'
    (define-key comint-mode-map (icicle-kbd "C-c tab") 'icicle-comint-command)) ; `C-c TAB'

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
  (when (and (boundp 'inferior-tcl-mode-map)  (memq 'comint-dynamic-complete
                                                    icicle-functions-to-redefine))
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
    (unless (lookup-key dired-mode-map (icicle-kbd "C-S-o")) ; Dired `C-S-o', aka `C-O'
      (define-key dired-mode-map (icicle-kbd "C-S-o")
        'icicle-visit-marked-file-of-content-other-window)))

  ;; More Dired keys, but these require `dired+.el'.
  (when (boundp 'diredp-recursive-map)
    (let* ((key  (apply 'vector         ; `M-s M-s m'
                        (append (listify-key-sequence icicle-search-key-prefix)
                                (listify-key-sequence (icicle-kbd "m")))))
           (def  (lookup-key dired-mode-map key)))
      (unless (and def  (not (integerp def)))
        (define-key dired-mode-map key 'icicle-search-dired-marked-recursive)))
    (define-key diredp-recursive-map (icicle-kbd "M-s M-s") ; `M-+ M-s M-s'
      'icicle-search-dired-marked-recursive)
    (define-key diredp-recursive-map (icicle-kbd "C-{") ; `M-+ C-{'
      'icicle-dired-project-other-window)
    (define-key diredp-recursive-map (icicle-kbd "C-M->") ; `M-+ C-M->'
      'icicle-dired-save-marked-recursive)
    (define-key diredp-recursive-map (icicle-kbd "C->") ; `M-+ C->'
      'icicle-dired-save-marked-more-recursive)
    (define-key diredp-recursive-map (icicle-kbd "C-M-}") ; `M-+ C-M-}'
      'icicle-dired-save-marked-to-variable-recursive)
    (define-key diredp-recursive-map (icicle-kbd "C-}") ; `M-+ C-}'
      'icicle-dired-save-marked-to-cache-file-recursive))

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

  ;; Unbind keys in Shell mode.
  (when (and (boundp 'shell-mode-map)  (memq 'icicle-comint-dynamic-complete
                                             icicle-functions-to-redefine))
    (define-key shell-mode-map (icicle-kbd "C-i") (if (> emacs-major-version 23)
                                                        'completion-at-point
                                                      'comint-dynamic-complete)))

  ;; Unbind keys in Shell Script mode.
  (when (and (boundp 'sh-mode-map)  (memq 'icicle-comint-dynamic-complete
                                          icicle-functions-to-redefine))
    (icicle-unmap 'comint-dynamic-complete sh-mode-map 'icicle-comint-dynamic-complete))

  ;; Unbind keys in Ielm mode.
  (when (and (boundp 'ielm-map)  (memq 'icicle-comint-dynamic-complete
                                       icicle-functions-to-redefine))
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

;;;###autoload (autoload 'icicle-skip-this-command "icicles")
(defun icicle-skip-this-command ()
  "Prevent `handle-switch-frame' from being added to `this-command'."
  (interactive)
  (setq this-command  last-command))

;;;###autoload (autoload 'icicle-handle-switch-frame "icicles")
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
       (define-key map [menu-bar minibuf quit] ; Replace `keyboard-escape-quit'
         '(menu-item "Quit" icicle-abort-recursive-edit
           :help "Cancel minibuffer input or recursive edit"))
       (define-key map [menu-bar minibuf return]
         '(menu-item "Enter" exit-minibuffer
           :help "Terminate input and exit minibuffer" :keys "RET"))
       (define-key map [menu-bar minibuf separator-help] '("--"))

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
       (define-key map [menu-bar minibuf icicle-regexp-quote-input]
         '(menu-item "Regexp-Quote Input" icicle-regexp-quote-input
           :enable (with-current-buffer (window-buffer (minibuffer-window))
                     (not (zerop (buffer-size))))
           :help "Regexp-quote current input or its active region, then apropos-complete"
           :keys "M-%"))
       (define-key map [menu-bar minibuf separator-set2] '("--"))

       (define-key map [menu-bar minibuf icicle-clear-current-history]
         '(menu-item "Clear History Entries" icicle-clear-current-history
           :help "Clear current minibuffer history of selected entries"))
       (define-key map [menu-bar minibuf icicle-erase-minibuffer]
         '(menu-item "Delete from History" icicle-erase-minibuffer-or-history-element
           :visible (memq last-command
                     '(previous-history-element next-history-element
                       icicle-erase-minibuffer-or-history-element
                       previous-matching-history-element next-matching-history-element))
           :help "Delete current history element (in minibuffer now)" :keys "M-k"))
       (define-key map [menu-bar minibuf icicle-delete-history-element]
         '(menu-item "Clear (Erase) Minibuffer" icicle-erase-minibuffer-or-history-element
           :visible (not (memq last-command
                          '(previous-history-element next-history-element
                            icicle-erase-minibuffer-or-history-element
                            previous-matching-history-element next-matching-history-element)))
           :help "Erase the Minibuffer" :keys "M-k"))
       (define-key map [menu-bar minibuf icicle-insert-list-join-string]
         '(menu-item "Insert Join-String" icicle-insert-list-join-string
           :help "Insert `icicle-list-join-string' into the minibuffer"))
       (define-key map [menu-bar minibuf icicle-insert-key-description]
         '(menu-item "Insert Key Description" icicle-insert-key-description
           :visible (not icicle-searching-p) :keys "M-q"
           :help "Read key and insert its description - e.g., reading ^F inserts `C-f'"))
       (define-key map [menu-bar minibuf icicle-insert-history-element]
         '(menu-item "Insert Past Input using Completion" icicle-insert-history-element
           :enable (consp (symbol-value minibuffer-history-variable))
           :help "Use completion to insert a previous input into the minibuffer"))
       (define-key map [menu-bar minibuf icicle-insert-string-from-a-var]
         '(menu-item "Insert String from a Variable..." icicle-insert-string-from-variable
           :visible current-prefix-arg :keys "C-="
           :help "Read a variable name and insert its string value into the minibuffer"))
       (define-key map [menu-bar minibuf icicle-insert-string-from-std-var]
         '(menu-item "Insert `icicle-input-string'" icicle-insert-string-from-variable
           :visible (not current-prefix-arg) :keys "C-="
           :help "Insert text from variable `icicle-input-string' into the minibuffer"))
       (define-key map [menu-bar minibuf icicle-insert-string-at-point]
         '(menu-item "Insert Text from Point" icicle-insert-string-at-point
           :help "Insert text at or near the cursor into the minibuffer"))
       (define-key map [menu-bar minibuf icicle-completing-read+insert]
         '(menu-item "Insert On-Demand Completion" icicle-completing-read+insert
           :visible (consp icicle-completing-read+insert-candidates)
           :help "Read and insert something using (lax) completion"))
       (define-key map [menu-bar minibuf icicle-read+insert-file-name]
         '(menu-item "Insert File Name" icicle-read+insert-file-name
           :help "Read and insert a file name using (lax) completion"))

       ;; $$$$$$ Keep `C-?' also for a while, undocumented, for backward compatibility only.
       (define-key map (icicle-kbd "C-?")           'icicle-minibuffer-help) ; `C-?'
       (define-key map (icicle-kbd "M-?")           'icicle-minibuffer-help) ; `M-?'
       (define-key map (icicle-kbd "C-g")           'icicle-abort-recursive-edit) ; `C-g'
       (define-key map (icicle-kbd "M-S-backspace") 'icicle-erase-minibuffer) ; `M-S-backspace'
       (define-key map (icicle-kbd "M-S-delete")    'icicle-erase-minibuffer) ; `M-S-delete'
       (define-key map (icicle-kbd "M-.")           'icicle-insert-string-at-point) ; `M-.'
       (define-key map (icicle-kbd "C-x C-f")       'icicle-resolve-file-name) ; `C-x C-f'
       (define-key map (icicle-kbd "C-=")           'icicle-insert-string-from-variable) ; `C-='
       (define-key map (icicle-kbd "M-i")           'icicle-clear-current-history) ; `M-i'
       (define-key map (icicle-kbd "M-k")          'icicle-erase-minibuffer-or-history-element) ; `M-k'
       (define-key map (icicle-kbd "M-o")           'icicle-insert-history-element) ; `M-o'
       (define-key map (icicle-kbd "M-:")           'icicle-pp-eval-expression-in-minibuffer) ; `M-:'
       (define-key map (icicle-kbd "C-a")           'icicle-beginning-of-line+) ; `C-a'
       (define-key map (icicle-kbd "C-e")           'icicle-end-of-line+) ; `C-e'
       (define-key map (icicle-kbd "C-M-v")         'icicle-scroll-forward) ; `C-M-v'
       (define-key map (icicle-kbd "C-M-S-v")       'icicle-scroll-backward) ; `C-M-S-v' (aka `C-M-V')
       (define-key map (icicle-kbd "C-M-pause")     'icicle-other-history) ; `C-M-pause'
       (dolist (key  icicle-completing-read+insert-keys)
         (define-key map key                        'icicle-completing-read+insert)) ; `C-M-S-c'
       (dolist (key  icicle-read+insert-file-name-keys)
         (define-key map key                        'icicle-read+insert-file-name)) ; `C-M-S-f'
       (define-key map (icicle-kbd "C-j")           'icicle-insert-newline-in-minibuffer) ; `C-j'
       (when (fboundp 'icicle-yank-secondary)
         (define-key map (icicle-kbd "C-M-y") 'icicle-yank-secondary))) ; `C-M-y'

     ;; `minibuffer-local-ns-map': default minibuffer map when spaces are not allowed.
     ;; In Emacs 22+, local is parent of local-ns.
     (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-ns-map))
       (let ((map  minibuffer-local-ns-map))
         (define-key map [menu-bar minibuf quit] ; Replace `keyboard-escape-quit'
           '(menu-item "Quit" icicle-abort-recursive-edit
             :help "Cancel minibuffer input or recursive edit"))
         (define-key map [menu-bar minibuf return]
           '(menu-item "Enter" exit-minibuffer
             :help "Terminate input and exit minibuffer" :keys "RET"))
         (define-key map [menu-bar minibuf separator-help] '("--"))

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
         (define-key map [menu-bar minibuf icicle-regexp-quote-input]
           '(menu-item "Regexp-Quote Input" icicle-regexp-quote-input
             :enable (with-current-buffer (window-buffer (minibuffer-window))
                       (not (zerop (buffer-size))))
             :help "Regexp-quote current input or its active region, then apropos-complete"
             :keys "M-%"))
         (define-key map [menu-bar minibuf separator-set2] '("--"))

         (define-key map [menu-bar minibuf icicle-clear-current-history]
           '(menu-item "Clear History Entries" icicle-clear-current-history
             :help "Clear current minibuffer history of selected entries"))
         (define-key map [menu-bar minibuf icicle-erase-minibuffer]
           '(menu-item "Delete from History" icicle-erase-minibuffer-or-history-element
             :visible (memq last-command
                       '(previous-history-element next-history-element
                         icicle-erase-minibuffer-or-history-element
                         previous-matching-history-element next-matching-history-element))
             :help "Delete current history element (in minibuffer now)" :keys "M-k"))
         (define-key map [menu-bar minibuf icicle-delete-history-element]
           '(menu-item "Clear (Erase) Minibuffer" icicle-erase-minibuffer-or-history-element
             :visible (not (memq last-command
                            '(previous-history-element next-history-element
                              icicle-erase-minibuffer-or-history-element
                              previous-matching-history-element next-matching-history-element)))
             :help "Erase the Minibuffer" :keys "M-k"))
         (define-key map [menu-bar minibuf icicle-insert-list-join-string]
           '(menu-item "Insert Join-String" icicle-insert-list-join-string
             :help "Insert `icicle-list-join-string' into the minibuffer"))
         (define-key map [menu-bar minibuf icicle-insert-key-description]
           '(menu-item "Insert Key Description" icicle-insert-key-description
             :visible (not icicle-searching-p) :keys "M-q"
             :help "Read key and insert its description - e.g., reading ^F inserts `C-f'"))
         (define-key map [menu-bar minibuf icicle-insert-history-element]
           '(menu-item "Insert Past Input using Completion" icicle-insert-history-element
             :enable (consp (symbol-value minibuffer-history-variable))
             :help "Use completion to insert a previous input into the minibuffer"))
         (define-key map [menu-bar minibuf icicle-insert-string-from-a-var]
           '(menu-item "Insert String from a Variable..." icicle-insert-string-from-variable
             :visible current-prefix-arg :keys "C-="
             :help "Read a variable name and insert its string value into the minibuffer"))
         (define-key map [menu-bar minibuf icicle-insert-string-from-std-var]
           '(menu-item "Insert `icicle-input-string'" icicle-insert-string-from-variable
             :visible (not current-prefix-arg) :keys "C-="
             :help "Insert text from variable `icicle-input-string' into the minibuffer"))
         (define-key map [menu-bar minibuf icicle-insert-string-at-point]
           '(menu-item "Insert Text from Point" icicle-insert-string-at-point
             :help "Insert text at or near the cursor into the minibuffer"))
         (define-key map [menu-bar minibuf icicle-completing-read+insert]
           '(menu-item "Insert On-Demand Completion" icicle-completing-read+insert
             :visible (consp icicle-completing-read+insert-candidates)
             :help "Read and insert something using (lax) completion"))
         (define-key map [menu-bar minibuf icicle-read+insert-file-name]
           '(menu-item "Insert File Name" icicle-read+insert-file-name
             :help "Read and insert a file name using (lax) completion"))

         ;; $$$$$$ Keep `C-?' also for a while, undocumented, for backward compatibility only.
         (define-key map (icicle-kbd "C-?")           'icicle-minibuffer-help) ; `C-?'
         (define-key map (icicle-kbd "M-?")           'icicle-minibuffer-help) ; `M-?'
         (define-key map (icicle-kbd "C-g")           'icicle-abort-recursive-edit) ; `C-g'
         (define-key map (icicle-kbd "M-S-backspace") 'icicle-erase-minibuffer) ; `M-S-backspace'
         (define-key map (icicle-kbd "M-S-delete")    'icicle-erase-minibuffer) ; `M-S-delete'
         (define-key map (icicle-kbd "M-.")           'icicle-insert-string-at-point) ; `M-.'
         (define-key map (icicle-kbd "C-x C-f")       'icicle-resolve-file-name) ; `C-x C-f'
         (define-key map (icicle-kbd "C-=")           'icicle-insert-string-from-variable) ; `C-='
         (define-key map (icicle-kbd "M-i")           'icicle-clear-current-history) ; `M-i'
         (define-key map (icicle-kbd "M-k")    'icicle-erase-minibuffer-or-history-element) ; `M-k'
         (define-key map (icicle-kbd "M-o")           'icicle-insert-history-element) ; `M-o'
         (define-key map (icicle-kbd "M-:")       'icicle-pp-eval-expression-in-minibuffer) ; `M-:'
         (define-key map (icicle-kbd "C-a")           'icicle-beginning-of-line+) ; `C-a'
         (define-key map (icicle-kbd "C-e")           'icicle-end-of-line+) ; `C-e'
         (define-key map (icicle-kbd "C-M-v")         'icicle-scroll-forward) ; `C-M-v'
         (define-key map (icicle-kbd "C-M-S-v")  'icicle-scroll-backward) ; `C-M-S-v' (aka `C-M-V')
         (define-key map (icicle-kbd "C-M-pause")     'icicle-other-history) ; `C-M-pause'
         (dolist (key  icicle-completing-read+insert-keys)
           (define-key map key 'icicle-completing-read+insert)) ; `C-M-S-c'
         (dolist (key  icicle-read+insert-file-name-keys)
           (define-key map key 'icicle-read+insert-file-name)) ; `C-M-S-f'
         (define-key map (icicle-kbd "C-j")           'icicle-insert-newline-in-minibuffer) ; `C-j'
         (when (fboundp 'icicle-yank-secondary)
           (define-key map (icicle-kbd "C-M-y") 'icicle-yank-secondary)))) ; `C-M-y'

     ;; `minibuffer-local-isearch-map': minibuffer map for editing isearch strings.
     ;; In Emacs 21+, local is parent of local-isearch.
     (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-isearch-map))
       (let ((map  minibuffer-local-isearch-map))
         (define-key map [menu-bar minibuf quit] ; Replace `keyboard-escape-quit'
           '(menu-item "Quit" icicle-abort-recursive-edit
             :help "Cancel minibuffer input or recursive edit"))
         (define-key map [menu-bar minibuf return]
           '(menu-item "Enter" exit-minibuffer
             :help "Terminate input and exit minibuffer" :keys "RET"))
         (define-key map [menu-bar minibuf separator-help] '("--"))

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
         (define-key map [menu-bar minibuf icicle-regexp-quote-input]
           '(menu-item "Regexp-Quote Input" icicle-regexp-quote-input
             :enable (with-current-buffer (window-buffer (minibuffer-window))
                       (not (zerop (buffer-size))))
             :help "Regexp-quote current input or its active region, then apropos-complete"
             :keys "M-%"))
         (define-key map [menu-bar minibuf separator-set2] '("--"))

         (define-key map [menu-bar minibuf icicle-clear-current-history]
           '(menu-item "Clear History Entries" icicle-clear-current-history
             :help "Clear current minibuffer history of selected entries"))
         (define-key map [menu-bar minibuf icicle-erase-minibuffer]
           '(menu-item "Delete from History" icicle-erase-minibuffer-or-history-element
             :visible (memq last-command
                       '(previous-history-element next-history-element
                         icicle-erase-minibuffer-or-history-element
                         previous-matching-history-element next-matching-history-element))
             :help "Delete current history element (in minibuffer now)" :keys "M-k"))
         (define-key map [menu-bar minibuf icicle-delete-history-element]
           '(menu-item "Clear (Erase) Minibuffer" icicle-erase-minibuffer-or-history-element
             :visible (not (memq last-command
                            '(previous-history-element next-history-element
                              icicle-erase-minibuffer-or-history-element
                              previous-matching-history-element next-matching-history-element)))
             :help "Erase the Minibuffer" :keys "M-k"))
         (define-key map [menu-bar minibuf icicle-insert-list-join-string]
           '(menu-item "Insert Join-String" icicle-insert-list-join-string
             :help "Insert `icicle-list-join-string' into the minibuffer"))
         (define-key map [menu-bar minibuf icicle-insert-key-description]
           '(menu-item "Insert Key Description" icicle-insert-key-description
             :visible (not icicle-searching-p) :keys "M-q"
             :help "Read key and insert its description - e.g., reading ^F inserts `C-f'"))
         (define-key map [menu-bar minibuf icicle-insert-history-element]
           '(menu-item "Insert Past Input using Completion" icicle-insert-history-element
             :enable (consp (symbol-value minibuffer-history-variable))
             :help "Use completion to insert a previous input into the minibuffer"))
         (define-key map [menu-bar minibuf icicle-insert-string-from-a-var]
           '(menu-item "Insert String from a Variable..." icicle-insert-string-from-variable
             :visible current-prefix-arg :keys "C-="
             :help "Read a variable name and insert its string value into the minibuffer"))
         (define-key map [menu-bar minibuf icicle-insert-string-from-std-var]
           '(menu-item "Insert `icicle-input-string'" icicle-insert-string-from-variable
             :visible (not current-prefix-arg) :keys "C-="
             :help "Insert text from variable `icicle-input-string' into the minibuffer"))
         (define-key map [menu-bar minibuf icicle-insert-string-at-point]
           '(menu-item "Insert Text from Point" icicle-insert-string-at-point
             :help "Insert text at or near the cursor into the minibuffer"))
         (define-key map [menu-bar minibuf icicle-completing-read+insert]
           '(menu-item "Insert On-Demand Completion" icicle-completing-read+insert
             :visible (consp icicle-completing-read+insert-candidates)
             :help "Read and insert something using (lax) completion"))
         (define-key map [menu-bar minibuf icicle-read+insert-file-name]
           '(menu-item "Insert File Name" icicle-read+insert-file-name
             :help "Read and insert a file name using (lax) completion"))

         ;; $$$$$$ Keep `C-?' also for a while, undocumented, for backward compatibility only.
         (define-key map (icicle-kbd "C-?")           'icicle-minibuffer-help) ; `C-?'
         (define-key map (icicle-kbd "M-?")           'icicle-minibuffer-help) ; `M-?'
         (define-key map (icicle-kbd "C-g")           'icicle-abort-recursive-edit) ; `C-g'
         (define-key map (icicle-kbd "M-S-backspace") 'icicle-erase-minibuffer) ; `M-S-backspace'
         (define-key map (icicle-kbd "M-S-delete")    'icicle-erase-minibuffer) ; `M-S-delete'
         (define-key map (icicle-kbd "M-.")           'icicle-insert-string-at-point) ; `M-.'
         (define-key map (icicle-kbd "C-x C-f")       'icicle-resolve-file-name) ; `C-x C-f'
         (define-key map (icicle-kbd "C-=")           'icicle-insert-string-from-variable) ; `C-='
         (define-key map (icicle-kbd "M-i")           'icicle-clear-current-history) ; `M-i'
         (define-key map (icicle-kbd "M-o")           'icicle-insert-history-element) ; `M-o'
         (define-key map (icicle-kbd "M-k")    'icicle-erase-minibuffer-or-history-element) ; `M-k'
         (define-key map (icicle-kbd "M-:")       'icicle-pp-eval-expression-in-minibuffer) ; `M-:'
         (define-key map (icicle-kbd "C-a")           'icicle-beginning-of-line+) ; `C-a'
         (define-key map (icicle-kbd "C-e")           'icicle-end-of-line+) ; `C-e'
         (define-key map (icicle-kbd "C-M-v")         'icicle-scroll-forward) ; `C-M-v'
         (define-key map (icicle-kbd "C-M-S-v")  'icicle-scroll-backward) ; `C-M-S-v' (aka `C-M-V')
         (define-key map (icicle-kbd "C-M-pause")     'icicle-other-history) ; `C-M-pause'
         (dolist (key  icicle-completing-read+insert-keys)
           (define-key map key 'icicle-completing-read+insert)) ; `C-M-S-c'
         (dolist (key  icicle-read+insert-file-name-keys)
           (define-key map key 'icicle-read+insert-file-name)) ; `C-M-S-f'
         (define-key map (icicle-kbd "C-j")           'icicle-insert-newline-in-minibuffer) ; `C-j'
         (when (fboundp 'icicle-yank-secondary)
           (define-key map (icicle-kbd "C-M-y") 'icicle-yank-secondary)))) ; `C-M-y'

     ;; `minibuffer-local-completion-map': completion map.
     (icicle-bind-completion-keys minibuffer-local-completion-map)

     ;; `minibuffer-local-must-match-map': must-match map.
     ;; In Emacs 22+, local-completion is parent of local-must-match
     (if (not (eq minibuffer-local-completion-map (keymap-parent minibuffer-local-must-match-map)))
         (icicle-bind-completion-keys minibuffer-local-must-match-map)
       (define-key minibuffer-local-must-match-map (icicle-kbd "C-g") ; `C-g'
         'icicle-abort-recursive-edit)  ; `C-g' - need it anyway, even if inherit completion map.
       (dolist (key  icicle-completing-read+insert-keys)
         (define-key minibuffer-local-must-match-map key 'icicle-completing-read+insert)) ; `C-M-S-c'
       (dolist (key  icicle-read+insert-file-name-keys)
         (define-key minibuffer-local-must-match-map key 'icicle-read+insert-file-name)) ; `C-M-S-f'
       ;; Override the binding of `C-j' to `minibuffer-complete-and-exit'.
       (define-key minibuffer-local-must-match-map (icicle-kbd "C-j") ; `C-j' (newline)
         'icicle-insert-newline-in-minibuffer))

     ;; `completion-list-mode-map': map for `*Completions*' buffer.
     ;; Abort on `C-g' or `q'.  Switch to minibuffer on `C-insert'.  Do not allow normal input.
     (let ((map  completion-list-mode-map))
       (dolist (key  icicle-candidate-help-keys) ; `C-M-return', `C-help', `C-M-help', `C-f1',
         (define-key map key 'icicle-help-on-candidate)) ; `C-M-f1'
       ;; $$$$$$ Keep `C-?' also for a while, undocumented, for backward compatibility only.
       (define-key map (icicle-kbd "C-?")      'icicle-minibuffer-help) ; `C-?'
       (define-key map (icicle-kbd "M-?")      'icicle-minibuffer-help) ; `M-?'
       (define-key map (icicle-kbd "C-g")      'icicle-abort-recursive-edit) ; `C-g'
       (define-key map (icicle-kbd "q")        'icicle-abort-recursive-edit) ; `q'
       (define-key map (icicle-kbd "C-insert") 'icicle-insert-completion) ; `C-insert'
       (define-key map (icicle-kbd "down")     'icicle-next-line) ; `down'
       (define-key map (icicle-kbd "up")       'icicle-previous-line) ; `up'
       (define-key map (icicle-kbd "right")    'icicle-move-to-next-completion) ; `right'
       (define-key map (icicle-kbd "left")     'icicle-move-to-previous-completion) ; `left'
       (dolist (key icicle-previous-candidate-keys)
         (define-key map key 'icicle-move-to-previous-completion)) ; `S-TAB'
       (define-key map (icicle-kbd "C-i")      'icicle-move-to-next-completion) ; `TAB'
       (define-key map (icicle-kbd "tab")      'icicle-move-to-next-completion) ; `TAB'
       (when (boundp 'mouse-wheel-down-event) ; Emacs 22+ -  `wheel-down', `wheel-up'
         (define-key map (vector mouse-wheel-down-event) 'icicle-scroll-Completions-backward)
         (define-key map (vector mouse-wheel-up-event) 'icicle-scroll-Completions-forward))
       (define-key map (icicle-kbd "S-down-mouse-2") 'icicle-mouse-remove-candidate) ; `S-mouse-2'
       (define-key map (icicle-kbd "S-mouse-2")      'ignore)
       (define-key map (icicle-kbd "C-S-down-mouse-2") ; `C-S-mouse-2'
         'icicle-mouse-candidate-alt-action)
       (define-key map (icicle-kbd "C-S-mouse-2")    'ignore)
       (define-key map (icicle-kbd "C-down-mouse-2") 'icicle-mouse-candidate-action) ; `C-mouse-2'
       (define-key map (icicle-kbd "C-mouse-2")      'ignore)
       (define-key map (icicle-kbd "C-M-down-mouse-2") ; `C-M-mouse-2'
         'icicle-mouse-help-on-candidate)
       (define-key map (icicle-kbd "C-M-mouse-2")    'ignore)
       (define-key map (icicle-kbd "M-S-down-mouse-2") ; `M-S-mouse-2'
         'icicle-mouse-save/unsave-candidate)
       (define-key map (icicle-kbd "M-S-mouse-2")    'ignore)
       (define-key map (icicle-kbd "M-down-mouse-2") ; `M-mouse-2'
         'icicle-mouse-candidate-read-fn-invoke)
       (define-key map (icicle-kbd "M-mouse-2")      'ignore)
       (define-key map (icicle-kbd "C-down-mouse-3") ; `C-mouse-3'
         'icicle-Completions-mouse-3-menu)
       (define-key map (icicle-kbd "C-mouse-3")      'ignore)
       (define-key map (icicle-kbd "M-down-mouse-3") ; `M-mouse-3'
         'icicle-mouse-candidate-set-save-more)
       (define-key map (icicle-kbd "M-mouse-3")      'ignore)
       (define-key map (icicle-kbd "M-S-down-mouse-3") ; `M-S-mouse-3'
         'icicle-mouse-candidate-set-save)
       (define-key map (icicle-kbd "M-S-mouse-3")    'ignore)
       (define-key map (icicle-kbd "mouse-3")        'icicle-mouse-save-then-kill) ; `mouse-3'
       (define-key map (icicle-kbd "C->")            'icicle-candidate-set-save-more) ; `C->'
       (define-key map (icicle-kbd "C-M->")          'icicle-candidate-set-save) ; `C-M->'
       (define-key map (icicle-kbd "C-)")         'icicle-candidate-set-save-more-selected) ; `C-)'
       (define-key map (icicle-kbd "C-M-)")          'icicle-candidate-set-save-selected) ; `C-M-)'
       (define-key map (icicle-kbd "C-M-<")          'icicle-candidate-set-retrieve) ; `C-M-<'
       (define-key map (icicle-kbd "C-l")            'icicle-retrieve-previous-input) ; `C-l'
       (define-key map (icicle-kbd "C-a")            'icicle-beginning-of-line+) ; `C-a'
       (define-key map (icicle-kbd "C-e")            'icicle-end-of-line+) ; `C-e'
       ;; (suppress-keymap map) ; Inhibit character self-insertion.
       ))

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

       ;; $$$$$$ Keep `C-?' also for a while, undocumented, for backward compatibility only.
       (define-key map (icicle-kbd "C-?")           nil) ; `C-?'
       (define-key map (icicle-kbd "M-?")           nil) ; `M-?'
       (define-key map (icicle-kbd "C-g")           (if (and (fboundp 'minibuffer-keyboard-quit)
                                                             delete-selection-mode)
                                                          'minibuffer-keyboard-quit
                                                        'abort-recursive-edit)) ; `C-g'
       (define-key map (icicle-kbd "M-S-backspace") nil) ; `M-S-DEL'
       (define-key map (icicle-kbd "M-S-delete")    nil) ; `M-S-delete'
       (define-key map (icicle-kbd "M-.")           nil) ; `M-.'
       (define-key map (icicle-kbd "C-x C-f")       nil) ; `C-x C-f'
       (define-key map (icicle-kbd "C-=")           nil) ; `C-='
       (define-key map (icicle-kbd "M-i")           nil) ; `M-i'
       (define-key map (icicle-kbd "M-k")           nil) ; `M-k'
       (define-key map (icicle-kbd "M-o")           nil) ; `M-o'
       (define-key map (icicle-kbd "M-:")           nil) ; `M-:'
       (define-key map (icicle-kbd "C-a")           nil) ; `C-a'
       (define-key map (icicle-kbd "C-e")           nil) ; `C-e'
       (define-key map (icicle-kbd "C-M-v")         nil) ; `C-M-v'
       (define-key map (icicle-kbd "C-M-S-v")       nil) ; `C-M-S-v' (aka `C-M-V')
       (define-key map (icicle-kbd "C-M-pause")     nil) ; `C-M-pause'
       (dolist (key  icicle-completing-read+insert-keys) (define-key map key nil)) ; `C-M-S-c'
       (dolist (key  icicle-read+insert-file-name-keys) (define-key map key nil)) ; `C-M-S-f'
       (define-key map (icicle-kbd "C-j")           'exit-minibuffer) ; `C-j'
       (define-key map (icicle-kbd "C-M-y")         nil)) ; `C-M-y'

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

         ;; $$$$$$ Keep `C-?' also for a while, undocumented, for backward compatibility only.
         (define-key map (icicle-kbd "C-?")           nil) ; `C-?'
         (define-key map (icicle-kbd "M-?")           nil) ; `M-?'
         (define-key map (icicle-kbd "C-g")           (if (and (fboundp 'minibuffer-keyboard-quit)
                                                               delete-selection-mode)
                                                            'minibuffer-keyboard-quit
                                                          'abort-recursive-edit)) ; `C-g'
         (define-key map (icicle-kbd "M-S-backspace") nil) ; `M-S-DEL'
         (define-key map (icicle-kbd "M-S-delete")    nil) ; `M-S-delete'
         (define-key map (icicle-kbd "M-.")           nil) ; `M-.'
         (define-key map (icicle-kbd "C-x C-f")       nil) ; `C-x C-f'
         (define-key map (icicle-kbd "C-=")           nil) ; `C-='
         (define-key map (icicle-kbd "M-i")           nil) ; `M-i'
         (define-key map (icicle-kbd "M-k")           nil) ; `M-k'
         (define-key map (icicle-kbd "M-o")           nil) ; `M-o'
         (define-key map (icicle-kbd "M-:")           nil) ; `M-:'
         (define-key map (icicle-kbd "C-a")           nil) ; `C-a'
         (define-key map (icicle-kbd "C-e")           nil) ; `C-e'
         (define-key map (icicle-kbd "C-M-v")         nil) ; `C-M-v'
         (define-key map (icicle-kbd "C-M-S-v")       nil) ; `C-M-S-v' (aka `C-M-V')
         (define-key map (icicle-kbd "C-M-pause")     nil) ; `C-M-pause'
         (dolist (key  icicle-completing-read+insert-keys) (define-key map key nil)) ; `C-M-S-c'
         (dolist (key  icicle-read+insert-file-name-keys) (define-key map key nil)) ; `C-M-S-f'
         (define-key map (icicle-kbd "C-j")           'exit-minibuffer) ; `C-j'
         (define-key map (icicle-kbd "C-M-y")         nil))) ; `C-M-y'

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

         ;; $$$$$$ Keep `C-?' also for a while, undocumented, for backward compatibility only.
         (define-key map (icicle-kbd "C-?")           nil) ; `C-?'
         (define-key map (icicle-kbd "M-?")           nil) ; `M-?'
         (define-key map (icicle-kbd "C-g")           (if (and (fboundp 'minibuffer-keyboard-quit)
                                                               delete-selection-mode)
                                                            'minibuffer-keyboard-quit
                                                          'abort-recursive-edit)) ; `C-g'
         (define-key map (icicle-kbd "M-S-backspace") nil) ; `M-S-DEL'
         (define-key map (icicle-kbd "M-S-delete")    nil) ; `M-S-delete'
         (define-key map (icicle-kbd "M-.")           nil) ; `M-.'
         (define-key map (icicle-kbd "C-x C-f")       nil) ; `C-x C-f'
         (define-key map (icicle-kbd "C-=")           nil) ; `C-='
         (define-key map (icicle-kbd "M-i")           nil) ; `M-i'
         (define-key map (icicle-kbd "M-k")           nil) ; `M-k'
         (define-key map (icicle-kbd "M-o")           nil) ; `M-o'
         (define-key map (icicle-kbd "M-:")           nil) ; `M-:'
         (define-key map (icicle-kbd "C-a")           nil) ; `C-a'
         (define-key map (icicle-kbd "C-e")           nil) ; `C-e'
         (define-key map (icicle-kbd "C-M-v")         nil) ; `C-M-v'
         (define-key map (icicle-kbd "C-M-S-v")       nil) ; `C-M-S-v' (aka `C-M-V')
         (define-key map (icicle-kbd "C-M-pause")     nil) ; `C-M-pause'
         (dolist (key  icicle-completing-read+insert-keys) (define-key map key nil)) ; `C-M-S-c'
         (dolist (key  icicle-read+insert-file-name-keys) (define-key map key nil)) ; `C-M-S-f'
         (define-key map (icicle-kbd "C-j")           'exit-minibuffer))) ; `C-j'

     ;; `minibuffer-local-completion-map': completion map.
     (icicle-restore-completion-keys minibuffer-local-completion-map)

     ;; `minibuffer-local-must-match-map': must-match map.
     ;; In Emacs 22+, local-completion is parent of local-must-match
     (if (not (eq minibuffer-local-completion-map (keymap-parent minibuffer-local-must-match-map)))
         (icicle-restore-completion-keys minibuffer-local-must-match-map)
       (define-key minibuffer-local-must-match-map (icicle-kbd "C-g")
         (if (and (fboundp 'minibuffer-keyboard-quit)  delete-selection-mode)
             'minibuffer-keyboard-quit
           'abort-recursive-edit))  ; `C-g' - need it anyway, even if inherit completion map.
       (dolist (key  icicle-completing-read+insert-keys)
         (define-key minibuffer-local-must-match-map key nil))
       (dolist (key  icicle-read+insert-file-name-keys)
         (define-key minibuffer-local-must-match-map key nil))
       (define-key minibuffer-local-must-match-map (icicle-kbd "C-j") ; `C-j' (newline)
         'minibuffer-complete-and-exit))

     ;; `completion-list-mode-map': map for `*Completions*' buffer.
     (let ((map  completion-list-mode-map))
       (dolist (key  icicle-candidate-help-keys)       (define-key map key nil))
       (define-key map (icicle-kbd "C-g")              nil)
       (define-key map (icicle-kbd "q")                nil)
       (define-key map (icicle-kbd "C-insert")         nil)
       (dolist (key  icicle-prefix-cycle-next-keys)     (define-key map key nil))
       (dolist (key  icicle-prefix-cycle-previous-keys) (define-key map key nil))
       (dolist (key  icicle-previous-candidate-keys)    (define-key map key nil))
       (define-key map (icicle-kbd "C-i")              nil)
       (define-key map (icicle-kbd "tab")              nil)
       (define-key map (icicle-kbd "S-down-mouse-2")   nil)
       (define-key map (icicle-kbd "S-mouse-2")        nil)
       (define-key map (icicle-kbd "C-S-down-mouse-2") nil)
       (define-key map (icicle-kbd "C-S-mouse-2")      nil)
       (define-key map (icicle-kbd "C-down-mouse-2")   nil)
       (define-key map (icicle-kbd "C-mouse-2")        nil)
       (define-key map (icicle-kbd "C-M-down-mouse-2") nil)
       (define-key map (icicle-kbd "C-M-mouse-2")      nil)
       (define-key map (icicle-kbd "M-S-down-mouse-2") nil)
       (define-key map (icicle-kbd "M-S-mouse-2")      nil)
       (define-key map (icicle-kbd "M-down-mouse-2")   nil)
       (define-key map (icicle-kbd "M-mouse-2")        nil)
       (define-key map (icicle-kbd "C-down-mouse-3")   nil)
       (define-key map (icicle-kbd "C-mouse-3")        nil)
       (define-key map (icicle-kbd "M-down-mouse-3")   nil)
       (define-key map (icicle-kbd "M-mouse-3")        nil)
       (define-key map (icicle-kbd "M-S-down-mouse-3") nil)
       (define-key map (icicle-kbd "M-S-mouse-3")      nil)
       (define-key map (icicle-kbd "mouse-3")          nil)
       (define-key map (icicle-kbd "C->")              nil)
       (define-key map (icicle-kbd "C-M->")            nil)
       (define-key map (icicle-kbd "C-)")              nil)
       (define-key map (icicle-kbd "C-M-)")            nil)
       (define-key map (icicle-kbd "C-M-<")            nil)
       (define-key map (icicle-kbd "C-l")              nil)
       (define-key map (icicle-kbd "C-a")              nil)
       (define-key map (icicle-kbd "C-e")              nil)
       (define-key map (icicle-kbd "down")             nil)
       (define-key map (icicle-kbd "up")               nil)
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

  ;; Menu-bar `Minibuf' menu.

  ;; In Emacs 22+, local is parent of local-completion
  (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map))
    (define-key map [menu-bar minibuf quit] ; Replace `keyboard-escape-quit'
      '(menu-item "Quit" icicle-abort-recursive-edit
        :help "Cancel minibuffer input or recursive edit"))
    (define-key map [menu-bar minibuf return]
      '(menu-item "Enter" exit-minibuffer
        :help "Terminate input and exit minibuffer" :keys "RET"))
    (define-key map [menu-bar minibuf separator-help] '("--"))

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
    (define-key map [menu-bar minibuf icicle-regexp-quote-input]
      '(menu-item "Regexp-Quote Input" icicle-regexp-quote-input
        :enable (with-current-buffer (window-buffer (minibuffer-window)) (not (zerop (buffer-size))))
        :help "Regexp-quote current input or its active region, then apropos-complete"
        :keys "M-%"))
    (define-key map [menu-bar minibuf separator-set2] '("--"))

    (define-key map [menu-bar minibuf icicle-clear-current-history]
      '(menu-item "Clear History Entries" icicle-clear-current-history
        :help "Clear current minibuffer history of selected entries"))
    (define-key map [menu-bar minibuf icicle-erase-minibuffer]
      '(menu-item "Delete from History" icicle-erase-minibuffer-or-history-element
        :visible (memq last-command
                  '(previous-history-element next-history-element
                    icicle-erase-minibuffer-or-history-element
                    previous-matching-history-element next-matching-history-element))
        :help "Delete current history element (in minibuffer now)" :keys "M-k"))
    (define-key map [menu-bar minibuf icicle-delete-history-element]
      '(menu-item "Clear (Erase) Minibuffer" icicle-erase-minibuffer-or-history-element
        :visible (not (memq last-command
                       '(previous-history-element next-history-element
                         icicle-erase-minibuffer-or-history-element
                         previous-matching-history-element next-matching-history-element)))
        :help "Erase the Minibuffer" :keys "M-k"))
    (define-key map [menu-bar minibuf icicle-insert-list-join-string]
      '(menu-item "Insert Join-String" icicle-insert-list-join-string
        :help "Insert `icicle-list-join-string' into the minibuffer"))
    (define-key map [menu-bar minibuf icicle-insert-key-description]
      '(menu-item "Insert Key Description" icicle-insert-key-description
        :visible (not icicle-searching-p) :keys "M-q"
        :help "Read key and insert its description - e.g., reading ^F inserts `C-f'"))
    (define-key map [menu-bar minibuf icicle-insert-history-element]
      '(menu-item "Insert Past Input using Completion" icicle-insert-history-element
        :help "Use completion to insert a previous input into the minibuffer"))
    (define-key map [menu-bar minibuf icicle-insert-string-from-a-var]
      '(menu-item "Insert String from a Variable..." icicle-insert-string-from-variable
        :visible current-prefix-arg :keys "C-="
        :help "Read a variable name and insert its string value into the minibuffer"))
    (define-key map [menu-bar minibuf icicle-insert-string-from-std-var]
      '(menu-item "Insert `icicle-input-string'" icicle-insert-string-from-variable
        :visible (not current-prefix-arg) :keys "C-="
        :help "Insert text from variable `icicle-input-string' into the minibuffer"))
    (define-key map [menu-bar minibuf icicle-insert-string-at-point]
      '(menu-item "Insert Text from Point" icicle-insert-string-at-point
        :help "Insert text at or near the cursor into the minibuffer"))
    (define-key map [menu-bar minibuf icicle-completing-read+insert]
      '(menu-item "Insert On-Demand Completion" icicle-completing-read+insert
        :visible (consp icicle-completing-read+insert-candidates)
        :help "Read and insert something using (lax) completion"))
    (define-key map [menu-bar minibuf icicle-read+insert-file-name]
      '(menu-item "Insert File Name" icicle-read+insert-file-name
        :help "Read and insert a file name using (lax) completion"))
    )
  (define-key map [menu-bar minibuf icicle-goto/kill-failed-input]
    '(menu-item "Cursor to Mismatch (Repeat: Kill)" icicle-goto/kill-failed-input
      :enable (and (overlayp icicle-input-completion-fail-overlay)
               (overlay-start icicle-input-completion-fail-overlay))
      :help "Put cursor where input fails to complete - repeat to kill mismatch"))
  (define-key map [menu-bar minibuf icicle-retrieve-next-input]
    '(menu-item "Restore Next Completion Input" icicle-retrieve-next-input
      :enable (consp (symbol-value (if (icicle-file-name-input-p)
                                       'icicle-previous-raw-file-name-inputs
                                     'icicle-previous-raw-non-file-name-inputs)))
      :help "Cycle forward to insert a previous completion input in the minibuffer (`C-u': \
complete)"))
  (define-key map [menu-bar minibuf icicle-retrieve-previous-input]
    '(menu-item "Restore Previous Completion Input" icicle-retrieve-previous-input
      :enable (consp (symbol-value (if (icicle-file-name-input-p)
                                       'icicle-previous-raw-file-name-inputs
                                     'icicle-previous-raw-non-file-name-inputs)))
      :help "Cycle backward to insert a previous completion input in the minibuffer (`C-u': \
complete)"))
  (define-key map [menu-bar minibuf separator-C-l] '("--"))

  (define-key map [menu-bar minibuf ?\?] nil)
  (define-key map [menu-bar minibuf space] nil)
  (define-key map [menu-bar minibuf tab] nil)
  (define-key map [menu-bar minibuf alt-action-list-all]
    '(menu-item "Alt Act on List of Candidates" icicle-all-candidates-list-alt-action
      :help "Apply the alternative action to the list of matching completion candidates"
      :enable icicle-all-candidates-list-alt-action-fn))
  (define-key map [menu-bar minibuf alt-action-all]
    '(menu-item "Alt Act on Each Candidate" icicle-all-candidates-alt-action
      :help "Apply the alternative action to each matching completion candidates"
      :enable icicle-candidate-alt-action-fn))
  (define-key map [menu-bar minibuf action-list-all]
    '(menu-item "Act on List of Candidates" icicle-all-candidates-list-action
      :help "Apply the command action to the list of matching completion candidates"
      :enable icicle-all-candidates-list-action-fn))
  (define-key map [menu-bar minibuf action-all]
    '(menu-item "Act on Each Candidate" icicle-all-candidates-action
      :help "Apply the command action to each matching completion candidates"
      :enable icicle-candidate-action-fn))
  (define-key map [menu-bar minibuf separator-actions] '("--"))

  (define-key map [menu-bar minibuf set-define]
    '(menu-item "Define Candidates by Lisp Sexp" icicle-candidate-set-define
      :help "Define the set of current completion candidates by evaluating a sexp"))
  (define-key map [menu-bar minibuf icicle-keep-only-past-inputs]
    '(menu-item "Keep Only Previously Entered" icicle-keep-only-past-inputs
      :enable (and icicle-completion-candidates  (consp (symbol-value minibuffer-history-variable)))
      :help "Removed candidates that you have not previously chosen and entered"))
  (define-key map [menu-bar minibuf set-union]
    '(menu-item "Add (Union) Saved Candidate Set" icicle-candidate-set-union
      :enable icicle-saved-completion-candidates
      :help "Set union between the current and saved completion candidates"))
  (define-key map [menu-bar minibuf set-difference]
    '(menu-item "Subtract Saved Candidate Set" icicle-candidate-set-difference
      :enable icicle-saved-completion-candidates
      :help "Set difference between the current and saved completion candidates"))
  (define-key map [menu-bar minibuf set-intersection]
    '(menu-item "Intersect Saved Candidate Set" icicle-candidate-set-intersection
      :enable icicle-saved-completion-candidates
      :help "Set intersection between the current and saved candidates"))
  (define-key map [menu-bar minibuf icicle-save-predicate-to-variable]
    '(menu-item "Save Predicate to Variable" icicle-save-predicate-to-variable
      :help "Save the current completion predicate to a variable"))
  (define-key map [menu-bar minibuf icicle-narrow-candidates-with-predicate]
    '(menu-item "Satisfy Also Predicate..." icicle-narrow-candidates-with-predicate
      :help "Match another input pattern (narrow completions set)"
      :enable icicle-completion-candidates))
  (define-key map [menu-bar minibuf icicle-narrow-candidates]
    '(menu-item "Match Also Regexp..." icicle-narrow-candidates
      :enable icicle-completion-candidates
      :help "Match another input pattern (narrow completions set)"))
  (define-key map [menu-bar minibuf icicle-widen-candidates]
    '(menu-item "Match Alternative..." icicle-widen-candidates
      :enable icicle-completion-candidates
      :help "Match alternative input pattern (widen completions set)"))
  (define-key map [menu-bar minibuf set-complement]
    '(menu-item "Complement Candidates" icicle-candidate-set-complement
      :help "Complement the set of current completion candidates"))
  (define-key map [menu-bar minibuf separator-set1] '("--"))

  (define-key map [menu-bar minibuf set-swap]
    '(menu-item "Swap Saved and Current Sets" icicle-candidate-set-swap
      :enable icicle-saved-completion-candidates
      :help "Swap the saved and current sets of completion candidates"))
  (define-key map [menu-bar minibuf icicle-candidate-set-save-more-selected]
    '(menu-item "Save More Selected (Region) Candidates"
      icicle-candidate-set-save-more-selected
      :help "Add the candidates in the region to the saved candidates"))
  (define-key map [menu-bar minibuf icicle-candidate-set-save-selected]
    '(menu-item "Save Selected (Region) Candidates"
      icicle-candidate-set-save-selected
      :help "Save the candidates in the region, for later recall"))
  (define-key map [menu-bar minibuf icicle-candidate-set-save-more]
    '(menu-item "Save More Candidates" icicle-candidate-set-save-more
      :help "Add current completion candidates to saved candidates set"))
  (define-key map [menu-bar minibuf set-save-to-cache-file]
    '(menu-item "    to Cache File..." icicle-candidate-set-save-persistently
      :help "Save current completion candidates to a cache file, for later recall"))
  (define-key map [menu-bar minibuf set-save-to-variable]
    '(menu-item "    to Variable..." icicle-candidate-set-save-to-variable
      :help "Save current completion candidates to a variable, for later recall"))
  (define-key map [menu-bar minibuf set-save]
    '(menu-item "Save Candidates" icicle-candidate-set-save
      :help "Save the set of current completion candidates, for later recall"))
  (define-key map [menu-bar minibuf icicle-candidate-set-retrieve-more]
    '(menu-item "Retrieve More Saved Candidates"
      icicle-candidate-set-retrieve-more
      :help "Add saved candidates to current completion candidates"))
  (define-key map [menu-bar minibuf set-retrieve-from-cache-file]
    '(menu-item "    from Cache File..."
      icicle-candidate-set-retrieve-persistent
      :help "Retrieve saved completion candidates from a cache file, making them current"))
  (define-key map [menu-bar minibuf set-retrieve-from-variable]
    '(menu-item "    from Variable..." icicle-candidate-set-retrieve-from-variable
      :help "Retrieve saved completion candidates from variable, making them current"))
  (define-key map [menu-bar minibuf set-retrieve]
    '(menu-item "Retrieve Saved Candidates" icicle-candidate-set-retrieve
      :enable icicle-saved-completion-candidates
      :help "Retrieve the saved set of completion candidates, making it current"))
  (define-key map [menu-bar minibuf separator-complete] '("--"))

  (define-key map [menu-bar minibuf word-complete]
    '(menu-item "Word-Complete" icicle-prefix-word-complete
      :help "Complete at most one word of prefix"))
  (define-key map [menu-bar minibuf prefix-complete]
    '(menu-item "Prefix-Complete" icicle-prefix-complete
      :help "Complete prefix as far as possible"))
  (define-key map [menu-bar minibuf apropos-complete]
    '(menu-item "Apropos-Complete" icicle-apropos-complete :keys "S-TAB"
      :help "Complete regular expression as far as possible and list completions"))

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
  (icicle-bind-custom-completion-keys map))

(defun icicle-bind-custom-completion-keys (map &optional defs)
  "Bind customizable keys for minibuffer completion map MAP.
These are the keys defined by option `icicle-completion-key-bindings'.
MAP is `minibuffer-local-completion-map' or
`minibuffer-local-must-match-map'."
  (let (key command condition)
    (unless defs  (setq defs  icicle-completion-key-bindings))
    (dolist (key-def  defs)
      (setq key        (car key-def)
            command    (cadr key-def)
            condition  (car (cddr key-def)))
      (when (eval condition)
        (if (symbolp key)
            (icicle-remap key command map)
          (define-key map key command))))))

(defun icicle-restore-completion-keys (map)
  "Restore standard keys for minibuffer completion map MAP.
MAP is `minibuffer-local-completion-map',
`minibuffer-local-filename-completion-map', or
`minibuffer-local-must-match-map'."

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
  (icicle-restore-custom-completion-keys map)

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

(defun icicle-restore-custom-completion-keys (map)
  "Restore customizable keys for minibuffer completion map MAP.
These are the keys defined by option `icicle-completion-key-bindings'.
MAP is `minibuffer-local-completion-map',
`minibuffer-local-filename-completion-map', or
`minibuffer-local-must-match-map'."
  (let (key condition)
    (dolist (key-def  icicle-completion-key-bindings)
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
    ;; Change the region background here dynamically.  It would be better to
    ;; just use a buffer-local face, but those don't yet exist.
    (when (= 1 (recursion-depth))
      (setq icicle-saved-region-background  (face-background 'region))
      (when icicle-change-region-background-flag
        (set-face-background 'region icicle-region-background)))
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
          ;; `other-buffer' doesn't work, because it looks for a buffer only from the same frame.
          icicle-pre-minibuffer-buffer           (cadr (buffer-list)) ; $$$$$$ (other-buffer nil t)
          )
    (when (and (icicle-completing-p)  (> emacs-major-version 20))
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
               (not icicle-progressive-completing-p) ; If narrowed, then we have already completed.
               (icicle-completing-p)    ; Function initializes variable `icicle-completing-p'.
               (sit-for icicle-incremental-completion-delay)) ; Let user interrupt.
      (case icicle-default-cycling-mode
        (apropos    (icicle-apropos-complete))
        (otherwise  (icicle-prefix-complete)))) ; Prefix completion, by default.
    (run-hooks 'icicle-minibuffer-setup-hook)))

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
    (set-face-background 'region icicle-saved-region-background)))

(defun icicle-activate-mark ()
  "Prevent region from being deactivated.  Use in `icicle-post-command-hook'."
  (when (and (window-minibuffer-p (selected-window))
             icicle-completing-p
             (not executing-kbd-macro))
    (setq deactivate-mark  nil)))

(defun icicle-redefine-standard-functions ()
  "Alias the functions in `icicle-functions-to-redefine' to Icicles versions."
  (when (fboundp 'icicle-completing-read)
    (dolist (fn  icicle-functions-to-redefine)
      (when (fboundp (intern (concat "icicle-ORIG-" (symbol-name fn))))
        (defalias fn (intern (concat "icicle-" (symbol-name fn))))))))

(defun icicle-restore-standard-functions ()
  "Restore original versions of functions in `icicle-functions-to-redefine'."
  (when (fboundp 'icicle-ORIG-completing-read)
    (let (orig-fn)
      (dolist (fn  icicle-functions-to-redefine)
        (when (fboundp (setq orig-fn  (intern (concat "icicle-ORIG-" (symbol-name fn)))))
          (defalias fn orig-fn))))))

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
    (defalias 'choose-completion            'icicle-choose-completion)
    (defalias 'choose-completion-string     'icicle-choose-completion-string)
    (defalias 'completing-read              'icicle-completing-read)
    (when (fboundp 'icicle-completing-read-multiple)
      (defalias 'completing-read-multiple   'icicle-completing-read-multiple)
      (setq crm-local-completion-map  icicle-crm-local-completion-map
            crm-local-must-match-map  icicle-crm-local-must-match-map))
    (defalias 'completion-setup-function    'icicle-completion-setup-function)
    (unless (> emacs-major-version 22)
      (defalias 'dired-smart-shell-command  'icicle-dired-smart-shell-command))
    (defalias 'display-completion-list      'icicle-display-completion-list)
    (defalias 'exit-minibuffer              'icicle-exit-minibuffer)
    (when (fboundp 'face-valid-attribute-values)
      (defalias 'face-valid-attribute-values 'icicle-face-valid-attribute-values))
    (defalias 'minibuffer-complete-and-exit 'icicle-minibuffer-complete-and-exit)
    (defalias 'mouse-choose-completion      'icicle-mouse-choose-completion)
    (defalias 'next-history-element         'icicle-next-history-element)
    (defalias 'read-buffer                  'icicle-read-buffer)
    (defalias 'read-face-name               'icicle-read-face-name)
    (if (boundp 'read-file-name-function) ; Emacs 22+
        (setq icicle-orig-read-file-name-fn  (prog1 (and (not (eq read-file-name-function
                                                                  'icicle-read-file-name))
                                                         read-file-name-function)
                                               (setq read-file-name-function
                                                     'icicle-read-file-name)))
      (defalias 'read-file-name             'icicle-read-file-name)) ; Emacs 20, 21
    (when (fboundp 'read-file-name-default) ; Emacs 24+
      (defalias 'read-file-name-default       'icicle-read-file-name-default))
    (when (fboundp 'icicle-read-number)
      (defalias 'read-number                'icicle-read-number))
    (unless (> emacs-major-version 22)
      (defalias 'shell-command              'icicle-shell-command))
    (unless (> emacs-major-version 22)
      (defalias 'shell-command-on-region    'icicle-shell-command-on-region))
    (when (> emacs-major-version 22)
      (defalias 'sit-for                    'icicle-sit-for))
    (defalias 'switch-to-completions        'icicle-switch-to-completions)
    ))

(defun icicle-restore-std-completion-fns ()
  "Restore some standard functions that were replaced in Icicle mode."
  (when (fboundp 'icicle-ORIG-completing-read)
    (defalias 'choose-completion            'icicle-ORIG-choose-completion)
    (defalias 'choose-completion-string     'icicle-ORIG-choose-completion-string)
    (defalias 'completing-read              'icicle-ORIG-completing-read)
    (when (fboundp 'icicle-ORIG-completing-read-multiple)
      (defalias 'completing-read-multiple   'icicle-ORIG-completing-read-multiple)
      (setq crm-local-completion-map  icicle-ORIG-crm-local-completion-map
            crm-local-must-match-map  icicle-ORIG-crm-local-must-match-map))
    (defalias 'completion-setup-function    'icicle-ORIG-completion-setup-function)
    (when (fboundp 'icicle-ORIG-dired-smart-shell-command) ; Emacs 23
      (defalias 'dired-smart-shell-command  'icicle-ORIG-dired-smart-shell-command))
    (defalias 'display-completion-list      'icicle-ORIG-display-completion-list)
    (defalias 'exit-minibuffer              'icicle-ORIG-exit-minibuffer)
    (when (fboundp 'icicle-ORIG-face-valid-attribute-values)
      (defalias 'face-valid-attribute-values 'icicle-ORIG-face-valid-attribute-values))
    (defalias 'minibuffer-complete-and-exit 'icicle-ORIG-minibuffer-complete-and-exit)
    (defalias 'mouse-choose-completion      'icicle-ORIG-mouse-choose-completion)
    (defalias 'next-history-element         'icicle-ORIG-next-history-element)
    (defalias 'read-buffer                  'icicle-ORIG-read-buffer)
    (defalias 'read-face-name               'icicle-ORIG-read-face-name)
    (if (boundp 'read-file-name-function) ; Emacs 22+
        (setq read-file-name-function  (and (not (eq icicle-orig-read-file-name-fn
                                                     'icicle-read-file-name))
                                            icicle-orig-read-file-name-fn))
      (defalias 'read-file-name             'icicle-ORIG-read-file-name)) ; Emacs 20, 21
    (when (fboundp 'icicle-ORIG-read-file-name-default) ; Emacs 24+
      (defalias 'read-file-name-default       'icicle-ORIG-read-file-name-default))
    (when (fboundp 'icicle-ORIG-read-number)
      (defalias 'read-number                'icicle-ORIG-read-number))
    (when (fboundp 'icicle-ORIG-shell-command) ; Emacs < 23
      (defalias 'shell-command              'icicle-ORIG-shell-command))
    (when (fboundp 'icicle-ORIG-shell-command-on-region) ; Emacs < 23
      (defalias 'shell-command-on-region    'icicle-ORIG-shell-command-on-region))
    (when (> emacs-major-version 22)
      (defalias 'sit-for                    'icicle-ORIG-sit-for))
    (defalias 'switch-to-completions        'icicle-ORIG-switch-to-completions)
    ))

;; Free vars here: `icicle-saved-kmacro-ring-max' is bound in `icicles-var.el'.
(defun icicle-redefine-standard-options ()
  "Replace certain standard Emacs options with Icicles versions."
  (when (boundp 'icicle-search-ring-max)
    (setq icicle-saved-search-ring-max         search-ring-max ; Save it.
          search-ring-max                      icicle-search-ring-max
          icicle-saved-regexp-search-ring-max  regexp-search-ring-max ; Save it.
          regexp-search-ring-max               icicle-regexp-search-ring-max))
  (when (boundp 'icicle-kmacro-ring-max)
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
                 (defalias 'icicle-ORIG-comint-dynamic-complete
                     (symbol-function 'comint-dynamic-complete)))
               (when (and (fboundp 'comint-dynamic-complete-filename)
                          (not (fboundp 'icicle-ORIG-comint-dynamic-complete-filename)))
                 (defalias 'icicle-ORIG-comint-dynamic-complete-filename
                     (symbol-function 'comint-dynamic-complete-filename)))
               (when (and (fboundp 'comint-replace-by-expanded-filename)
                          (not (fboundp 'icicle-ORIG-comint-replace-by-expanded-filename)))
                 (defalias 'icicle-ORIG-comint-replace-by-expanded-filename
                     (symbol-function 'comint-replace-by-expanded-filename)))
	       (when (and (fboundp 'comint-completion-at-point) ; Emacs 24+
                          (not (fboundp 'icicle-ORIG-comint-completion-at-point)))
                 (defalias 'icicle-ORIG-comint-completion-at-point
                     (symbol-function 'comint-completion-at-point)))
               (when icyp (icicle-mode 1)))))
  (if (featurep 'comint) (eval-after-load "icicles-mode" form) (eval-after-load "comint" form)))

;;; `ess-site.el' - `ess-complete-object-name'.
(let ((form  '(let ((icyp  (and (boundp 'icicle-mode)  icicle-mode)))
               (when icyp (icicle-mode -1))
               (when (and (fboundp 'ess-complete-object-name)
                          (not (fboundp 'icicle-ORIG-ess-complete-object-name)))
                 (defalias 'icicle-ORIG-ess-complete-object-name
                     (symbol-function 'ess-complete-object-name)))
               (when icyp (icicle-mode 1)))))
  (if (featurep 'ess-site) (eval-after-load "icicles-mode" form) (eval-after-load "ess-site" form)))

;;; `gud.el' - `gud-gdb-complete-command'.
(let ((form  '(let ((icyp  (and (boundp 'icicle-mode)  icicle-mode)))
               (when icyp (icicle-mode -1))
               (when (and (fboundp 'gud-gdb-complete-command)
                          (not (fboundp 'icicle-ORIG-gud-gdb-complete-command)))
                 (defalias 'icicle-ORIG-gud-gdb-complete-command
                     (symbol-function 'gud-gdb-complete-command)))
               (when icyp (icicle-mode 1)))))
  (if (featurep 'gud) (eval-after-load "icicles-mode" form) (eval-after-load "gud" form)))

;;; `info.el' - `Info-goto-node', `Info-index', `Info-menu'.
(let ((form  '(let ((icyp  (and (boundp 'icicle-mode)  icicle-mode)))
               (when icyp (icicle-mode -1))
               (when (and (featurep 'info)  (not (fboundp 'icicle-ORIG-Info-goto-node)))
                 (defalias 'icicle-ORIG-Info-goto-node (symbol-function 'Info-goto-node))
                 (defalias 'icicle-ORIG-Info-index     (symbol-function 'Info-index))
                 (defalias 'icicle-ORIG-Info-menu      (symbol-function 'Info-menu)))
               (when icyp (icicle-mode 1)))))
  (if (featurep 'info) (eval-after-load "icicles-mode" form) (eval-after-load "info" form)))

;;; `bbdb-com.el' version 2.35 - `bbdb-complete-name'.
(let ((form  '(let ((icyp  (and (boundp 'icicle-mode)  icicle-mode)))
               (when icyp (icicle-mode -1))
               (when (and (fboundp 'bbdb-complete-name)
                          (not (fboundp 'icicle-ORIG-bbdb-complete-name)))
                 (defalias 'icicle-ORIG-bbdb-complete-name (symbol-function 'bbdb-complete-name)))
               (when icyp (icicle-mode 1)))))
  (if (and (featurep 'bbdb-com)  (fboundp 'bbdb-complete-name))
      (eval-after-load "icicles-mode" form)
    (eval-after-load "bbdb-com" form)))

;;; `bbdb-com.el' version 3.02 - `bbdb-complete-mail'.
(let ((form  '(let ((icyp  (and (boundp 'icicle-mode)  icicle-mode)))
               (when icyp (icicle-mode -1))
               (when (and (fboundp 'bbdb-complete-mail)
                          (not (fboundp 'icicle-ORIG-bbdb-complete-mail)))
                 (defalias 'icicle-ORIG-bbdb-complete-mail (symbol-function 'bbdb-complete-mail)))
               (when icyp (icicle-mode 1)))))
  (if (and (featurep 'bbdb-com)  (fboundp 'bbdb-complete-mail))
      (eval-after-load "icicles-mode" form)
    (eval-after-load "bbdb-com" form)))

;;; `dired-aux.el' - `dired-read-shell-command'.
(let ((form  '(let ((icyp  (and (boundp 'icicle-mode)  icicle-mode)))
               (when icyp (icicle-mode -1))
               (when (and (fboundp 'dired-read-shell-command)
                          (not (fboundp 'icicle-ORIG-dired-read-shell-command)))
                 (defalias 'icicle-ORIG-dired-read-shell-command
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
                 (defalias 'icicle-ORIG-dired-read-shell-command
                     (symbol-function 'dired-read-shell-command)))
               (unless (fboundp 'read-shell-command) ; `dired-smart-shell-command' in Emacs < 23.
                 (when (and (fboundp 'dired-smart-shell-command)
                            (not (fboundp 'icicle-ORIG-dired-smart-shell-command)))
                   (defalias 'icicle-ORIG-dired-smart-shell-command
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
                 (defalias 'icicle-ORIG-recentf-make-menu-items
                     (symbol-function 'recentf-make-menu-items)))
               (when icyp (icicle-mode 1)))))
  (if (featurep 'recentf) (eval-after-load "icicles-mode" form) (eval-after-load "recentf" form)))

      
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

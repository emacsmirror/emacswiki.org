;;; mon-default-start-loads.el --- fncns for initializing MON Emacs environment
;;; ==============================
;; -*- mode: EMACS-LISP; no-byte-compile: t; -*-

;;; ================================================================
;; Copyright © 2009, 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-default-start-loads.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2009-01-15T14:01:42-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: local, environment, installation, emacs

;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; Provides functions required when initializing the Emacs startup 
;; Environment. Makes numerous condtional tests on current user 
;; `IS-BUG-P', `IS-MON-P-GNU', `IS-MON-P-W32'. Accordingly, makes the initial
;; `require' calls for (among others) the following packages: 
;; mon-utils.el, mon-w32-load.el, mon-GNU-load.el, color-theme.el, naf-mode.el,
;; dired-details.el, mon-doremi.el, google-define.el, uniq.el, regexpl.el,
;; register-list.el, color-occur.el, boxquote.el, dvc-autoloads.el,
;; traverselisp.el, show-point-mode.el, apache-mode.el, mon-keybindings.el,
;;
;; FUNCTIONS:►►►
;; `mon-build-path-for-load-path', `mon-keybind-put-hooks-init',
;; `mon-keybind-w32-init', `mon-rst-mode-facification',
;; `mon-set-C-source-directory-init', `mon-set-apache-mode-init',
;; `mon-set-auctex-init', `mon-set-bookmark-file-init', `mon-set-browser-init',
;; `mon-set-buffer-local-comment-start',
;; `mon-set-buffer-local-comment-start-init', `mon-set-color-themes-init',
;; `mon-set-common-lisp-hspec-init', `mon-set-css-path-init',
;; `mon-set-custom-file-init', `mon-set-customizations-before-custom-file-init',
;; `mon-set-doc-view-programs-init', `mon-set-dvc-init',
;; `mon-set-emacs-temp-file/dir-init', `mon-set-help-mode-init',
;; `mon-set-ibuffer-init', `mon-set-ido-init', `mon-set-infopath-init',
;; `mon-set-ispell-init', `mon-set-lisp-init', `mon-set-load-path-init',
;; `mon-set-longlines-init', `mon-set-proced-init',
;; `mon-set-rst-mode-faces-init', `mon-set-system-specific-and-load-init',
;; `mon-set-thumbs-conversion-program-init', `mon-set-traverselisp-init',
;; `mon-set-unicodedata-init', `mon-set-w3m-init', `mon-set-woman-manpath-init',
;; `mon-set-url-pkg-init', `mon-set-google-maps-init',
;; `mon-set-erc-configs-init', `mon-set-custom-file-init-w32-configs',
;; `mon-set-boxcutter-init', `mon-set-mon-feature-customs-init',
;; `mon-set-image-dired-init', `mon-set-org-init',
;; `mon-set-custom-customizations-init',
;; FUNCTIONS:◄◄◄
;; 
;; MACROS:
;; `mon-default-start-error/sane', 
;;
;; CONSTANTS:
;;
;; VARIABLES:
;; `*mon-default-start-loads-xrefs*'
;; `*mon-default-start-load-sanity*'
;; `*mon-default-start-load-sanity-WARN-ONLY*'
;;
;; GROUPS:
;; `mon-default-start-loads'
;;
;; ADVISED:
;; `find-function-search-for-symbol' , `find-variable-noselect' 
;;
;; ALIASED/ADIVISED/SUBST'D:
;;
;; <UNQUALIFIED-ALIAS>                   <CORE-SYMBOL>
;; `debug-on-error-toggle'           -> `toggle-debug-on-error'
;;
;; DEPRECATED:
;;
;; RENAMED:
;; `mon-actvt-show-point-mode'      -> `mon-toggle-show-point-mode'
;; `bld-path-for-load-path'         -> `mon-build-path-for-load-path'
;; `mon-switch-bookmark-file'       -> `mon-set-bookmark-file-init'
;; `mon-buffer-local-comment-start' -> `mon-set-buffer-local-comment-start'
;;
;; MOVED:
;; `mon-cmd'                   -> mon-utils.el
;; `mon-conkeror'              -> mon-utils.el
;; `mon-terminal'              -> mon-utils.el
;; `mon-firefox'               -> mon-utils.el
;; `*mon-tags-table-list*'     -> mon-doc-help-utils.el
;; `mon-update-tags-tables'    -> mon-doc-help-utils.el
;;
;; REQUIRES:
;; :FILE mon-color-occur.el 
;; :NOTE mon-color-occur.el is a patched version of Matsushita Akihisa color-cccur.el
;; :SEE (URL `http://www.bookshelf.jp/elc/color-occur.el')
;;
;; TODO:
;;
;; NOTES:
;; == defavice for *Help* ==
;; :SEE (URL `http://www.emacswiki.org/emacs-en/OpenQuestions#toc24')
;; From *Help* buffer how to automatically examine *.el source in view-mode?
;; C-h f view-mode
;; With point over 'view.el' of Help buffer @ line 1
;; M-x describe-text-properties
;; :RETURNS
;; "Here is a 'help-function-def' button labeled `view.el'. There are text
;; properties here: button (t) category help-function-def-button help-args
;; (view-mode "../emacs/lisp/view.el")"
;;
;; Most of the time, when I go to examine the source from Help I want to do so
;; without having to worry about mucking it up accidentally esp. when the source is
;; beneath "../emacs/lisp/*.el". In these situations I almost alwasy want to read
;; the source not edit it. View-mode is my prefered way of examining source when I
;; want only to read it as it allows me to page in a manner congruent with most
;; GNU/nix environments/vi/less/more etc. Is it possible to hook into the help button
;; actions to toggle view-mode when opening for an *.el file from Help? AFAICT Help
;; is already leveraging view-mode in a non extensible manner. How does one get
;; Emacs to jump over its own head?
;;
;; We used to add `.dbc'  extensions to `auto-mode-alist' with `longlines-mode'.
;; However, as '.dbc' extension are used for any and _all_ notes/data re: DBC
;; related material this was too broad a setting, and screwed up programmatic 
;; creation of files with '.dbc' extension. 
;;
;; SNIPPETS:
;;
;; THIRD PARTY CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-default-start-loads.el
;; FIRST-PUBLISHED: <Timestamp: #{2009-09-23T12:18:55-04:00Z}#{09393} - by MON>
;;
;; EMACSWIKI:
;;
;; HEADER-ADDED: <Timestamp: #{2009-08-17T13:08:30-04:00Z}#{09341} - by MON>
;;
;; FILE-CREATED:
;; <Timestamp: #{2009-01-15T14:01:42-04:00Z} - by MON>
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
;; Copyright © 2009-2011 MON KEY 
;;; ==============================

;;; CODE:

 
(eval-when-compile (require 'cl))

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
  (setq *IS-MON-OBARRAY* (make-vector 17 nil)))


;;; ==============================
;;; :CHANGESET 2387
;;; :CREATED <Timestamp: #{2011-01-11T15:54:56-05:00Z}#{11022} - by MON KEY>
(defgroup mon-default-start-loads nil
  "Customization group for variables and functions of :FILE mon-default-start-loads.el\n
:SEE-ALSO .\n►►►"
  :prefix "mon-set-"
  :link '(url-link 
          :tag ":EMACSWIKI-FILE" "http://www.emacswiki.org/emacs/mon-default-start-loads.el")
  :link '(emacs-library-link "mon-default-start-loads.el")
  :group 'mon-base)

;;; ==============================
;;; :TODO Figure out why Emacs 23.2 clobbers the minibuffer when Emacs is
;;; invoked by fluxbox at startup. Has something to do with X frame params and
;;; menu-bar-mode being disabled.
;;; start looking around `resize-mini-windows' and `command-line-x-option-alist'
;;; First things first. Make sure we aren't tortured by blinking cursors etc.
(when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
           (bound-and-true-p IS-MON-SYSTEM-P))
  ;; <UNQUALIFIED-ALIAS>      <CORE-SYMBOL>
  (unless (and (intern-soft "debug-on-error-toggle" obarray)
               (fboundp (intern-soft "debug-on-error-toggle" obarray)))
(defalias 'debug-on-error-toggle 'toggle-debug-on-error))
  ;;
  (unless show-paren-mode (show-paren-mode 1))
  (unless (null scroll-bar-mode) (scroll-bar-mode -1))
  (unless (null tool-bar-mode) (tool-bar-mode -1))
  (unless (null blink-cursor-mode) (blink-cursor-mode -1))
  (setq-default cursor-type '(bar . 3))
  (when (and (intern-soft "IS-MON-P" obarray)  ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-P))
    (unless (null menu-bar-mode) (menu-bar-mode -1)))
  (when (and (intern-soft "IS-W32-P" obarray) ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-W32-P))
    (setq-default x-select-enable-clipboard 1))
  (put 'narrow-to-region  'disabled nil)
  (put 'downcase-region   'disabled nil)
  (put 'upcase-region     'disabled nil)
  (put 'capitalize-region 'disabled nil)
  (put 'eval-expression   'disabled nil))

;;; ==============================
;;; Tell BC whats who for `mon-keybind-put-hooks-init' & `mon-set-system-specific-and-load-init'
(dolist (kybnd-fncn '(mon-keybind-w3m
                      mon-keybind-dired-mode
                      mon-keybind-completions
                      mon-keybind-emacs-lisp-mode
                      mon-keybind-lisp-interaction-mode
                      mon-keybind-slime))
  (declare-function kybnd-fncn "mon-keybindings" t t))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-02T18:02:25-04:00Z}#{10135} - by MON KEY>
(defcustom *mon-default-start-loads-xrefs* nil
  "*Xrefing list `mon-set-*' functions and related mon initialization symbols.\n
The symbols contained of this list are defined in :FILE mon-default-start-loads.el\n
:SEE-ALSO `*mon-default-loads-xrefs*', `*mon-default-start-loads-xrefs*',
`*mon-dir-locals-alist-xrefs*', `*mon-testme-utils-xrefs*',
`*mon-button-utils-xrefs*', `*naf-mode-xref-of-xrefs*'.\n►►►"
  :type '(repeat symbol)
  :group 'mon-default-start-loads
  :group 'mon-xrefs)
;;
(unless (and (intern-soft "*mon-default-start-loads-xrefs*" obarray)
             (bound-and-true-p *mon-default-start-loads-xrefs*))
  (setq *mon-default-start-loads-xrefs* 
        '(mon-default-start-error/sane mon-build-path-for-load-path
          mon-keybind-put-hooks-init mon-keybind-w32-init
          mon-rst-mode-facification mon-set-C-source-directory-init
          mon-set-apache-mode-init mon-set-auctex-init
          mon-set-bookmark-file-init mon-set-browser-init
          mon-set-buffer-local-comment-start
          mon-set-buffer-local-comment-start-init mon-set-color-themes-init
          mon-set-common-lisp-hspec-init mon-set-css-path-init
          mon-set-custom-file-init
          mon-set-customizations-before-custom-file-init
          mon-set-doc-view-programs-init mon-set-dvc-init
          mon-set-emacs-temp-file/dir-init mon-set-help-mode-init
          mon-set-ibuffer-init mon-set-ido-init mon-set-infopath-init
          mon-set-ispell-init mon-set-lisp-init mon-set-load-path-init
          mon-set-longlines-init mon-set-proced-init mon-set-rst-mode-faces-init
          mon-set-system-specific-and-load-init
          mon-set-thumbs-conversion-program-init mon-set-boxcutter-init
          mon-set-traverselisp-init mon-set-unicodedata-init mon-set-w3m-init
          mon-set-woman-manpath-init mon-toggle-show-point-mode
          mon-set-url-pkg-init mon-set-google-maps-init
          mon-set-init-fncn-xrefs-init mon-set-erc-configs-init
          mon-set-custom-file-init-w32-configs mon-set-mon-feature-customs-init
          mon-set-custom-customizations-init mon-set-image-dired-init 
          ;; :VARIABLES 
          *mon-default-start-load-sanity*
          *mon-default-start-load-sanity-WARN-ONLY*
          *mon-default-start-loads-xrefs*))
  (custom-note-var-changed '*mon-default-start-loads-xrefs*))


 
;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-02T18:29:52-04:00Z}#{10135} - by MON KEY>
(defvar *mon-default-start-load-sanity* nil 
  "*A list of functions evalauted when initalizing MON init.\n 
List elements are those functions which loaded without errors and therefor
should not be evaluated again in the current Emacs session.\n
:SEE-ALSO `*mon-default-start-load-sanity-WARN-ONLY*',
`mon-default-start-error/sane' `*mon-default-start-loads-xrefs*'.\n►►►")
;;
;;;(progn (makunbound '*mon-default-start-load-sanity*)
;;;       (unintern   "*mon-default-start-load-sanity*" obarray) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-06T14:02:50-04:00Z}#{10142} - by MON KEY>
(defvar *mon-default-start-load-sanity-WARN-ONLY* nil
  "When non-nil override all error singaling of `mon-default-start-error/sane'.\n
Setting this variable non-nil may be useful when:\n
 o Debugging inits;\n
 o Examining/valuating a function member of `*mon-default-start-load-sanity*';\n
 o When `IS-NOT-A-MON-SYSTEM' to avoid the `IS-MON-SYSTEM-P' runtime checks.\n
:NOTE Can be let bound to disable signaling errors when reloading MON system/
 \(let \(\(*mon-default-start-load-sanity-WARN-ONLY* t\)\)
  \(load \"~/.emacs\"\)\)
:SEE-ALSO `*mon-default-start-load-sanity*'.\n►►►")

;;; ==============================
;;; :RENAMED `bld-path-for-load-path' -> `mon-build-path-for-load-path'
(defun mon-build-path-for-load-path (expand-path suffix-path) 
  "Return a path for `load-path' by concat'ing EXPAND-PATH and SUFFIX-PATH.\n
:SEE-ALSO `mon-set-custom-file-init', `mon-set-buffer-local-comment-start',
`mon-set-bookmark-file-init', `mon-set-infopath-init', `mon-build-path'.\n►►►"
  (concat (file-name-as-directory expand-path) suffix-path))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-02T18:13:07-04:00Z}#{10135} - by MON KEY>
(defmacro mon-default-start-error/sane (fncn &optional just-warn &rest do-sane)
  "Evaluate body DO-SANE when `IS-MON-SYSTEM-P' returns non-nil.\n
Signal an error if `IS-NOT-A-MON-SYSTEM' or FNCN was evaluated at init and
optional arg just-warn was ommitted or null.\n
If FNCN has not yet been evaluated and `IS-MON-SYSTEM-P' push function onto
variable `*mon-default-start-load-sanity*'.\n
The intent is to prevent those procedures evalauated at init time from: :FILE
mon-default-start-loads.el from being inadverdently re-evaluated later in the
current Emacs session.\n
When optional arg JUST-WARN or `*mon-default-start-load-sanity-WARN-ONLY*'
is non-nil signal a warning instead of signaling an error.\n
Other users of `mon-*.el' packages may choose to change or rebind the
`bound-and-true-p' check for `IS-MON-SYSTEM-P' to ensure MON packages integrate
sanely on their systems. Binding `*mon-default-start-load-sanity-WARN-ONLY*'
non-nil to will also override all error signaling.\n
:SEE-ALSO .\n►►►"
  `(progn
     (cond ((or (and (intern-soft "IS-NOT-A-MON-SYSTEM" obarray)  ;; *IS-MON-OBARRAY*
                     (bound-and-true-p IS-NOT-A-MON-SYSTEM))
                (not (intern-soft "IS-MON-SYSTEM-P" obarray))    ;; *IS-MON-OBARRAY*
                (not (bound-and-true-p IS-MON-SYSTEM-P)))
            (if (or *mon-default-start-load-sanity-WARN-ONLY* ,just-warn)
                (warn                      ;(format 
                 (concat ":FUNCTION `%s' " ;-- variable "
                         "-- variable `IS-MON-SYSTEM-P' unbound or non-existent") ,fncn) ;)
              (error                    ;(format 
               (concat ":FUNCTION `%s' "
                       "-- variable `IS-MON-SYSTEM-P' unbound or non-existent") ,fncn) ;)
              ))
           ((memq ,fncn *mon-default-start-load-sanity*)
            (if (or *mon-default-start-load-sanity-WARN-ONLY* ,just-warn)
                (warn                   ;(format 
                 (concat
                  ":FUNCTION `%s' " 
                  "-- eval'd at init is member of `*mon-default-start-load-sanity*'")
                 ,fncn)                 ;)
              (error ;(format 
               (concat ":FUNCTION `%s'"
                       "-- eval'd at init is member of `*mon-default-start-load-sanity*'")
               ,fncn) ;)
              )))
     ,@do-sane
     (unless (or *mon-default-start-load-sanity-WARN-ONLY* ,just-warn)
       (push ,fncn *mon-default-start-load-sanity*))))

;; (put 'mon-default-start-error/sane 'lisp-indent-function <INT>) 
;;
;;,---- :UNCOMMENT-TO-TEST
;;| (let ((IS-MON-SYSTEM-P nil))
;;|   (mon-default-start-error/sane 'SOME-TEST-FNCN-NAME  nil "bubba")
;;`----
;;,---- :UNCOMMENT-TO-TEST
;;| (let ((IS-MON-SYSTEM-P t))
;;|   (mon-default-start-error/sane 'SOME-TEST-FNCN-NAME "bubba")
;;|   (mon-default-start-error/sane 'SOME-TEST-FNCN-NAME "bubba"))
;;`----

;;; ==============================
;; :LOAD-PATH-SETUP

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-02T18:05:42-04:00Z}#{10135} - by MON KEY>
(defun mon-set-load-path-init (&optional warn-only)
  "Set the initial `load-path' and `user-emacs-directory' on MON systems.\n
Adds values of following variables to `load-path':\n
 `*mon-emacs-root*', `*mon-site-lisp-root*',
 `*mon-user-emacsd*', `*mon-naf-mode-root*',
 `*mon-ebay-tmplt-mode-root*'\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane 
   'mon-set-load-path-init warn-only
   (let ((mon-silp `(,*mon-emacs-root*
		     ,*mon-site-lisp-root*
		     ,*mon-user-emacsd*
		     ,*mon-naf-mode-root*
		     ,*mon-ebay-tmplt-mode-root*)))
     (dolist (m-silp mon-silp)
       (add-to-list 'load-path m-silp))
     ;; :NOTE This may cause problems when (not IS-BUG-P-REMOTE).
     (setq user-emacs-directory (file-name-as-directory *mon-user-emacsd*)))))
;; 
;; (mon-set-load-path-init t)
(mon-set-load-path-init)

;;; ==============================
;; :FONT-LOCK/COLOR-THEME

;;; ==============================
;;; :NOTE :AFTER `mon-set-emacs-temp-file/dir-init'
;;; :CREATED <Timestamp: #{2010-04-02T19:09:16-04:00Z}#{10135} - by MON KEY>
(defun mon-set-color-themes-init (&optional warn-only)
  "Initialize font-locking and default `color-themes' on MON systems.\n
Set following variables non-nil:
 `global-font-lock-mode', `font-lock-maximum-decoration',
 `color-theme-is-global'\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane 
   'mon-set-color-themes-init warn-only
   ;; :NOTE This should be set before any code requiring color themes. 
   ;; Loaded here instead of from custom to make sure that it happens now!
   (progn
     (custom-set-variables 
      '(global-font-lock-mode t)
      '(font-lock-maximum-decoration t))
     ;; :NOTE The color-theme library needs to be loaded early _BEFORE_ `naf-mode'!.
     ;;       Eventually we need to move away from the color-theme package it is
     ;;       rapidly becoming obsolete with newer emacs 23.* and breaking changes
     ;;       are in effect.
     (require 'color-theme)
     (color-theme-initialize)
     (custom-set-variables
      '(color-theme-is-global t)
      '(color-theme-legal-frame-parameters "\\(color\\|mode\\|font\\|height\\|width\\)$"))
     (cond  ((and (intern-soft "IS-BUG-P" obarray)  ;; *IS-MON-OBARRAY*
                  (bound-and-true-p IS-BUG-P))
             (color-theme-ld-dark)) ;; (color-theme-euphoria))       
            ((and (intern-soft "IS-MON-P" obarray) ;; *IS-MON-OBARRAY*
                  (bound-and-true-p IS-MON-P))
             (color-theme-ld-dark))))
   ))
;;
;; (mon-set-color-themes-init t)
(mon-set-color-themes-init)

;;; ==============================
;; :SHOW-POINT-MODE
;; :NOTE To ensure we've setup a show-point environment do it now.
;;; :NOTE :AFTER `mon-set-emacs-temp-file/dir-init'
(when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
           (bound-and-true-p IS-MON-SYSTEM-P))
  (require 'show-point-mode))

;;; ==============================
;;; :NOTE Keep this here b/c it is needed when debugging and we want it loaded early.
;;; (add-hook 'emacs-lisp-mode-hook (function (lambda () (mon-toggle-show-point-mode)))
;;; :RENAMED `mon-actvt-show-point-mode' -> `mon-toggle-show-point-mode' 
(defun mon-toggle-show-point-mode ()
  "Toggle `show-point-mode' for current-buffer.\n
Used for hooks othwerwise is equivalent to calling `show-point-mode'.\n
:SEE-ALSO `mon-toggle-dired-dwim-target', `mon-toggle-eval-length',
`mon-toggle-menu-bar', `mon-toggle-truncate-line',
`mon-naf-mode-toggle-restore-llm', `mon-toggle-read-only-point-motion'
`mon-inhibit-modification-hooks', `mon-inhibit-point-motion-hooks',
`mon-inhibit-read-only'.\n►►►"
  (interactive)
  (let ((is-show-point-mode 
         (buffer-local-value show-point-mode (current-buffer))))
    (cond (is-show-point-mode (show-point-mode 0))
          ((not is-show-point-mode) (show-point-mode 1)))))
;;
;;; :TEST-ME (mon-toggle-show-point-mode)

;;; ==============================
;;; :TODO `small-temporary-file-directory'
;;; :NOTE `auto-save-file-name-transforms' 
;;; (set (make-local-variable 'delete-auto-save-files) t)
;;; :CREATED <Timestamp: #{2010-04-02T22:10:18-04:00Z}#{10136} - by MON KEY>
(defun mon-set-emacs-temp-file/dir-init (&optional warn-only)
  "Initialize local Emacs temporary directories on MON systems.\n 
Set these Emacs \"temporary\" directories relative to `*mon-local-emacs-temp-dir*':\n
 `temporary-file-directory', `thumbs-thumbsdir', `thumbs-temp-dir',
 `doc-view-cache-directory', `url-temporary-directory'
 `browse-url-temp-dir', 
When these directories do not exist created them.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO `*mon-local-emacs-temp-dir*'.\n►►►"
  (mon-default-start-error/sane 
   'mon-set-emacs-temp-file/dir-init warn-only
   ;; Make sure the following directories exist first:
   (let* ((temporary-file-directory
           (concat *mon-local-emacs-temp-dir* "/misc-emacs-temp"))
          (thumbs-thumbsdir
           (expand-file-name (format "thumbs-%d" (user-uid))
                             (concat *mon-local-emacs-temp-dir* "/emacs-thumbs")))
          (thumbs-temp-dir 
           (expand-file-name (format "thumbs-%d-temp" (user-uid)) thumbs-thumbsdir))
          (doc-view-cache-directory
           (expand-file-name (format "docview%d" (user-uid)) 
                             (concat *mon-local-emacs-temp-dir* "/docview-cache")))
          (browse-url-temp-dir 
           (expand-file-name  (format "url-%d-temp" (user-uid)) 
                              *mon-local-emacs-temp-dir*))
          (mon-tramp-temp-dir (expand-file-name "tramp" *mon-local-emacs-temp-dir*)))
     (dolist (ensure-dir '(thumbs-thumbsdir thumbs-temp-dir doc-view-cache-directory
                                                 temporary-file-directory browse-url-temp-dir
                                                 mon-tramp-temp-dir))
            (when (null (car (file-attributes (symbol-value ensure-dir))))
              (mkdir (symbol-value ensure-dir) t))))
   (require 'doc-view)
   (custom-set-variables
    ;; :NOTE BZR revno: 102144 2010-10-28 removed `temporary-file-directory' from lisp/files.el
    ;; and added its customomization to lisp/cus-start.el
    ;; If something breaks around here in emacs-24 start there.
    '(temporary-file-directory 
      (concat *mon-local-emacs-temp-dir* "/misc-emacs-temp") t)
    '(thumbs-thumbsdir 
      (expand-file-name (format "thumbs-%d" (user-uid))
                        (concat *mon-local-emacs-temp-dir* "/emacs-thumbs")))
    '(thumbs-temp-dir
      (expand-file-name (format "thumbs-%d-temp" (user-uid)) thumbs-thumbsdir))
    '(doc-view-cache-directory
      (expand-file-name (format "docview%d" (user-uid)) 
                        (concat *mon-local-emacs-temp-dir* "/docview-cache")) t)
    '(browse-url-temp-dir 
      (expand-file-name  (format "url-%d-temp" (user-uid)) 
                         *mon-local-emacs-temp-dir*) t)
    '(url-temporary-directory browse-url-temp-dir))
   ;; :SEE `mon-purge-tramp-persistency-file', `tramp-compat-temporary-file-directory', 
   (when (and (intern-soft "IS-MON-P" obarray) ;;*IS-MON-OBARRAY*
              (bound-and-true-p IS-MON-P))
     (custom-set-variables '(tramp-persistency-file-name 
                             (expand-file-name (format "tramp-persist-%d" (user-uid))
                                               (concat *mon-local-emacs-temp-dir* "/tramp")))))
   ;;'( `slime-temp-directory'  {...} ) this is hardwired to value of `temporary-file-directory'
    ))
;;
;; (mon-set-emacs-temp-file/dir-init t)
(mon-set-emacs-temp-file/dir-init)

;;; ==============================
;; :INFOPATH-SETUP

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-22T17:31:14-04:00Z}#{10121} - by MON KEY>
(defun mon-set-infopath-init (&optional warn-only)
  "Put current running Emacs' info directory on `Info-directory-list'.\n
This is mostly a W32 related fncn. Currenlty does nothing on GNU systems.\n
Evauluated by `mon-run-post-load-hooks'.\n
:NOTE to reset info :SEE info node `(emacs)General Variables'.\n
:NOTE The hook: \(add-hook 'before-init-hook 'w32-init-info\)
in :FILE lisp/w32-fns.el must be commented out for this to work properly.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO `w32-init-info', `mon-get-env-vars-emacs',
`Info-default-directory-list', `Info-directory-list',
`Info-additional-directory-list', `Info-dir-contents',
`Info-default-dirs'.\n►►►"
  (mon-default-start-error/sane 
   'mon-set-info-path-init warn-only
   (when (and (intern-soft "IS-MON-P-W32" obarray)  ;; *IS-MON-OBARRAY*
              (bound-and-true-p IS-MON-P-W32))
     (when (and (intern-soft  "Info-dir-contents" obarray)
                (bound-and-true-p Info-dir-contents))
       (setq Info-dir-contents nil))
     (when (and (intern-soft  "Info-dir-contents-directory" obarray) ;; *IS-MON-OBARRAY*
                (bound-and-true-p Info-dir-contents-directory))
       (setq Info-dir-contents-directory nil))
     (let ((emc-pth (getenv "EMC_PTH"))
           (info-pth (file-truename  (getenv "INFOPATH")))
           gthr-info-pth)
       ;; :NOTE Assume there is only one path specified by INFOPATH env-var.
       ;; When there are more than one, this should be a `split-string' instead.
       (when info-pth 
         (push (concat (replace-regexp-in-string "[:;]$" ""  info-pth) "/") gthr-info-pth))
       (when (and emc-pth (file-directory-p emc-pth))
         (setq emc-pth (file-truename (concat (directory-file-name emc-pth) "/info/")))
         (push emc-pth gthr-info-pth))
       (dolist (gip gthr-info-pth)
         (pushnew gip Info-default-directory-list :test 'equal))
       (when (member "c:/emacs/info/" Info-default-directory-list)
         (setq Info-default-directory-list 
               (delete "c:/emacs/info/" Info-default-directory-list))))
     (setq Info-default-directory-list 
           (delete-dups Info-default-directory-list)))
   ))
;;
;;; :TEST-ME Info-directory-list
;;
;; (mon-set-infopath-init t)
(mon-set-infopath-init)

;;; ==============================
;;; :RENAMED `mon-switch-bookmark-file' -> `mon-set-bookmark-file-init'
;;; :COURTESY stefan@xsteve.at :VERSION 23.01.2001 :HIS xsteve-functions.el
;;; :MODIFICATIONS <Timestamp: 2009-08-09-W32-7T03:31:36-0400Z - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2010-04-02T17:26:44-04:00Z}#{10135} - by MON KEY>
(defun mon-set-bookmark-file-init (&optional new-bk-mrk-file warn-only)
  "Relocate where Emacs stores the `bookmark-default-file' at init time.\n
When evaluated this procedure sets variables as `custom-set-variables'.
The following variables are affected:\n
 `bookmark-default-file' `bookmark-save-flag'\n
When optional arg NEW-BK-MRK-FILE is non-nil it is bookmark filename for the
value of `bookmark-default-file'. Default is \".emacs.bmk\" in the directory
`*mon-user-emacsd*'.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When second optional arg WARN-ONLY is non-nil message a warning instead of an
error if function is already a member of variable
`*mon-default-start-load-sanity*' as per `mon-default-start-error/sane'.\n
A bookmark file typically has a .bmk extension, for additional discussion:
:SEE info node `(emacs)Bookmarks'\n
:SEE-ALSO `bookmark-load', `bookmark-all-names', `bookmark-save-flag',
`bookmark-relocate'.\n►►►"
  (mon-default-start-error/sane 
   'mon-set-bookmark-file-init warn-only
   ;; (setq bookmark-default-file new-bm) (setq bookmark-save-flag 1)
   (let ((new-bm (or new-bk-mrk-file 
                     (concat (file-name-as-directory *mon-user-emacsd*) 
                             (if (and (intern-soft "IS-MON-P-GNU" obarray) ;; *IS-MON-OBARRAY*
                                      (bound-and-true-p IS-MON-P-GNU))
                                 (format ".emacs-%s.bmk" (cdr (mon-gnu-system-conditionals)))
                               ".emacs.bmk")))))
     ;; :NOTE Also settting vanilla variable `bookmark-file' should force
     ;; `defcustom'd `bookmark-default-file' to take its value.
     (and new-bm
          (setq bookmark-file new-bm)
          (set-variable 'bookmark-default-file bookmark-file)
          (custom-note-var-changed 'bookmark-default-file))
     ;;
     ;; (setq bookmark-default-file new-bm)
     ;; (set-variable 'bookmark-default-file 'bookmark-default-file)
     ;; (set-variable 'bookmark-default-file new-bm)
     ;; (put 'bookmark-default-file 'customized-value 
     ;;      (list (custom-quote (eval bookmark-default-file))))
     ;; (custom-note-var-changed 'bookmark-default-file)
     ;; :DEBUGGING (get 'bookmark-default-file 'customized-value)
     (custom-set-variables 
      '(bookmark-save-flag 1)
      ;; :NOTE When bookmark names are getting truncated set the variable nil.
      ;; `bookmark-bmenu-toggle-filenames' is also a <FUNCTION>.
      ;; '(bookmark-bmenu-toggle-filenames nil) 
      ))
   ))
;;
;; (mon-set-bookmark-file-init t)
(mon-set-bookmark-file-init)

;;; ==============================
;; :WOMAN-PATH

;;; ==============================
;;; :TODO Consider adding `convert-standard-filename' here for W32 path frobbing.
;;; :NOTE New cygwin/mingw/emacs installs and twiddling keep manpath wacky on w32.
;;; :MODIFICATIONS <Timestamp: #{2009-10-26T19:12:52-04:00Z}#{09441} - by MON>
;;; :MODIFICATIONS <Timestamp: #{2009-08-14T12:35:21-04:00Z}#{09335} - by MON>
;;; :MODIFICATIONS <Timestamp: #{2010-04-02T19:57:19-04:00Z}#{10135} - by MON KEY>
(defun mon-set-woman-manpath-init (&optional warn-only)
  "Set the `woman-manpath' on MON systems at init time.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:NOTE This is W32 Kludge. The integration of various Emacs/Cygwin/MinGW/MSYS
installs and twiddling keep manpath terminally wacky on w32. The intent here is
to help make it a little less so.\n
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane 
   'mon-set-woman-manpath-init warn-only
   (require 'woman)
   ;; (setq woman-use-own-frame nil)
   (custom-set-variables '(woman-use-own-frame nil))
   (when (and (intern-soft "IS-MON-P-W32" obarray) ;; *IS-MON-OBARRAY*
              (bound-and-true-p IS-MON-P-W32))
     (let ((wmn-p 
            (mapcar #'(lambda (this-csf) 
                        (let ((this-csf-file ;;(convert-standard-filename 
                               (file-truename this-csf)))
                          (unless (null this-csf-file)
                            (when (file-exists-p this-csf-file) this-csf-file))))
                    `( ;; :CROSS-SITE-MAN
                      ,(concat (nth 5 (assoc 1 *mon-emacsd*)) "/cross-site-man")
                       ;; :GNUWIN32
                      ,(when (getenv "SP_GNUW32")
                             (concat (file-name-directory (getenv "SP_GNUW32")) "man"))
                       ;; :MINGW
                      ,(when (getenv "MINGW") 
                             (concat (file-name-directory (getenv "MINGW")) "share/man"))
                       ;; :MSYS
                      ,(when (getenv "MSYS") 
                             (concat (file-name-directory (getenv "MSYS")) "share/man"))
                       ;; :CYGWIN
                       ,@(when (and (getenv "SP_CYGWN") (file-exists-p (getenv "SP_CYGWN")))
                               (let ((cygman-root (file-name-directory  (getenv"SP_CYGWN")))
                                     (cygmans
                                      '("usr/share/man" "usr/local/share/man" 
                                        "usr/X11R6/share/man"
                                       "usr/ssl/man" "usr/local/man"))
                                     cygman-csf)
                                (dolist (csf cygmans (setq cygman-csf (nreverse cygman-csf)))
                                  (push (concat cygman-root csf) cygman-csf))))))))
       (mapc #'(lambda (addp) 
                 (unless (null addp) (add-to-list 'woman-manpath addp))) wmn-p)))
   ))
;;
;; (mon-set-woman-manpath-init t)
(mon-set-woman-manpath-init)

;;; ==============================
;; :EMACS-C-SOURCE-DIRECTORY

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-03T17:03:10-04:00Z}#{10136} - by MON KEY>
(defun mon-set-C-source-directory-init (&optional warn-only)
  "Set source paths at on MON local systems at loadtime.\n
Sets the following variables:\n
 `source-directory', `find-function-C-source-directory'
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO `find-function-C-source-directory', `source-directory',
`find-function-source-path', `internal-doc-file-name', `mon-help-emacs-introspect'.\n►►►"
  (mon-default-start-error/sane 
   'mon-set-C-source-directory-init warn-only
   (cond  ((and (intern-soft "IS-MON-P-W32" obarray)  ;; *IS-MON-OBARRAY*
                (bound-and-true-p IS-MON-P-W32))
           (let ((c-srcs 
                  (file-expand-wildcards (concat (getenv "SP_BIN") "/*/emacs-cur-src/*/src") t)))
             (when (car c-srcs)
               (setq-default source-directory (car c-srcs))
               ;; (require 'find-func)
               (setq find-function-C-source-directory (car c-srcs)))))
          ;; Newly installed/built Emacs may need to have these adjusted accrodingly:
          ;; (setq-default source-directory "/usr/local/*/emacs/src")
          ;; (setq-default find-function-C-source-directory "/usr/local/*/emacs/src")
          ;; :TODO this still needs some tweaking when:
          ;;  a) locating src paths
          ;;  b) binding `find-function-C-source-directory' and `source-directory'
          ;; Right now the some of logic is ordered around the ewiki's example
          ;; shared repo bzr setup, e.g. wildcard expansion of the environment
          ;; variable "DEVHOME".
          ;; 
          ((and (intern-soft "IS-MON-P-GNU" obarray) ;; *IS-MON-OBARRAY*
                (bound-and-true-p IS-MON-P-GNU))
           (let ((c-srcs
                  (or (car (file-expand-wildcards (concat (getenv "DEVHOME") "/ema*/tr*/src") t))
                      ;;(car (file-expand-wildcards (concat *mon-HG-root-path* "/ema*/tr*/src") t))
                      (car (file-expand-wildcards 
                            (concat (file-name-directory *mon-emacs-root*) "/ema*/tr*/src") t)))))
             (when (and c-srcs (file-directory-p c-srcs) (file-readable-p c-srcs))
               (setq find-function-C-source-directory (file-name-as-directory c-srcs))
               (setq source-directory 
                     (file-name-directory (directory-file-name find-function-C-source-directory))))))
          ;; :NOTE Most likely this is already in environ as the
          ;; button-type `help-function-def' has the plist prop
          ;; `help-function` with a lambda form requiring it.  
          ;; (require 'find-func)
          ;;
          ;; (custom-set-variables '(find-function-source-path
          )
   ))
;;
;; (mon-set-C-source-directory-init t)
(mon-set-C-source-directory-init)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-03T17:28:12-04:00Z}#{10136} - by MON KEY>
(defun mon-set-unicodedata-init (&optional warn-only)
  "Set unicode related variables at loadtime.\n
Sets the following variables:
 `describe-char-unicodedata-file'
 `describe-char-unidata-list'\n
:SEE (URL `http://www.unicode.org/Public/UNIDATA/UnicodeData.txt').
:SEE (URL `ftp://www.unicode.org/Public/zipped/5.2.0/UCD.zip').
:SEE-ALSO `*mon-unidata-file-list*', `mon-wget-unicodedata-files',
`mon-help-diacritics', `mon-help-char-representation'.\n►►►"
  (mon-default-start-error/sane 
   'mon-set-unicodedata-init warn-only ;; nil
   (when (and (intern-soft "IS-MON-P" obarray) ;; *IS-MON-OBARRAY*
              (bound-and-true-p IS-MON-P))
     (require 'descr-text)
     (let ((chk-UCF (expand-file-name "UNICODE-DATA/UnicodeData.txt" *mon-site-lisp-root*)))
       (when (file-exists-p chk-UCF)
         ;; :WAS (setq describe-char-unicodedata-file chk-UCF))
         ;;
         ;; (custom-set-variables '(describe-char-unicodedata-file chk-UCF))
         ;;
         ;; (put 'describe-char-unicodedata-file 'customized-value 
         ;;     (list (custom-quote (eval describe-char-unicodedata-file))))
         (set-variable 'describe-char-unicodedata-file chk-UCF)
         (custom-note-var-changed 'describe-char-unicodedata-file))
       ;; (custom-set-variables '(describe-char-unidata-list chk-UCF))
       ;; describe-char-unidata-list 
       (custom-set-variables 
        '(describe-char-unidata-list '(name general-category decomposition 
                                       digit-value numeric-value old-name 
                                       iso-10646-comment)))))
   ))
;;
;; (mon-set-unicodedata-init)
;; (mon-set-unicodedata-init)

;;; ==============================
;; :EXECUTABLES
;;; :NOTE Following are inits that rely on external executables in path.

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-02T23:08:06-04:00Z}#{10136} - by MON KEY>
(defun mon-set-doc-view-programs-init (&optional warn-only)
  "Initialize doc-view related programs on MON systems when `IS-MON-P-W32'.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
Set the following variables:\n
 `doc-view-dvipdf-program', `doc-view-dvipdfm-program',
 `doc-view-pdftotext-program', `doc-view-ghostscript-program',
 `doc-view-ps2pdf-program'\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO `mon-get-pdfinfo', `mon-get-pdftotext', `mon-get-ps2ascii',
`*mon-pdftotext-exec-path*', `*mon-pdfinfo-exec-path*'.\n►►►"
  (mon-default-start-error/sane 
   'mon-set-doc-view-programs-init warn-only ;; nil
   (when (and (intern-soft "IS-MON-P-W32" obarray) ;; *IS-MON-OBARRAY*
              (bound-and-true-p IS-MON-P-W32))
     (custom-set-variables
      '(doc-view-dvipdf-program nil)
      '(doc-view-dvipdfm-program 
        (or 
         (and 
          (file-exists-p (file-truename (concat (getenv "SP_TEXLIV") "\\dvipdfm.exe")))
          (file-truename (concat (getenv "SP_TEXLIV") "\\dvipdfm.exe")))
         (executable-find "dvipdfm")))
      '(doc-view-ghostscript-program 
        (or 
         (and 
          (file-exists-p (file-truename (concat (getenv "SP_GS") "\\gs8.64\\bin\\gswin32c.exe")))
          (file-truename (concat (getenv "SP_GS") "\\gs8.64\\bin\\gswin32c.exe")))
         (executable-find "gswin32c")))
      '(doc-view-pdftotext-program
        (or 
         (and 
          (file-exists-p (file-truename (concat (getenv "SP_TEXLIV")"\\pdftotext.exe")))
          (file-truename (concat (getenv "SP_TEXLIV")"\\pdftotext.exe")))
         (and 
          (file-exists-p (file-truename (concat (getenv "SP_XPDF") "\\pdftotext.exe")))
          (file-truename (concat (getenv "SP_XPDF") "\\pdftotext.exe")))
         (executable-find "pdftotext")))
      '(doc-view-ps2pdf-program
        (or 
         (and 
          (file-exists-p (file-truename (concat (getenv "SP_TEXLIV") "\\ps2pdf.exe")))
          (file-truename (concat (getenv "SP_TEXLIV") "\\ps2pdf.exe")))
         (executable-find "ps2pdf")))
      ))
   ))
;;
;; (mon-set-doc-view-programs-init t)
(mon-set-doc-view-programs-init)


;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-04-02T20:00:29-04:00Z}#{10136} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-10-21T20:01:43-04:00Z}#{09434} - by MON>
(defun mon-set-thumbs-conversion-program-init (&optional warn-only)
  "Set the `thumbs-conversion-program' on MON systems at init time.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:NOTE When `IS-W32-P' is non-nil may employ w32 related kludges around binding
of the ImageMagick executables convert.exe and imconvert.exe\n
:SEE :FILE mon-rename-image-utils.el for additional discussion.\n
:SEE :FILE lisp/image-file.el lisp/thumbs.el mon-boxcutter.el\n
:SEE \(woman \"xwud\"\) \(woman \"xwd\"\)\n
:SEE-ALSO `thumbs-conversion-program', `*boxcutter-path*', `*boxcutter-captures*'
`boxcutter-capture', `mon-set-boxcutter-init', `mon-set-image-dired-init'.\n►►►"
  (mon-default-start-error/sane 
   'mon-set-thumbs-conversion-program-init warn-only ;; nil ;; just-warn
   ;; :IMAGE-FILE lisp/image-file.el
   (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
              (bound-and-true-p IS-MON-SYSTEM-P))
     (or (and (intern-soft "IS-W32-P" obarray) ;; *IS-MON-OBARRAY*
              (bound-and-true-p IS-W32-P)
         ;; :NOTE Imagemagick is supplied with Emacs Emacs 24. at which point we
         ;; can prob. re-enable svg for w32 (assuming libsvg or equiv) is no
         ;; longer a required dependency.
              (custom-set-variables       
               '(image-file-name-extensions 
                 '("jpeg" "jpg" "gif" "tiff" "tif" 
                   "xbm" "xpm" "pbm" "pgm" "ppm" "pnm"
                   "nef" "bmp" "png")))
               ;; :THUMBS lisp/thumbs.el
               ;; :TODO If env "SP_IMGCK" isn't set but we _did_ find imconvert 
               ;; (setenv "SP_IMGCK") -> `thumbs-conversion-program'       
              (let ((tcp (if (getenv "SP_IMGCK")    
                              (let ((xp-convert (car (directory-files (getenv "SP_IMGCK") t "imconvert"))))
                                (if xp-convert xp-convert (executable-find "imconvert")))
                            (executable-find "imconvert"))))
                 (or ;; don't leave the parent or branch
                  (when tcp 
                    (set-variable 'thumbs-conversion-program tcp)
                    (custom-note-var-changed 'thumbs-conversion-program))
                  t)))
         (and (intern-soft "IS-MON-P-GNU")
              (bound-and-true-p IS-MON-P-GNU)
              ;; progn is to prevent exiting the and too early
              (progn (set-variable 'thumbs-conversion-program (executable-find "convert"))
                     (custom-note-var-changed 'thumbs-conversion-program)
                     t)
              (custom-set-variables
               '(image-file-name-extensions
                 '("png" "jpeg" "jpg" "gif" "tiff" "tif" 
                   "xbm" "xpm" "pbm" "pgm" "ppm"
                   "pnm" "nef" "bmp" "png" "svg"))))))
   ))
;;
;; (mon-set-thumbs-conversion-program-init t)
(mon-set-thumbs-conversion-program-init)

;;; ==============================
;;; :CHANGESET 2292
;;; :CREATED <Timestamp: #{2010-11-09T20:36:53-05:00Z}#{10452} - by MON KEY>
(defun mon-set-boxcutter-init (&optional warn-only)
  "Set the `*boxcutter-captures*' paths on MON systems at init time.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE :FILE mon-boxcutter.el
:SEE-ALSO `boxcutter-mkdir-loadtime', `mon-set-image-dired-init',
`mon-set-thumbs-conversion-program-init'.\n►►►"
  (mon-default-start-error/sane 
   'mon-set-boxcutter-init warn-only
   (custom-set-variables
    '(*boxcutter-path*
      (or (getenv "SP_BXC")
          (when (or (executable-find "boxcutter.exe") 
                    (executable-find "boxcutter-fs.exe"))
            (directory-file-name
             (file-name-directory ;; (file-name-sans-extension  
              (or (executable-find "boxcutter.exe") 
                  (executable-find "boxcutter-fs.exe")))))
          ;; :NOTE In case we want to create the API for imagemagick's import.
          ;; 9/10 times on a GNU this will return /usr/bin but we do it here
          ;; for the sake of consistency...
          (when (and (intern-soft "IS-MON-P-GNU" obarray) ;; *IS-MON-OBARRAY*
                     (bound-and-true-p IS-MON-P-GNU))
            (when (executable-find "import")
              (directory-file-name 
               (file-name-directory (executable-find "import")))))))
    '(*boxcutter-captures*
      (or 
       (let ((is-func (cadr (assoc 'the-boxcutter-pth *mon-misc-path-alist*))))
         (when (functionp is-func)
           (funcall is-func)))
       (expand-file-name  "Screenshots" user-emacs-directory))))))


;;; ==============================
;;; :TODO Finish! Adjust imconvert/convert conditionals appropriately and build
;;; command according to system-type
;;; :CHANGESET 2331
;;; :CREATED <Timestamp: #{2010-11-29T16:22:54-05:00Z}#{10481} - by MON KEY>
(defun mon-set-image-dired-init (&optional warn-only)
  "Set the image-dired related customizations on MON systems at init time.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE :FILE lisp/image-dired.el\n
:SEE-ALSO `mon-set-boxcutter-init', `mon-set-thumbs-conversion-program-init'.\n►►►"  
  (mon-default-start-error/sane 
   'mon-set-image-dired-init warn-only
   (require 'image-dired)
   ;; (convert-standard-filename (concat (getenv "SP_GNUW32") "/pngcrush.exe"))
   (getenv "SP_EXIF")
   ;; (progn 
   ;;   (set-variable 'image-dired-cmd-create-standard-thumbnail-command  {....} )
   ;;   (custom-note-var-changed 'image-dired-cmd-create-standard-thumbnail-command))
   (setq image-dired-cmd-create-standard-thumbnail-command 
         (concat "imconvert -size %wx%h \"%f\" -thumbnail \"%wx%h>\" png:\"%t\" ;"
                 (convert-standard-filename (concat (getenv "SP_GNUW32") "/pngcrush.exe"))
                 " -q -text b "
                 "\"Description\" \"Thumbnail of file://%f\" -text b "
                 "\"Software\" \"GNU Emacs\" -text b \"Thumb::MTime\" \"%m\" -text b "
                 "\"Thumb::URI\" \"file://%f\" %q %t ; rm %q"))
   (custom-set-variables 
    ;; :IMAGE-DIRED-LOCAL-PATHS
    ;; :DEFAULT (locate-user-emacs-file "image-dired/")    
    ;; '(image-dired-dir "") 
    ;;
    ;; :DEFAULT (locate-user-emacs-file "image-dired/.image-dired_db")
    ;; '(image-dired-db-file "") 
    ;;
    ;; :DEFAULT (locate-user-emacs-file "image-dired/.image-dired_temp")
    ;; '(image-dired-temp-image-file "")
    ;;
    ;; :DEFAULT (locate-user-emacs-file "image-dired/.image-dired_rotate_temp")
    ;; '(image-dired-temp-rotate-image-file  )       
    ;;
    ;; :DEFAULT (locate-user-emacs-file "image-dired/.image-dired_gallery")
    ;; '(image-dired-gallery-dir ) 
    ;;
    ;; :IMAGE-DIRED-URL-PATHS
    ;;
    ;; :DEFAULT "http://your.own.server/image-diredpics"
    ;; '(image-dired-gallery-image-root-url )
    ;;
    ;; :DEFAULT "http://your.own.server/image-diredthumbs"
    ;; '(image-dired-gallery-thumb-image-root-url )
    ;;
    ;; :DEFAULT "~/pics/"
    ;; (image-dired-main-image-directory )
    ;;    
    ;; :IMAGE-DIRED-OPTIONS
    ;;
    ;; :DEFAULT 'use-image-dired-dir ;; standard | use-image-dired-dir | per-directory   
    '(image-dired-thumbnail-storage 'per-directory) 
    ;;
    ;; :DEFAULT (list "private" "hidden" "pending")
    ;; (image-dired-gallery-hidden-tags )
    ;;
    ;; :DEFAULT (if (eq 'standard image-dired-thumbnail-storage) 128 100)
    ;; '(image-dired-thumb-size )
    ;;
    ;; :DEFAULT image-dired-thumb-size
    ;; '(image-dired-thumb-width )
    ;;
    ;; :DEFAULT  image-dired-thumb-size
    ;; '(image-dired-thumb-height
    ;;
    ;; :DEFAULT 2
    ;; (image-dired-thumb-relief )
    ;;
    ;; :DEFAULT 2
    ;; (image-dired-thumb-margin )
    ;;
    ;; :DEFAULT 'dynamic {dynamic|fixed|interacitve|none)
    ;; (image-dired-line-up-method )
    ;;
    ;; :DEFAULT  3
    ;; '(image-dired-thumbs-per-row )
    ;;
    ;; :DEFAULT 1
    ;; (image-dired-display-window-width-correction )
    ;;
    ;; :DEFAULT 0
    ;; (image-dired-display-window-height-correction )
    ;;
    ;; :DEFAULT t
    ;; image-dired-track-movement )
    ;;
    ;; :DEFAULT nil
    ;; (image-dired-append-when-browsing )
    ;;
    ;; :DEFAULT t
    ;; (image-dired-dired-disp-props )
    ;;
    ;; :DEFAULT  50
    ;; (image-dired-show-all-from-dir-max-files )
    ;;
    ;;
    ;; :IMAGE-DIRED-COMMAND-STRINGS
    ;;
    ;; :DEFAULT "%p -size %wx%h \"%f\" -resize \"%wx%h>\" -strip jpeg:\"%t\""
    ;; '(image-dired-cmd-create-thumbnail-options )
    ;;
    ;; :DEFAULT "%p -rotate %d \"%t\""
    ;; '(image-dired-cmd-rotate-thumbnail-options )  
    ;;
    ;; :DEFAULT "%p -rotate %d -copy all -outfile %t \"%o\""
    ;; '(image-dired-cmd-rotate-original-options )   
    ;;
    ;; :DEFAULT  "%p -size %wx%h \"%f\" -resize \"%wx%h>\" -strip jpeg:\"%t\""
    ;; '(image-dired-cmd-create-temp-image-options )
    ;;
    ;; :DEFAULT   "%p -%t=\"%v\" \"%f\""
    ;; '(image-dired-cmd-write-exif-data-options )
    ;;
    ;; :DEFAULT   "%p -s -s -s -%t \"%f\""
    ;; '(image-dired-cmd-read-exif-data-options )
    ;;
    ;; :DEFAULT  "%b: %f (%t): %c"
    ;; (image-dired-display-properties-format )
    ;;
    ;; :IMAGE-DIRED-PROGRAMS
    ;;
    ;; :DEFAULT "display"  {"display "xli" "qiv -t"}
    ;; (image-dired-external-viewer )
    ;;
    ;; :DEFAULT (executable-find "pngcrush")
    ;; :SEE (URL `http://gnuwin32.sourceforge.net/packages/pngutils.htm')
    ;; '(image-dired-cmd-pngcrush-program "")
    ;;
    ;; :DEFAULT "jpegtran" (executable-find "jpegtran")
    ;; '(image-dired-cmd-rotate-original-program  )  
    ;;
    ;; :DEFAULT (executable-find "pngnq")
    ;; '(image-dired-cmd-pngnq-program "")
    ;;
    ;; :DEFAULT "exiftool" (executable-find "exiftool")
    ;; '(image-dired-cmd-write-exif-data-program )
    ;;
    ;; :DEFAULT "mogrify"
    ;; '(image-dired-cmd-rotate-thumbnail-program )
    ;;
    ;;  :DEFAULT "convert"
    ;; '(image-dired-cmd-create-thumbnail-program )
    '(image-dired-cmd-create-thumbnail-program "imconvert")
    ;;
    ;; :DEFAULT "convert"
    '(image-dired-cmd-create-temp-image-program "imconvert")
    ;;
    
    )))

 
;;; ==============================
;;; :ISPELL

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-03T17:10:18-04:00Z}#{10136} - by MON KEY>
(defun mon-set-ispell-init (&optional warn-only)
  "Set the ispell related prefs on MON local systems.\n
Set the following variables:
 `ispell-program-name'\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane 
   'mon-set-ispell-init warn-only
   (custom-set-variables
    '(ispell-program-name 
      (cond ((and (intern-soft "IS-MON-P-GNU" obarray) ;; *IS-MON-OBARRAY*
                  (bound-and-true-p IS-MON-P-GNU))
             (executable-find "aspell"))
            ((and (intern-soft "IS-W32-P" obarray) ;; *IS-MON-OBARRAY*
                  (bound-and-true-p IS-W32-P))
             (or (convert-standard-filename (executable-find "aspell"))
                 (convert-standard-filename (concat (getenv "SP_ASPLL") "\\aspell.exe")))))))
   ))
;;
;; (mon-set-ispell-init t)
(mon-set-ispell-init)

;;; ==============================
;; :PRE-CUSTOM-CUSTOMIZATIONS
;;; :NOTE Start setting customizations that we want before loading custom file.

;;; ==============================
;; :HELP-MODE-MODS

;;; ==============================
;;; :NOTE defadvice `find-function-search-for-symbol', `find-variable-noselect'
;;; :COURTESY Tom Rauchenwald 
;;; :CREATED <Timestamp: Thursday July 30, 2009 @ 06:17.42 PM - by MON>
;;; :SEE `NOTES:' section of this file's header for discussion.
;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-02T20:19:37-04:00Z}#{10136} - by MON KEY>
(defun mon-set-help-mode-init (&optional warn-only)
  "Initialize `help-mode' related preferences on MON systems at init time.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
Ads advice for `find-variable-noselect' and `find-function-search-for-symbol'.\n
Set `help-downcase-arguments' non-nil.\n
Set `print-length' null on the `help-mode-hook'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane
   'mon-set-help-mode-init warn-only 
   ;; :NOTE I don't like the new 23.0 style UPCASE args in help.  Where clarity
   ;;        is the concern would be nicer to map them to a face.
   ;; (setq help-downcase-arguments t)
   (custom-set-variables 
    '(help-downcase-arguments t t)
    ;; :NOTE May slow apropos down by ~10-20%
    '(apropos-do-all t))
   ;;
   ;; :NOTE Ensure evaluating examples in help mode returns a nice long list.
   (add-hook 'help-mode-hook 
             (function (lambda () 
                         (set (make-local-variable 'print-length) nil))))
   ;;
   (defadvice find-function-search-for-symbol 
       ;; CLASS NAME   [POSITION] [ARGLIST]             FLAG
       (after mon-adv1 last     (symbol type library) activate)
     "When in a `help-mode' buffer, regions with the property category and prop-val
`help-function-def-button' are advised such that selecting it visits the file
returned from `find-function-search-for-symbol' is in `view-mode'.\n 
:SEE-ALSO `describe-function', `find-lisp-object-file-name', `help-C-file-name',
`find-variable-noselect'.\n►►►"
     (with-current-buffer (car ad-return-value)
       (unless view-mode (view-mode 1))))
   ;;
   (defadvice find-variable-noselect 
       ;; CLASS NAME   [POSITION] [ARGLIST]             FLAG
       (after mon-adv2 last (variable &optional file) activate)
     "When in a `help-mode' buffer, regions with the property category and prop-val
`help-variable-def-button' are advised such that selecting it visits the file
returned from `find-variable-noselect' is in `view-mode'.\n
:SEE-ALSO `variable-at-point', `find-function-search-for-symbol'.\n►►►"
     (with-current-buffer (car ad-return-value)
       (unless view-mode (view-mode 1))))
   ))
;;
;; (mon-set-help-mode-init t)
(mon-set-help-mode-init)

;;; ==============================
;;; :NOTE (global-set-key [(f12)] 'ibuffer)
;;; :CREATED <Timestamp: #{2010-04-02T19:12:47-04:00Z}#{10135} - by MON KEY>
(defun mon-set-ibuffer-init (&optional warn-only)
    "Set ibuffer preferences on MON systems at init time.\n
Sets or changes the following ibuffer variables:
 `ibuffer-shrink-to-minimum-size', `ibuffer-shrink-to-minimum-size',
 `ibuffer-always-show-last-buffer', `ibuffer-sorting-mode',
 `ibuffer-use-header-line'\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane 
   'mon-set-ibuffer-init warn-only
   (custom-set-variables
    '(ibuffer-default-shrink-to-minimum-size t)
    '(ibuffer-always-show-last-buffer nil)
    '(ibuffer-use-header-line t))))
;;
;; (mon-set-ibuffer-init t)
(mon-set-ibuffer-init)

;;; ==============================
;;; :CHANGESET 1747 <Timestamp: #{2010-05-19T17:55:18-04:00Z}#{10203} - by MON KEY>
;;; :CREATED <Timestamp: #{2010-04-03T17:33:11-04:00Z}#{10136} - by MON KEY>
(defun mon-set-ido-init (&optional warn-only)
  "Set `ido-mode' preferences on MON systems at init time.\n
Set the following variables:\n
Set variable `ido-enable-prefix' non-nil\n 
Set variable `ido-enable-flex-matching' non-nil\n
Set `ido-everywhere' non-nil\n
Set variable `ido-mode' to 'both\n
Set variable `ido-save-directory-list-file' back to its default path:\n
  \"~/.emacs.d/ido/.ido.last\"\n
:NOTE This is required on MON systems because we normally load user settings
from a non-default location, however since this is a \"caching\" related
variable it needs to be fast and shouldn't do any blocking or network seeks.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO `'.\n►►►"
  (mon-default-start-error/sane
   'mon-set-ido-init warn-only
   ;; :NOTE .ido.last used to be "ido_last"
   (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
              (bound-and-true-p IS-MON-SYSTEM-P))
     (add-to-list 'completion-ignored-extensions "lost+found/" t)
     (require 'ido)
     (let ((mk-ido-d (concat (file-truename (getenv "HOME")) "/.emacs.d/ido")))
       (unless (file-directory-p mk-ido-d) (mkdir mk-ido-d))
       (unless (file-exists-p (concat mk-ido-d "/.ido.last"))
         (with-temp-file (concat mk-ido-d "/.ido.last") (newline)))
       (setq mk-ido-d (concat mk-ido-d "/.ido.last"))
       (set-variable 'ido-save-directory-list-file mk-ido-d)
        ;; (put 'ido-save-directory-list-file 'customized-value 
        ;;     (list (custom-quote (eval ido-save-directory-list-file))))
       (custom-note-var-changed 'ido-save-directory-list-file)
       ;; :DEBUGGING (get 'ido-save-directory-list-file 'customized-value)
       (custom-set-variables 
        ;; `(ido-save-directory-list-file ,mk-ido-d t)
         '(ido-everywhere nil t)
         '(ido-enable-prefix 1 t)
         '(ido-enable-flex-matching 1 t)
         '(ido-mode 'both t)
         ;; :NOTE Sometimes ido should ignore the *Completions* buffer.
         ;; (add-to-list 'ido-ignore-buffers "\\*Completions\\*")
         ;;
         ;; ido-ignore-buffers          ;; :DEFAULT ("\\` ") <REGEXP>|<FUNCTION> 

         ;; (defun mon-ido-ignore-*Completions* ()
         ;; (minibufferp (current-buffer))
         ;;
         ;; :NOTE Following are options for `ido-default-file-method' and `ido-default-buffer-method'
         ;; selected-window other-window display other-frame maybe-frame raise-frame
         ;; :SEE  `ido-default-file-method', `ido-find-file' `ido-switch-buffer'         
         ;; ido-default-file-method     ;; :DEFAULT 'raise-frame 
         ;; ido-default-buffer-method   ;; :DEFAULT 'raise-frame 
         ;;
         ;; ido-max-window-height       ;; :DEFAULT nil 
         ;; ido-max-prospects           ;; :DEFAULT 12
         ;; ido-max-work-file-list      ;; :DEFAULT 10
         ;; ido-max-dir-file-cache      ;; :DEFAULT 100     
         ;; ido-max-work-directory-list ;; :DEFAULT 50
         ;; ido-auto-merge-work-directories-length ;; :DEFAULT 0 -- merge when 0 chars don't match 
         ))
     (cond ((and (intern-soft "IS-MON-P-W32" obarray) ;; *IS-MON-OBARRAY*
                 (bound-and-true-p IS-MON-P-W32))
            (custom-set-variables 
             '(ido-work-directory-list-ignore-regexps 
               '("c:/WINDOWS/.NtUninstall.*") t)))
           ((and (intern-soft "IS-MON-P-GNU" obarray) ;; *IS-MON-OBARRAY*
                 (bound-and-true-p IS-MON-P-GNU))
            (custom-set-variables 
             '(ido-work-directory-list-ignore-regexps  
               (list ".*lost+found.*" 
                     (concat ".*" (regexp-opt '(".git" ".bzr" ".hg" ".svn" "_darcs" "RCS" "CVS" "rcs" "cvs"))))
               t)))))
   ))
;;
;; (mon-set-ido-init t)
(mon-set-ido-init)


;;; ==============================
;;; :CHANGESET 2363
;;; :CREATED <Timestamp: #{2010-12-20T21:28:31-05:00Z}#{10511} - by MON KEY>
(defun mon-set-org-init (&optional warn-only)
  (mon-default-start-error/sane
   'mon-set-org-init warn-only
   ;; org-cycle-emulate-tab
   ;; show-subtree
   ;; show-all
   ;; org-cycle-include-plain-lists
   (custom-set-variables '(org-startup-folded 'showall)
                         ;; 'showeverything
                         ;; 'content
                         ;; `org-startup-options'
                         )
   ;; Org attach steals this binding form me:
   (add-hook 'org-mode-hook 
             (function (lambda ()
                         (local-unset-key "\C-c\C-a")
                         (local-set-key "\C-c\C-a" 'show-all))))
   ))

(mon-set-org-init)

;;; ==============================
;; :CUSTOM-FILE

;;; ==============================
;;; :CHANGESET 2389
;;; :CREATED <Timestamp: #{2011-01-12T14:25:03-05:00Z}#{11023} - by MON KEY>
(defun mon-set-custom-customizations-init (&optional warn-only)
  "Set custom customizations on MON systems.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO `mon-set-customizations-before-custom-file-init'.\n►►►"
  (mon-default-start-error/sane
   'mon-set-custom-customizations warn-only
   '(custom-set-variables
     '(custom-buffer-done-kill t) ;; This one is a modern Emacs miracle!
     '(custom-unlispify-tag-names nil) ;; :DEFAULT t
     '(custom-unlispify-menu-entries nil) ;; :DEFAULT t
     ;; '(custom-guess-doc-alist ;; :DEFAULT (("\\`\\*?Non-nil " boolean))
     ;; '(custom-buffer-style 'links)     ;; :DEFAULT 'links {brackets}
     ;; '(custom-unlispify-remove-prefixes nil) :DEFAULT nil
     ;; '(custom-buffer-verbose-help t)   ;; :DEFAULT t
     ;; '(custom-buffer-indent  3)        ;; :DEFAULT 3
     ;; '(custom-menu-order-groups 'first) ;; :DEFAULT 'first
     ;; '(custom-magic-show-hidden  '(option face)) :DEFAULT '(option face) {group}
     ;; '(custom-face-default-form 'selected) ;; :DEFAULT 'selected {all lisp}
     )
   (dolist (cgna-D '(;; ("<MATCH-STRING>\\'" [ <SYMBOL> | <LIST> ] )
                     ("\\`\\*.*\\*\\'" variable)  ;; `*earmuffed-var*'
                     ("\\`[%$]"        function)  ;; `%private-fun', `$internal-fun'
                     ("!\\'"           function)  ;; `set-foo!', `mutate-bar!'
                     ("\\`\\+.*\\+\\'" variable)  ;; `+superior-lisp+', `+constant-name+'
                     ("\\`-"           variable)) ;; `--some-var--', `-her-buddy-', `--his-pal'
                   (custom-note-var-changed 'custom-guess-name-alist))
     (add-to-list 'custom-guess-name-alist cgna-D t))
   ))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-03T15:56:38-04:00Z}#{10136} - by MON KEY>
(defun mon-set-customizations-before-custom-file-init (&optional warn-only)
  "Set custom customizations before loading the custom file on MON systems.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO `mon-set-custom-customizations-init'.\n►►►"
  (mon-default-start-error/sane
   'mon-set-customizations-before-custom-file-init warn-only
   (fset 'yes-or-no-p 'y-or-n-p)
   (custom-set-variables 
    ;; :PATHS-FILES
    ;; '(delete-by-moving-to-trash t)
    ;; :EPG 
    ;; :SEE-FILE lisp/epg-config.el
    ;; :NOTE Prefer gpg v 1.4.3 see `epg-gpg-minimum-version'
    ;; :SEE-ALSO `epg-gpg-program', `epg-gpgsm-program',
    ;; `epg-gpg-minimum-version', `epg-debug', `epg-gpg-home-directory',
    ;; `epg-passphrase-coding-system',
    ;; (epg-configuration) (epg-check-configuration (epg-configuration)) 
    '(kill-read-only-ok 1 t)
    '(auto-save-default t)  ;; :DEFAULT t ;; :SEE `auto-save-file-name-transforms'
    '(dired-recursive-deletes t)
    '(dired-recursive-copies 'top)
    '(confirm-kill-emacs 'y-or-n-p)
    '(history-delete-duplicates t) ;; Keep an eye on this to make sure it doesn't slow things down 
    ;; '(history-add-new-input t) ;; :DEFAULT t
    ;; '(history-length 30)       ;; :DEFAULT 30 :NOTE This appears as a property on any of the standard minibuffer history vars
    '(enable-recursive-minibuffers 1)
    '(require-final-newline t)      ;; t|visit-save|visit :SEE `mode-require-final-newline', `require-final-newline'
    '(shell-input-autoexpand t)     ;; comint-input-autoexpand
    '(auto-image-file-mode 1)
    ;; :YANK/CLIP    
    '(yank-excluded-properties t)
    '(x-select-enable-clipboard 1) ;; :SEE :FILE lisp/w32-vars.el
    '(delete-selection-mode nil)
    '(transient-mark-mode 1)
    ;; :COLUMNS
    '(fill-column 80)
    '(column-number-mode t)
    ;; :TIME
    '(display-time-mode 1)
    '(display-time-day-and-date 1)
    '(display-time-24hr-format 1)
    ;; :EOL-MNEMONICS
    '(eol-mnemonic-dos "(w-crlf)")
    '(eol-mnemonic-mac "(mac-cr)")
    '(eol-mnemonic-undecided "(?eol?)")
    '(eol-mnemonic-unix "(unx-lf)")
    ;; :DISPLAY-ANNOYANCES
    '(size-indication-mode 1)
    '(initial-buffer-choice t) ;; :NOTE `t' finds *scratch*
    '(inhibit-startup-message 1) 
    '(ring-bell-function 'ignore)
    '(cursor-type '(bar . 3))
    '(cursor-in-non-selected-windows nil)
    '(visual-line-fringe-indicators '(nil right-curly-arrow))
    '(nxml-slash-auto-complete-flag t)
    ) ;; :CLOSE `custom-set-variables' for `IS-MON-SYSTEM-P'
   (when (and (intern-soft "IS-MON-P" obarray) ;; *IS-MON-OBARRAY*
              IS-MON-P)
     (custom-set-variables 
      '(ffap-rfc-directories
        (concat (nth 5 (mon-get-mon-emacsd-paths)) "/RFCs-HG"))
      '(nxhtml-load nil)
      '(initial-major-mode 'emacs-lisp-mode)
      '(mouse-avoidance-mode 'exile)
      '(backward-delete-char-untabify-method 'hungry)) ;; 'all
     ;; Make sure shell-mode understands `ls dircolors e.g. ~/.bashrc's alias:
     ;; ls="ls --color=auto"
     ;; :NOTE autoloaded in :FILE lisp/ansi-color.el
     (ansi-color-for-comint-mode-on)
     ;; :SEE `edebug-form-spec' 
     ;; :SEE (info "(elisp Instrumenting Macro Calls")
     ;; (edebug-eval-macro-args t) 
     )
   ))
;;
;; (mon-set-customizations-before-custom-file-init t)
(mon-set-customizations-before-custom-file-init)

;;; ==============================
;;; :CHANGESET 2120
;;; :CREATED <Timestamp: #{2010-09-13T13:36:44-04:00Z}#{10371} - by MON KEY>
(defun mon-set-custom-file-init-w32-configs (&optional warn-only)
  "Set w32 specific configs at init time.\n
:SEE-ALSO `mon-keybind-w32-init', `mon-keybind-put-hooks-init'.\n►►►"
  (when (and (intern-soft "IS-W32-P" obarray)  ;; *IS-MON-OBARRAY*
             (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
             (bound-and-true-p IS-MON-SYSTEM-P)
             ;;(featurep 'emacsw32)
             )
    (when (not (and (intern-soft "IS-MON-P" obarray) ;; *IS-MON-OBARRAY*
                    (bound-and-true-p IS-MON-P)))
      (custom-set-variables
       '(initial-major-mode 'fundamental-mode)
       '(delete-by-moving-to-trash t)
       ;; Supposedly this is deprecated post 23.2
       ;; '(default-buffer-file-coding-system 'utf-8-unix t) 
       ;;
       ;; <Timestamp: #{2010-11-09T16:02:26-05:00Z}#{10452} - by MON KEY>
       '(current-language-environment "UTF-8")
       '(buffer-file-coding-system 'utf-8-unix t)
       '(default-buffer-file-coding-system 'utf-8-unix)
       ) 
      (setq-default buffer-file-coding-system 'utf-8-unix)
      (setq-default default-buffer-file-coding-system 'utf-8-unix))
    (custom-set-variables
     '(dired-guess-shell-gnutar nil)
     '(w32shell-cygwin-bin "C:\\bin")
     '(w32shell-shell 'cmd)
     ;; :NOTE Should prop. do a feauture check on these first:
     '(emacsw32-eol-check-before-save t)
     '(emacsw32-eol-check-new-files t)
     '(emacsw32-eol-file-name-lf-list 
       '(".x?html?$" ".css$" ".js$" ".pl$" ".pm$" ".el" ".text$" ".txt$" ".naf$" ".dbc$"))
     '(emacsw32-max-frames t)
     ;; '(emacsw32-style-frame-title t)
     '(emacsw32-style-frame-title nil)
     ;; '(tramp-default-method "plinkx")
     ;; '(tramp-default-method-alist (quote (("\\`localhost\\'" "\\`root\\'" "su"))))
     )))
;;
(mon-set-custom-file-init-w32-configs)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-02T17:36:42-04:00Z}#{10135} - by MON KEY>
(defun mon-set-custom-file-init (&optional warn-only)
  "Set the default `custom-file' to an alternate location at init time.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
:NOTE This helps to maintain portability, the location of custom is apparently
arbitrary across systems e.g. Debian, w32, etc. esp. where upgrades/multiple
installations are concerned. Likewise, as the custom facility generally sucks
and apt to overwrite my configs at _its_ perogative I find it cleaner and often
helpful to keep a version controlled copy in a location of _my_ choosing.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:EXAMPLE\n\n\(custom-file\)\n
:SEE-ALSO `customize-rogue'.\n►►►"
  (mon-default-start-error/sane
   'mon-set-custom-file-init warn-only
   (let* ((*mon-user-emacsd* (file-name-as-directory *mon-user-emacsd*))
          (mscf (cond ((and (intern-soft "IS-MON-P-W32" obarray) ;; *IS-MON-OBARRAY*
                            (bound-and-true-p IS-MON-P-W32))
                       (concat *mon-user-emacsd* (nth 2 (assoc 1 *mon-emacsd*))))
                      ((and (intern-soft "IS-BUG-P" obarray)  ;; *IS-MON-OBARRAY*
                            (bound-and-true-p IS-BUG-P))
                       (concat *mon-user-emacsd* (nth 2 (assoc 3 *mon-emacsd*))))
                      ((and (intern-soft "IS-MON-P-GNU" obarray) ;; *IS-MON-OBARRAY*
                            (bound-and-true-p IS-MON-P-GNU))
                       (concat *mon-user-emacsd* (nth 2 (assoc 2 *mon-emacsd*)))))))
     (setq custom-file mscf))
   ;; :NOTE Eval'ing: (load custom-file t) the `t' flips the `no-error'.
   (load custom-file)
   ))
;;
;; (mon-set-custom-file-init t)
(mon-set-custom-file-init)

;;; ==============================
;;; :NOTE Make sure that calling functions tack on the ``file://'' prefix.
;;; Also, make sure to check that the path is constrcted correctly for browsers.
;;;       (concat "file://" common-lisp-hyperspec-root)
;;;       (substring (concat "file://" common-lisp-hyperspec-root) 7)
;;;       (concat "file://" common-lisp-hyperspec-root "Body/03_da.htm")
;;; "http://www.ai.mit.edu/projects/iiip/doc/CommonLISP/HyperSpec/"
;;; "http://www.lispworks.com/documentation/HyperSpec/"
;;; "http://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/html/hyperspec/HyperSpec/"
;;; "http://www.ai.mit.edu/projects/iiip/doc/CommonLISP/HyperSpec/"
;;;
;;; :NOTE This _should_ be set elsewhere already but just in case.
;;; :MODIFICATIONS <Timestamp: #{2010-04-02T20:03:26-04:00Z}#{10136} - by MON KEY>r
(defun mon-set-common-lisp-hspec-init (&optional warn-only)
  "Set the Common Lisp hspec on MON systems at init time.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:NOTE Evaluate before other slime initializations to ensure the path values
aren't clobbered for the variables:\n
 `common-lisp-hyperspec-root', `common-lisp-hyperspec-issuex-table',
 `common-lisp-hyperspec-symbol-table'\n
:NOTE Initizliztion of the above variables affect:
the function `mon-help-CL-symbols' and variable `*mon-help-CL-symbols*' in:
:FILE mon-doc-help-CL.el\n
:SEE-ALSO `mon-purge-cl-symbol-buffers-on-load'.\n►►►"
  (mon-default-start-error/sane
   'mon-set-common-lisp-hspec-init warn-only
   (when (or (and (intern-soft "IS-MON-P-GNU" obarray) ;; *IS-MON-OBARRAY*
                  (bound-and-true-p IS-MON-P-GNU))
             (and (intern-soft "IS-MON-P-W32" obarray) ;; *IS-MON-OBARRAY*
                  (bound-and-true-p IS-MON-P-32)
                  (not (and (intern-soft "IS-BUG-P" obarray) ;; *IS-MON-OBARRAY*
                            (bound-and-true-p IS-BUG-P)))))
     (unless (and (bound-and-true-p common-lisp-hyperspec-root)
                  (bound-and-true-p common-lisp-hyperspec-issuex-table)
                  (bound-and-true-p common-lisp-hyperspec-symbol-table))
       (setq common-lisp-hyperspec-root (nth 8 (mon-get-mon-emacsd-paths)))
       (setq common-lisp-hyperspec-issuex-table 
             (cond ((string-match-p "v3" common-lisp-hyperspec-root)
                    (concat common-lisp-hyperspec-root "Data/Issue-Cross-Refs.text"))
                   (t (concat common-lisp-hyperspec-root "Data/Map_IssX.txt"))))
       (setq common-lisp-hyperspec-symbol-table 
             (cond ((string-match-p "v3" common-lisp-hyperspec-root)
                    (concat common-lisp-hyperspec-root "Data/Symbol-Table.text"))
                   (t (concat common-lisp-hyperspec-root "Data/Map_Sym.txt"))))))))
;;
;; (mon-set-common-lisp-hspec-init t)

;;; ==============================
;;; :PREFIX "msli-"
;;; :CREATED <Timestamp: #{2010-04-02T20:29:38-04:00Z}#{10136} - by MON KEY>
(defun mon-set-lisp-init (&optional warn-only)
  "Load MON slime preferences and add `emacs-lisp-mode-hook's at init time.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO `*mon-lisp-safe-local-variable-values*'.\n►►►"
  (mon-default-start-error/sane
   'mon-set-slime-init warn-only
   (when (and (intern-soft "IS-MON-P-W32" obarray) ;; *IS-MON-OBARRAY*
              (bound-and-true-p IS-MON-P-W32))
     (load-file "slime-loads.el"))
   ;; (when IS-MON-P-GNU (require 'slime-loads-GNU-clbuild)
   ;;        (mon-slime-setup-init))
   (when (and (intern-soft "IS-MON-P" obarray)  ;; *IS-MON-OBARRAY*
              (bound-and-true-p IS-MON-P))
     (dolist (msli-D-1 *mon-lisp-safe-local-variable-values* 
                   (custom-set-variables 
                    `(safe-local-variable-values ,safe-local-variable-values)))
       (add-to-list 'safe-local-variable-values msli-D-1))
     ;;
     (add-hook 'emacs-lisp-mode-hook
               (function (lambda () 
                 (set (make-local-variable 'mouse-avoidance-mode) 'banish))))
     (add-hook 'emacs-lisp-mode-hook 
               (function (lambda () 
                 (set (make-local-variable 'indent-tabs-mode) nil))))
     (add-hook 'emacs-lisp-mode-hook 
               (function (lambda ()  ;; `minibuffer-completing-symbol'
                           (set (make-local-variable 'eval-expression-print-level) 8))))
     (add-hook 'emacs-lisp-mode-hook 
               (function (lambda () 
                           (set (make-local-variable 'eval-expression-print-length) nil))))
     ;; ==============================
     ;; :NOTE Following should be added/removed according to need:
     ;; -*-truncate-lines: t; -*-
     ;; (add-hook 'emacs-lisp-mode-hook #'(lambda () (setq truncate-lines t)))
     ;; (remove-hook 'emacs-lisp-mode-hook #'(lambda () (setq truncate-lines t)))
     ;; ==============================
     ;; (setq lisp-indent-function 'lisp-indent-function)
     ;; (setq-default lisp-indent-function 'common-lisp-indent-function)
     ;; ==============================
     ;; :SLIME-RELATED-HOOKS
     ;; (add-hook 'lisp-mode-hook (function (lambda () (slime-mode t))))
     ;; (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
     )))
;;
;; (mon-set-lisp-init t)

;;; ==============================
;;; :PROCED
;;; :NOTE Load `proced' package at startup and have it update automatically.
;;; :SEE-ALSO  `proced-auto-update-flag', `proced-auto-update-interval'
;;; :TODO Should be loaded in a dedicated frame and marked not to kill.
;;; :CREATED <Timestamp: #{2009-12-18T00:53:55-05:00Z}#{09515} - by MON>
(defun mon-set-proced-init (&optional warn-only)
  "Initialize `proced' related preferences on MON systems at init time.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane
   'mon-set-proced-init warn-only
   (require 'proced)    
   (when proced-available
     ;; :NOTE `proced-toggle-auto-update' related fncns can clobber the `match-data'!!
     ;; (proced-toggle-auto-update 1)))
     (proced))
   ;; Lets see the tty's when on a GNU/Linux box.
   (when (and (intern-soft "IS-MON-P-GNU" obarray) ;; *IS-MON-OBARRAY*
              (bound-and-true-p IS-MON-P-GNU))
     (setq proced-format 'medium))))
;;
;; (mon-set-proced-init t)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-02T20:49:15-04:00Z}#{10136} - by MON KEY>
(defun mon-set-auctex-init (&optional warn-only)
  "Initialize auctex on MON systems at init time.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane
   'mon-set-auctex-init warn-only
   (when (and (intern-soft "IS-MON-P-GNU" obarray) ;; *IS-MON-OBARRAY*
              (bound-and-true-p IS-MON-P-GNU))
     (load "auctex" t)
     (load "preview-latex" t))
   ))
;;
;; (mon-set-auctex-init t)

;;; ==============================
;;; :NOTE (dvc-current-active-dvc)
;;; :NOTE :VARIABLE `bzr-executable' in :FILE bzr-core.el 
;;; :MODIFICATIONS <Timestamp: #{2010-04-02T20:39:41-04:00Z}#{10136} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2010-01-01T11:58:49-05:00Z}#{10535} - by MON KEY>
;;; :CREATED <Timestamp: #{2010-03-20T17:24:42-04:00Z}#{10116} - by MON KEY>
(defun mon-set-dvc-init (&optional warn-only)
  "Initialize dvc sytem on MON systems at init time.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane
   'mon-set-dvc-init warn-only
   (add-to-list 'load-path 
                (mon-build-path-for-load-path *mon-site-lisp-root* "dvc/lisp"))
   (if (featurep 'dvc-core)
       (dvc-reload)
       (require 'dvc-autoloads))
   (when (or (and (intern-soft "IS-MON-P-W32" obarray)  ;; *IS-MON-OBARRAY*
                  (bound-and-true-p IS-MON-P-W32))
             (and (intern-soft "IS-BUG-P" obarray) ;; *IS-MON-OBARRAY*
                  (bound-and-true-p IS-BUG-P)))
     (setq dvc-sh-executable  
           (cond ((and (intern-soft "IS-BUG-P" obarray) ;; *IS-MON-OBARRAY*
                       (bound-and-true-p IS-BUG-P))
                  ;; (plist-get (cadr (assoc 'the-sh-pth *mon-misc-path-alist*)) :cygwin))
                  (cadr (assoc 'cygwin (cadr (assoc 'the-sh-pth *mon-misc-path-alist*)))))
                 ((and (intern-soft "IS-MON-P-W32" obarray)  ;; *IS-MON-OBARRAY*
                       (bound-and-true-p IS-MON-P-W32))
                  ;;(plist-get (cadr (assoc 'the-sh-pth *mon-misc-path-alist*)) :msys)))))
                  (cadr (assoc 'msys (cadr (assoc 'the-sh-pth *mon-misc-path-alist*))))))))
   (if (and (intern-soft "IS-W32-P" obarray) ;; *IS-MON-OBARRAY*
            (bound-and-true-p IS-W32-P)) ;; (eq system-type 'windows-nt)
       (if (executable-find "bzr")
           (setq bzr-executable (executable-find "bzr"))
         (setq bzr-executable "bzr")) ;; `vc-bzr-program'
     (if (executable-find "hg")
         (setq xhg-executable (executable-find "hg"))
       (when (file-executable-p (concat (getenv "SP_HG") "hg.exe"))
         (setq xhg-executable   (concat (getenv "SP_HG") "\\hg.exe")))))
   ;; Turn off the ever so pervasive dvc-tips buffer.
   (custom-set-variables
    '(dvc-select-priority '(bzr xhg xgit xdarcs xmtn tla baz) t)
    '(dvc-tips-enabled nil t)
    '(dvc-log-edit-other-frame t t)) ;; `bzr-add-log-entry', `dvc-add-log-entry'
   ))
;; (mon-set-dvc-init t)

;;; ==============================
;;; :CHANGESET 2120
;;; :CREATED <Timestamp: #{2010-09-13T13:14:38-04:00Z}#{10371} - by MON KEY>
(defun mon-set-erc-configs-init (&optional warn-only)
  "Set erc related preferences on MON systems at init time.\n
Set customizable variables:\n
`erc-nick' `erc-pals' `erc-prompt' `erc-system-name' `erc-add-query'\n
The values of these variables are on the alist key 'the-erc-configs 
in variable `*mon-misc-path-alist*'.\n
:SEE :FILE lisp/erc/*
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane
   'mon-set-erc-configs-init warn-only
   ;; (require 'erc)
   (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
              (bound-and-true-p IS-MON-SYSTEM-P)
              (intern-soft "*mon-misc-path-alist*" obarray) ;; *IS-MON-OBARRAY*
              (bound-and-true-p *mon-misc-path-alist*)
              t)
     (apply 'custom-set-variables 
            (cadr (assq 'the-erc-configs *mon-misc-path-alist*))
            )
     (custom-set-variables 
      ;; /ignore joins+parts. The key to IRC.
      '(erc-hide-list '("JOIN" "PART" "QUIT"))
      (custom-set-variables '(erc-autoaway-mode t))
      ;; erc-modules 
      ;;
      ;; erc-autoaway-mode
      ;; erc-auto-set-away
      ;; erc-away
      ;;
      ;; erc-log-channels-directory ;; :DEFAULT "~/log"
      ;; erc-log-insert-log-on-open
      ;; erc-log-filter-function
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; `erc-modes'
      ;; `erc-log-setup-logging'   BUFFER
      ;; `erc-save-buffer-in-logs' &optional BUFFER
      ;; `erc-log-disable-logging' BUFFER
      ;; :NOTE (erc-logging-enabled (current-buffer))
      ;;       (erc-current-logfile (current-buffer))
      ;; Disable logging in some buffer:
      ;; (set (make-local-variable 'erc-enable-logging) nil)
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ))
   ))

;;; ==============================
;;; :CHANGESET 1990
;;; :CREATED <Timestamp: #{2010-07-26T14:54:55-04:00Z}#{10301} - by MON KEY>
(defun mon-set-url-pkg-init (&optional warn-only)
  "Load url/url-*.el packages and set related preferences on MON systems at init time.\n
Evaluate `url-setup', set `url-debug' t, and require :FILE url/url-http.el
:SEE-ALSO `url-setup-done', `mon-set-browser-init', `mon-set-w3m-init'.\n►►►"
  (mon-default-start-error/sane
   'mon-set-url-pkg-init warn-only
   (require 'url)
   (require 'url-http)
   (url-do-setup)
   (let ((uconfdir (file-truename 
                    (expand-file-name ".emacs.d/url/" 
                                      (file-truename (getenv "HOME"))))))
     (unless  (file-directory-p uconfdir)
       (make-directory uconfdir))
     (set-variable 'url-configuration-directory uconfdir)
     (put 'url-configuration-directory 'customized-value 
          (list (custom-quote (eval url-configuration-directory)))))
   (custom-set-variables 
    '(url-debug t)
    ;;(url-configuration-directory  t)
    '(url-cookie-file (expand-file-name "cookies" url-configuration-directory) t)
    '(url-cookie-save-interval (* 3600 15) t) ;; default ran a timer every 60 seconds
    ;; url-cache-directory
    ;; url-cookie-confirmation
    ;; url-cookie-multiple-line
    ;; url-cookie-trusted-urls
    ;; url-cookie-untrusted-urls
    '(url-history-file (expand-file-name "history" url-configuration-directory) t)
    '(url-history-save-interval (* 3600 15) t)  ;; default ran a timer every 60 seconds
    '(url-history-track nil t)
    ;; url-proxy-services
    ;; url-gateway-method          { telnet|rlogin|socks|tls|ssl|native }
    ;; url-honor-refresh-requests
    ;; url-directory-index-file
    ;; url-uncompressor-alist
    ;; url-bad-port-list
    ;; url-max-password-attempts
    ;; url-max-redirections
    '(url-temporary-directory temporary-file-directory t)
    '(url-privacy-level 'paranoid t) ;'(email os lastloc agent cookie)
    '(url-automatic-caching nil t))
   (url-cookie-setup-save-timer)
   (url-history-setup-save-timer)
   (url-setup-privacy-info)
   ))
;;
;; (mon-set-url-pkg-init tt)


;;; ==============================
;;; :CHANGESET 2073
;;; :CREATED <Timestamp: #{2010-08-19T16:31:50-04:00Z}#{10334} - by MON KEY>
(defun mon-set-google-maps-init (&optional warn-only)
  "Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO `google-define'.\n►►►"
  (mon-default-start-error/sane
   'mon-set-goolge-maps-init warn-only
   (add-to-list 'load-path (expand-file-name "google-maps" *mon-site-lisp-root*))
   (require 'google-maps)
   ;; ==============================
   ;; :CREATED <Timestamp: #{2010-08-19T17:28:36-04:00Z}#{10334} - by MON>
   ;;
   (defcustom google-maps-static-default-maptype 'hybrid
     "Default maptype for displaying interactive maps with `google-maps'."
     :type '(choice (symbol :tag "hybrid"    :value hybrid)
                    (symbol :tag "roadmap"   :value roadmap)
                    (symbol :tag "satellite" :value satellite)
                    (symbol :tag "terrain"   :value terrain))
     :group 'google-maps-static)
   ;;
   (custom-set-variables 
    '(google-maps-static-default-zoom 15 t)
    '(google-maps-static-buffer-name "*GOOGLE-MAPS*" t))
   ))

;;; ==============================
;;; :CHANGESET 1786
;;; :CREATED <Timestamp: #{2010-05-29T12:03:37-04:00Z}#{10216} - by MON KEY>
(defun mon-set-w3m-init (&optional warn-only)
  "Set and load w3m and related preferences on MON systems at init time.\n
Add `mon-keybind-w3m' to the w3m-mode-hook to adjust `w3m-mode-map' keybindings.
Only relevant when `IS-MON-P-GNU'.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO `mon-set-browser-init', `mon-set-url-pkg-init'.\n►►►"
  (mon-default-start-error/sane
   'mon-set-w3m-init warn-only
   (when (and (intern-soft "IS-MON-P-GNU" obarray) ;; *IS-MON-OBARRAY*
              (bound-and-true-p IS-MON-P-GNU))
     (add-to-list 'load-path 
                  (mon-build-path-for-load-path *mon-site-lisp-root* "emacs-w3m"))
     (require 'w3m-load)
     (require 'w3m)
     (custom-set-variables
      '(w3m-use-cookies t t)
      '(browse-url-browser-function 'w3m-browse-url t)
      '(w3m-home-page "http://www.google.com" t)
      '(w3m-fill-column 80 t)
      '(w3m-add-user-agent nil t)
      ;; '(w3m-language (symbol-value 'current-language-environment)))
      ;; '(w3m-accept-languages )
      ;; '(w3m-init-file ) ;; :DEFAULT "~/.emacs-w3m"
      ;; '(w3m-default-save-directory ) ;; :DEFAULT ~/.w3m
      ;; '(w3m-profile-directory      ) ;; 
      ;; '(w3m-home-page )  
      ;; '(w3m-image-viewer ;; display | xfi | xv | xloadimage
      ))
   ))
;; (mon-set-w3m-init t)

;;; ==============================
;;; :TODO Entire conditional can be replaced with one call to `mon-get-mon-emacsd-paths'.
;;; :CHANGESET 1786 <Timestamp: #{2010-05-29T12:42:37-04:00Z}#{10216} - by MON KEY>
;;; :MODIFICATIONS <Timestamp: #{2010-04-02T21:29:00-04:00Z}#{10136} - by MON KEY>
;;; :CREATED <Timestamp: #{2010-01-29T16:00:47-05:00Z}#{10045} - by MON KEY>
(defun mon-set-browser-init (&optional warn-only)
  "Set generic web browser related preferences on MON systems at init time.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO `mon-set-w3m-init', `mon-set-url-pkg-init'.\n►►►"
  (mon-default-start-error/sane
   'mon-set-browser-init warn-only
   (apply 'custom-theme-set-variables 'user
          `((browse-url-firefox-program  (nth 10 (mon-get-mon-emacsd-paths)) t)
            (browse-url-firefox-new-window-is-tab t t)
            (browse-url-mozilla-new-window-is-tab 1 t)
            ,(cond ;; :NOTE These below were `custom-set-default'
              ((and (intern-soft "IS-BUG-P" obarray) ;; *IS-MON-OBARRAY*
                    (bound-and-true-p IS-BUG-P))
               '(browse-url-browser-function 'browse-url-generic)
               '(browse-url-generic-program (nth 10 (mon-get-mon-emacsd-paths))))
              ((and (intern-soft "IS-BUG-P-REMOTE" obarray) ;; *IS-MON-OBARRAY*
                    (bound-and-true-p IS-BUG-P-REMOTE))
               '(browse-url-browser-function 'browse-url-generic)
               '(browse-url-generic-program (nth 10 (mon-get-mon-emacsd-paths))))
              ((and (intern-soft "IS-MON-P-W32" obarray) ;; *IS-MON-OBARRAY*
                    (bound-and-true-p IS-MON-P-W32))
               '(browse-url-browser-function 'browse-url-generic)
               '(browse-url-generic-program (nth 9 (mon-get-mon-emacsd-paths))))
              ((and (intern-soft "IS-MON-P-GNU" obarray) ;; *IS-MON-OBARRAY*
                    (bound-and-true-p IS-MON-P-GNU))
               '(browse-url-browser-function 'browse-url-generic t)
               '(browse-url-generic-program (nth 9 (mon-get-mon-emacsd-paths)) t)))))
   ))
;;
;; (mon-set-browser-init t)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-26T19:08:09-04:00Z}#{09396} - by MON>
(defun mon-set-buffer-local-comment-start  ()
  "Make \";;\" a `buffer-local-value' for `comment-start'.\n
Bind locally in buffers without a buffer-local-value for comment-start and
`fundamental-mode'.\n
When `IS-MON-SYSTEM-P' evaluated on the `after-change-major-mode-hook'.\n 
:SEE-ALSO `*mon-default-comment-start*', `mon-comment-divider->col',
`mon-comment-divider-to-col', `mon-comment-divider-to-col-four',
`mon-comment-divider-w-len', `mon-comment-lisp-to-col', `comment-start',
`comment-padding', `comment-add', `comment-column', `comment-use-syntax'
`comment-style', `comment-styles'.\n►►►"
  (when (or (null (buffer-local-value 'comment-start (current-buffer)))
            (and (eq (buffer-local-value 'major-mode (current-buffer)) 'fundamental-mode)
                 (null (buffer-local-value 'comment-start (current-buffer)))))
    ;; (set (make-local-variable 'comment-styles)
    ;;      (add-to-list 'comment-styles '(mon-multi . (t nil nil multi-char))))
    ;; (set (make-local-variable 'comment-style) 'mon-multi)
    (set (make-local-variable 'comment-style)  'indent)
    (set (make-local-variable 'comment-start) ";;")))
;;
;; (mon-set-buffer-local-comment-start)

;;; ==============================
;;; :CHANGESET 1786
;;; :CREATED <Timestamp: #{2010-05-29T17:53:59-04:00Z}#{10216} - by MON KEY>
(defun mon-set-buffer-local-comment-start-init (&optional warn-only)
  "Set default value of `comment-start' on MON systems at init time.\n
Puts `mon-set-buffer-local-comment-start' on the `after-change-major-mode-hook'.\n
Put comment-start \"#\" on the `shell-mode-hook'.\n
Put comment-start \"/*\" on the `css-mode-hook'.\n
Put comment-start \";;;\" on the `find-file-hook' for extension \".dbc\".\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane
   'mon-set-buffer-local-comment-start-init warn-only  
   (add-hook 'after-change-major-mode-hook 
             'mon-set-buffer-local-comment-start t)
   ;; :CSS-MODE-COMMENT
   (add-hook 'css-mode-hook 
             (function (lambda () ;; :NOTE CSS-MODE should do this already but
                         ;; sometimes gets unbound by other hooks.
                         (set (make-local-variable 'comment-start) "/*")
                         (set (make-local-variable 'comment-end) "*/"))))
   ;; :SHELL-MODE-COMMENT
   ;; :NOTE Maybe should be less aggressive with this one.
   ;; :SEE `sh-make-vars-local' which can set the hook in a more context
   ;; sensitive manner.
   (add-hook 'shell-mode-hook
             (function (lambda () 
                         (set (make-local-variable 'comment-start) "#"))))
   ;; :DBC-EXT-COMMENT
   ;; Default comment-start  for file with ".dbc" to extension to ";;;".
   (add-hook 'find-file-hook 
             (function (lambda ()
                         (when (string-equal (file-name-extension (buffer-file-name)) "dbc")
                           (set (make-local-variable 'comment-start) ";;;")))))
   ))
;; 
;; (remove-hook 'after-change-major-mode-hook 'mon-set-buffer-local-comment-start)
;;
;; (mon-set-buffer-local-comment-start-init t)

;;; ==============================
;;; :CHANGESET 1711
;;; :CREATED <Timestamp: #{2010-05-01T17:30:09-04:00Z}#{10176} - by MON KEY>
(defun mon-set-css-path-init (&optional warn-only)
  "Set and load CSS related preferences/paths on MON systems at init time.\n
Add require statement for :FILE mon-css-check.el and :FILE mon-css-complete.el\n
Add autoload `css-color-mode' from :FILE \"mon-css-color.el\".\n
Put `css-color-turn-on-in-buffer' on the `css-mode-hook'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE :FILE site-lisp/mon-css-check.el | css-check.el
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane
   'mon-set-css-path-init warn-only
   ;; :NOTE These are Niels Giesen's css-check and css-complete.el with minor mods.
   (require 'mon-css-complete) 
   (require 'mon-css-check nil t)
   (when (featurep 'mon-css-check)
     (unless (bound-and-true-p *css-check-csstidy-path*)
       (let ((csstidy-path
              (or (executable-find "csstidy")
                  (when (and (intern-soft "IS-W32-P" obarray) ;; *IS-MON-OBARRAY*
                             (bound-and-true-p IS-W32-P))
                    (or (executable-find "csstidy.exe")
                        (car (file-expand-wildcards
                              (concat (getenv "SP_BIN") "/csstidy/*.exe"))))))))
         (when csstidy-path (setq *css-check-csstidy-path* csstidy-path)))))
   ;; ;; :CSS-COLOR
   (autoload 'css-color-mode "mon-css-color" "" t)
   (add-hook 'css-mode-hook 
             (function (lambda () (css-color-turn-on-in-buffer))))))
;;
;; (mon-set-css-path-init t)

;;; ==============================
;; :LONG-LINES-MODE->AUTO-MODE-ALIST
;;; :NOTE See `NOTES:' section "DON'T AUTO-MODE-ALIST" of the file's header for
;;; additional discussion.
;;: (add-to-list 'auto-mode-alist '("\\.dbc\\'" . longlines-mode))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-02T20:15:55-04:00Z}#{10136} - by MON KEY>
(defun mon-set-longlines-init (&optional warn-only)
  "Ensure that `longlines-mode' gets invoked least once at Emacs init.\n
This is _necessary_ because a lot of `naf-mode' procedures fail if 
`longlines-mode' isn't in the environment.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
:SEE-ALSO `mon-toggle-restore-llm'.\n►►►"
  (mon-default-start-error/sane
   'mon-set-longlines-init warn-only
   (save-excursion
     (let (llm-test)
       (setq llm-test
             (with-temp-buffer
               (and (not (bound-and-true-p longlines-mode))
                    (longlines-mode))))
       (and llm-test 
            (message (concat ":FUNCTION `mon-set-longlines-init' " 
                             "-- initialized `longlines-mode' at startup")))))))
;;
;; (mon-set-longlines-init t)

;;; ==============================
;;; :CHANGESET 1740
;;; :CREATED <Timestamp: #{2010-05-17T09:49:59-04:00Z}#{10201} - by MON KEY>
(defun mon-rst-mode-facification ()
  "Run on the `rst-mode-hook' to set rst-mode faces.\n
Hook added add loadtime with \`mon-default-start-error/sane'.\n
:SEE-ALSO `mon-set-rst-mode-faces-init'.\n►►►"
  (set-face-background 'rst-level-1-face  "SteelBlue")
  (set-face-foreground 'rst-level-2-face  "dark slate gray")
  (set-face-background 'rst-level-2-face  "slate blue")
  (set-face-foreground 'rst-level-2-face  "dark slate gray")
  (set-face-background 'rst-level-3-face  "medium slate blue")
  (set-face-foreground 'rst-level-3-face  "dark slate gray")
  (set-face-background 'rst-level-4-face  "light late blue")
  (set-face-foreground 'rst-level-4-face  "dark slate gray"))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-04-02T17:09:29-04:00Z}#{10135} - by MON KEY>
;;; :CREATED <Timestamp: #{2010-01-29T14:12:23-05:00Z}#{10045} - by MON KEY>
(defun mon-set-rst-mode-faces-init (&optional warn-only)
  "Set `rst-mode' faces to MON preferred style.\n
When IS-MON-SYSTEM-P evaluated on the `rst-mode-hook'.
Signal an error when `IS-NOT-MON-SYSTEM'.\n
:EXAMPLE\n\n\(pp-display-expression 
 \(symbol-function 'mon-rst-mode-facification\) \"*MON-RST-MOD-FACIFICATION-FACES*\"\)\n
:SEE-ALSO `mon-rst-mode-facification'.\n►►►"
  (mon-default-start-error/sane
   'mon-set-rst-mode-faces-init warn-only ;; nil
   (eval-when (compile load eval) (require 'rst nil t))
   (add-hook 'rst-mode-hook 'mon-rst-mode-facification)
   ))
;;
;; (remove-hook 'rst-mode-hook 'mon-set-rst-mode-faces)
;; (mon-set-rst-mode-faces-init)

;;; ==============================
;;; :CHANGESET 1786
;;; :CREATED <Timestamp: #{2010-05-29T21:52:05-04:00Z}#{10216} - by MON KEY>
(defun mon-set-apache-mode-init (&optional warn-only)
  "Set `apache-mode' preferences on MON systems at init time.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane
   'mon-set-apache-mode-init warn-only
   (when (and (intern-soft "IS-MON-P" obarray) ;; *IS-MON-OBARRAY*
              (bound-and-true-p IS-MON-P))
     (require 'apache-mode)
     (autoload 'apache-mode "apache-mode" nil t)
     (add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
     (add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
     (add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
     (add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
     (add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode)))
   ))
;; (mon-set-apache-mode-init t)

;;; ==============================
;;; :NOTE Thiery's traverselisp is still a moving target.  Should
;;;        periodically check that we've pulled a current version.
;;; :CHANGESET 1786
;;; :CREATED <Timestamp: #{2010-05-29T21:55:16-04:00Z}#{10216} - by MON KEY>
(defun mon-set-traverselisp-init (&optional warn-only)
  "Set `traverselisp' preferences on MON systems at init time.\n
Add  *mon-site-lisp-root*/traverselisp to load-path.
Add file extension \".naf~\" to `traverse-ignore-files'.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
function is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane
   'mon-set-traverselisp-init warn-only
   (when (and (intern-soft "IS-MON-P" obarray) ;; *IS-MON-OBARRAY*
              (bound-and-true-p IS-MON-P))
     (add-to-list 'load-path 
                  (mon-build-path-for-load-path 
                   *mon-site-lisp-root* "traverselisp"))
     (require 'traverselisp)
   (add-to-list 'traverse-ignore-files ".naf~"))
   ))
;; (mon-set-traverselisp-init t)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-06T16:04:09-04:00Z}#{09412} - by MON KEY>
;; (when (or (featurep 'mon-default-start-loads) 
;;           (and (intern-soft "IS-MON-SYSTEM-P" obarray)
;;                (bound-and-true-p IS-MON-SYSTEM-P)))
(defun mon-load-cedet ()
  "Load CEDET if it isn't already.\n
This function will be :DEPRECATED once EMACS <-> CEDET merge is complete.\n►►►"
  (interactive)
  (if (and (intern-soft "IS-MON-P" obarray) ;; *IS-MON-OBARRAY*
           (bound-and-true-p IS-MON-P)
           (not (featurep 'cedet)))
      ;; (load-file  (concat *mon-site-lisp-root* "/cedet-cvs/common/cedet.el"))
      (progn (require 'cedet)
             (message (concat ":FUNCTION `mon-load-cedet' "
                              "-- feature cedet loaded")))
    (message (concat ":FUNCTION `mon-load-cedet' "
                     "-- CEDET already loaded or your of a MONish way"))))
;; ) ;; :CLOSE when

;;; ==============================
;;; :NOTE :BEFORE :FILE mon-keybindings.el
;;; :CREATED <Timestamp: #{2010-04-03T17:40:37-04:00Z}#{10136} - by MON KEY>
(defun mon-keybind-w32-init ()
  "Initialize w32 related keys on MON systems.\n
Binds the following variables:\n
 `w32-pass-rwindow-to-system', `w32-pass-lwindow-to-system'
 `w32-rwindow-modifier',  `w32-pass-multimedia-buttons-to-system'
:NOTE Loaded via `mon-keybind-put-hooks-init' at init time by
`mon-set-system-specific-and-load-init'.\n
:SEE :FILE src/w32fns.c
:SEE-ALSO `mon-set-custom-file-init-w32-configs', `w32-alt-is-meta',
`w32-pass-alt-to-system', `w32-quit-key', `w32-phantom-key-code',
`w32-enable-num-loc', `w32-enable-caps-lock', `w32-scroll-lock-modifier',
`w32-apps-modifier', `w32-mouse-button-tolerance', `w32-mouse-move-interval',
`w32-pass-extra-mouse-buttons-to-system',
`w32-pass-multimedia-buttons-to-system', `w32-register-hot-key',
`w32-unregister-hot-key', `w32-registered-hot-keys', `w32-reconstruct-hot-key',
`w32-toggle-lock-key', `mon-keybind-w3m', `mon-keybind-dired-mode',
`mon-keybind-w32-init', `mon-keybind-lisp-interaction-mode',
`mon-keybind-emacs-lisp-mode', `mon-help-key-functions', `mon-help-keys'.\n►►►"
  (mon-default-start-error/sane
   'mon-keybind-w32-init nil ;; just-warn
   (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
              (bound-and-true-p IS-MON-SYSTEM-P)
              (and (intern-soft "IS-W32-P" obarray) ;; *IS-MON-OBARRAY*
                   (bound-and-true-p IS-W32-P)))
     (custom-set-variables
      '(w32-pass-lwindow-to-system nil)
      '(w32-pass-rwindow-to-system nil)
      '(w32-pass-multimedia-buttons-to-system nil)
      '(w32-lwindow-modifier 'super)
      '(w32-rwindow-modifier 'hyper)))
   ))
;;
;; (mon-keybind-w32-init)

;;; ==============================
;;; :CHANGESET 1896 
;;; :CREATED <Timestamp: #{2010-06-17T14:41:40-04:00Z}#{10244} - by MON KEY>
(defun mon-keybind-put-hooks-init (&optional warn-only)
  "Add keybindings to various mode-hooks at init.\n
Evaluated at init time by `mon-set-system-specific-and-load-init'.\n
:SEE-ALSO `mon-keybind-w3m', `mon-keybind-dired-mode', `mon-keybind-w32-init',
`mon-keybind-lisp-interaction-mode', `mon-keybind-emacs-lisp-mode',
`mon-help-key-functions', `mon-help-keys'.\n►►►"
  (mon-default-start-error/sane
   'mon-set-system-specific-and-load-init warn-only
   (mon-keybind-w32-init)
   ;; (remove-hook 'dired-mode-hook 'mon-keybind-dired-mode)
   (add-hook 'dired-mode-hook 'mon-keybind-dired-mode)
   ;; (remove-hook 'completion-list-mode-hook 'mon-keybind-completions)
   (add-hook 'completion-list-mode-hook 'mon-keybind-completions)
   ;; (remove-hook 'emacs-lisp-mode-hook 'mon-keybind-emacs-lisp-mode)
   (add-hook 'emacs-lisp-mode-hook  'mon-keybind-emacs-lisp-mode)
   ;; (remove-hook 'lisp-interaction-mode-hook 'mon-keybind-lisp-interaction-mode)
   (add-hook 'lisp-interaction-mode-hook 'mon-keybind-lisp-interaction-mode)
   ;; (remove-hook 'slime-mode-hook 'mon-keybind-slime)
   (add-hook 'slime-mode-hook 'mon-keybind-slime)
   (when (featurep 'w3m)
     ;; (remove-hook 'w3m-mode-hook 'mon-keybind-w3m)
     (add-hook 'w3m-mode-hook 'mon-keybind-w3m))
   (add-hook 'change-log-mode-hook 'mon-multi-isearch-rmv-hook)
   (when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
              (bound-and-true-p IS-MON-SYSTEM-P))
     (eval-after-load "mon-keybindings" '(mon-keybind-globally)))
   ))


;;; ==============================
;;; :CHANGESET 2289
;;; :CREATED <Timestamp: #{2010-11-08T20:17:53-05:00Z}#{10451} - by MON KEY>
(defun mon-set-mon-feature-customs-init (&optional warn-only)
  "Set mon-library specific customizations.\n
Initiliaze values for following variables:\n
 `*mon-timestamp-cond*' `*google-define-buffer-suffix*'  
 `*mon-default-comment-divider*' `*mon-default-comment-start*' 
 `*naf-comment-prefix*' `*ulan-sample-data*'
 `*mon-purge-htmlfontify-dir-on-quit*' `*mon-purge-on-its-all-text-on-quit*'\n
Evaluates `mon-set-boxcutter-init'\n 
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane 
   'mon-set-mon-feature-customs-init warn-only  
   (when (and (intern-soft "IS-MON-P" obarray) ;; *IS-MON-OBARRAY*
              (bound-and-true-p IS-MON-P)
              (intern-soft "*MON-NAME*" obarray) ;; *IS-MON-OBARRAY*
              (bound-and-true-p *MON-NAME*))
     (custom-set-variables
      '(*mon-timestamp-cond*
        `((,(file-name-nondirectory 
             (directory-file-name *mon-emacs-root*)) 
           ,(cadr (assoc 6 *MON-NAME*)))
          (,(file-name-nondirectory 
             (directory-file-name *mon-naf-mode-root*))
           ,(cadr (assoc 6 *MON-NAME*)))
          (,(file-name-nondirectory 
             (file-name-sans-extension 
              (locate-library "mon-time-utils")))
           ,(cadr (assoc 6 *MON-NAME*)))))))
   (custom-set-variables 
    `(*mon-default-comment-start*   ,(concat (make-string 3 59) " "))
    `(*mon-default-comment-divider* ,(concat ";;; " (make-string 30 61)))
    '(*mon-bind-dir-locals-alist* t)
    ;; :FILE google-define-redux.el
    '(*google-define-buffer-suffix*  '("*" . ":gg-definition*"))
    `(*naf-comment-prefix* ,(make-string 3 59)))
   (when (and (intern-soft "*mon-naf-mode-notes*" obarray) ;; *IS-MON-OBARRAY*
              (bound-and-true-p *mon-naf-mode-notes*)
              (intern-soft "IS-MON-P" obarray) ;;*IS-MON-OBARRAY*
              (bound-and-true-p IS-MON-P)) 
     (custom-set-variables
      '(*ulan-sample-data*
        (file-truename (concat *mon-naf-mode-notes* "/ULAN/ulan_rel_utf8_sample09/")))))
   (custom-set-variables 
    '(*mon-purge-htmlfontify-dir-on-quit* t))
   (when (and (and (intern-soft "IS-W32-P" obarray) ;; *IS-MON-OBARRAY*
                   (bound-and-true-p IS-W32-P))
              (and (intern-soft "*mon-misc-path-alist*" obarray) ;; *IS-MON-OBARRAY*
                   (bound-and-true-p *mon-misc-path-alist*)))
     (custom-set-variables 
      '(*mon-purge-on-its-all-text-on-quit* t)))
   (mon-set-boxcutter-init)
   ))
  
;;; ==============================
;;; :CHANGESET 1786
;;; :CREATED <Timestamp: #{2010-05-29T11:33:29-04:00Z}#{10216} - by MON KEY>
(defun mon-set-system-specific-and-load-init (&optional warn-only)
  "Require and load additional system-type specific configs and procedures.\n
When `IS-MON-P-W32' require mon-w32-load.el\n
When `IS-MON-P-GNU' require mon-GNU-load.el\n
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane
   'mon-set-system-specific-and-load-init warn-only
   (mon-set-mon-feature-customs-init)
   (cond ((or (and (intern-soft "IS-MON-P-W32" obarray) ;; *IS-MON-OBARRAY*
                   (bound-and-true-p IS-MON-P-W32))
              (and (intern-soft "IS-BUG-P" obarray) ;; *IS-MON-OBARRAY*
                   (bound-and-true-p IS-BUG-P)))
          (require 'mon-w32-load))
         ((and (intern-soft "IS-MON-P" obarray) ;; *IS-MON-OBARRAY*
               (bound-and-true-p IS-MON-P))
          (require 'grep)
          (when (and (intern-soft "IS-MON-P-GNU" obarray) ;; *IS-MON-OBARRAY*
                     (bound-and-true-p IS-MON-P-GNU))
            (require 'mon-GNU-load)
            ;; (require 'mon-GNU-load-no-HG)          
            ;; (load "slime-loads-GNU-clbuild.el")
            (require 'slime-loads-GNU-clbuild)
            (mon-slime-setup-init)
            ;; :BEFORE mon-utils.el
            (mon-set-common-lisp-hspec-init))))
   ;;
   ;; :NOTE :FILE mon-utils.el contains require statements for mon-*.el packages.   
   (require 'mon-utils)
   ;; :NAF-MODE
   ;; (require 'naf-mode)
   ;; :NOTE Automode files with '.naf' file extensions. '.naf' -> NAME AUTHORITY
   ;;       FILE `.naf' is a prefered default extension for anything `naf-mode'
   ;;       related. <Timestamp: #{2008-12-10} - by MON KEY> 
   (add-to-list 'auto-mode-alist '("\\.naf\\'" . naf-mode))
   ;; Evaluate remaining fncns defined below now:
   (mon-set-longlines-init)
   (mon-set-buffer-local-comment-start-init)   
   (mon-set-unicodedata-init)   
   (mon-set-url-pkg-init)
   (mon-set-google-maps-init)
   (mon-set-w3m-init)
   (mon-set-browser-init)
   (mon-set-lisp-init)
   (mon-set-dvc-init)   
   (mon-set-css-path-init)
   ;; (mon-set-auctex-init)
   (mon-set-proced-init)
   (mon-set-rst-mode-faces-init)
   (mon-set-traverselisp-init)
   (mon-set-apache-mode-init)
   (mon-set-erc-configs-init)
   ;; ==============================
   ;;
   ;; :REQUIRE-PACKAGES
   ;;
   ;; ==============================
   ;; :DIRED
   (require 'dired-details)
   (dired-details-install)
   (require 'dired-efap)
   ;; ==============================
   ;; :DOREMI
   ;; :NOTE This is Drew Adams' Do Re Mi commands consolidated to one file.
   ;;       Doesn't include the frame-fns.
   (require 'mon-doremi nil t)
   ;; ==============================
   ;; :NOTE External/Third-Party packages we want available but don't want loaded
   ;;       from mon-*.el packages or which need to be in the environment before
   ;;       loading them.  Also, useful when we are testing a new feature and not
   ;;       sure if we want to use it for the long haul.
   (require 'uniq nil t)
   (require 'regexpl nil t)
   (require 'register-list nil t)
   (require 'boxquote nil t)
   (require 'align-let nil t)
   ;; ==============================
   ;; :JS2-MODE
   ;; (autoload 'js2-mode "js2" nil t)
   ;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
   ;; ==============================
   ;; :NOTE This is a patched version of Matsushita Akihisa's color-cccur.el
   ;; :SEE (URL `http://www.bookshelf.jp/elc/color-occur.el')
   (require 'mon-color-occur)
   ;;; ==============================
   (require 'etags)
   (custom-set-variables 
    `(tags-table-list ,(car *mon-tags-table-list*)))
   ;; Now put some keybindings on the mode-hooks:
   (mon-keybind-put-hooks-init)
   ))
;;
;; (mon-set-system-specific-and-load-init t)
(mon-set-system-specific-and-load-init)

;;; ==============================
;;; :NOTE Load keybindings last to ensure everything is loaded in first.
(require 'mon-keybindings)

;;; ==============================
(provide 'mon-default-start-loads)
;;; ==============================

(when (and (intern-soft "IS-MON-SYSTEM-P" obarray) ;; *IS-MON-OBARRAY*
           (bound-and-true-p IS-MON-SYSTEM-P))
  (eval-after-load "mon-default-start-loads" '(require 'mon-post-load-hooks)))

 
;; Local Variables:
;; mode: EMACS-LISP
;; coding: utf-8
;; no-byte-compile: t
;; End:

;;; ==============================
;;; mon-default-start-loads.el ends here
;;; EOF


;;; mon-default-start-loads.el --- fncns for initializing MON Emacs environment
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
;; `mon-default-start-error/sane'
;; `mon-build-path-for-load-path'
;; `mon-toggle-show-point-mode'
;; `mon-set-infopath-init'
;; `mon-set-bookmark-file-init'
;; `mon-set-custom-file-init' 
;; `mon-set-color-themes-init'
;; `mon-set-buffer-local-comment-start'
;; `mon-set-rst-mode-faces', `mon-rst-mode-facification'
;; `mon-set-ibuffer-init'
;; `mon-set-woman-manpath-init'
;; `mon-set-common-lisp-hspec-init'
;; `mon-set-thumbs-conversion-program-init'
;; `mon-set-load-path-init'
;; `mon-set-auctex-init'
;; `mon-set-longlines-init'
;; `mon-set-help-mode-init'
;; `mon-set-dvc-init'
;; `mon-set-doc-view-programs-init'
;; `mon-set-emacs-temp-file/dir-init'
;; `mon-set-ispell-init'
;; `mon-set-C-source-directory-init'
;; `mon-set-customizations-before-custom-file-init'
;; `mon-set-unicodedata-init'
;; `mon-w32-key-init'
;; FUNCTIONS:◄◄◄
;; 
;; MACROS:
;;
;; CONSTANTS:
;;
;; VARIABLES:
;; `*mon-default-start-loads-xrefs*'
;; `*mon-default-start-load-sanity*'
;; `*mon-default-start-load-sanity-WARN-ONLY*'
;;
;; ADVISED:
;; `find-function-search-for-symbol' , `find-variable-noselect' 
;;
;; ALIASED/ADIVISED/SUBST'D:
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
;; Copyright © 2009, 2010 MON KEY 
;;; ==============================

;;; CODE:

(eval-when-compile (require 'cl))

;;; First things first. Make sure we aren't tortured by blinking cursors etc.
(when IS-MON-P
  (unless show-paren-mode (show-paren-mode 1))
  (unless (null scroll-bar-mode) (scroll-bar-mode -1))
  (unless (null tool-bar-mode) (tool-bar-mode -1))
  (unless (null menu-bar-mode) (menu-bar-mode -1))
  (unless (null blink-cursor-mode) (blink-cursor-mode -1))
  (setq-default cursor-type '(bar . 3)))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-02T18:02:25-04:00Z}#{10135} - by MON KEY>
(defvar *mon-default-start-loads-xrefs* nil
  "*Xrefing list of functions and variables defined in mon-default-start-loads\n
:EXAMPLE\n\(symbol-value '*mon-default-start-loads-xrefs*\)
\(symbol-value \(nth 3 *mon-default-start-loads-xrefs*\)\)\n
:SEE-ALSO `*naf-mode-xref-of-xrefs*'.\n►►►")
;;
(unless (bound-and-true-p *mon-default-start-loads-xrefs*)
  (setq *mon-default-start-loads-xrefs*
        '(*mon-default-start-loads-xrefs*
          *mon-default-start-load-sanity*
          *mon-default-start-load-sanity-WARN-ONLY*
          mon-default-start-error/sane
          mon-set-ibuffer-init
          mon-set-load-path-init
          mon-set-color-themes-init
          mon-set-infopath-init
          mon-set-woman-manpath-init
          mon-set-bookmark-file-init
          mon-set-custom-file-init
          mon-set-thumbs-conversion-program-init
          mon-set-common-lisp-hspec-init
          mon-set-longlines-init
          mon-set-dvc-init
          mon-set-help-mode-init
          mon-set-auctex-init
          ;; Not Macrolified:
          mon-build-path-for-load-path
          mon-set-buffer-local-comment-start
          mon-set-rst-mode-faces
          mon-toggle-show-point-mode
          mon-set-doc-view-programs-init
          mon-set-emacs-temp-file/dir-init
          mon-set-ispell-init
          mon-set-C-source-directory-init
          mon-set-customizations-before-custom-file-init
          mon-set-unicodedata-init
          mon-w32-key-init
          )))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-02T18:29:52-04:00Z}#{10135} - by MON KEY>
(defvar *mon-default-start-load-sanity* nil 
  "A list of functions evalauted when initalizing MON init.\n List elements are
those functions which loaded without errors and therefor should not be
evaluated again in the current Emacs session.\n
:SEE-ALSO `*mon-default-start-load-sanity-WARN-ONLY*', `mon-default-start-error/sane'
`*mon-default-start-loads-xrefs*'.\n►►►")
;;
;;;(progn (makunbound '*mon-default-start-load-sanity*)
;;;       (unintern   '*mon-default-start-load-sanity*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-06T14:02:50-04:00Z}#{10142} - by MON KEY>
(defvar *mon-default-start-load-sanity-WARN-ONLY* nil
  "When non-nil override all error singaling of `mon-default-start-error/sane'.
This is useful when: 
 o debugging inits;
 o examining/valuating a function member of `*mon-default-start-load-sanity*';
 o When `IS-NOT-A-MON-SYSTEM' to avoid the `IS-MON-SYSTEM-P' runtime checks.\n
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
  "Signal an error if FNNCN was evaluated at init or when `IS-NOT-A-MON-SYSTEM'.
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
     (cond ((not (bound-and-true-p IS-MON-SYSTEM-P))
            (if (or *mon-default-start-load-sanity-WARN-ONLY* ,just-warn)
                (warn (format (concat ":FUNCTION %s -- variable "
                                      "`IS-MON-SYSTEM-P' unbound or non-existent") ,fncn))
                (error (format (concat ":FUNCTION %s -- variable "
                                       "`IS-MON-SYSTEM-P' unbound or non-existent") ,fncn))))
           ((memq ,fncn *mon-default-start-load-sanity*)
            (if (or *mon-default-start-load-sanity-WARN-ONLY* ,just-warn)
                (warn (format (concat ":FUNCTION %s -- eval'd at init is "
                                      "member of `*mon-default-start-load-sanity*'") ,fncn))
                (error (format (concat ":FUNCTION %s -- eval'd at init is "
                                       "member of `*mon-default-start-load-sanity*'") ,fncn)))))
     ,@do-sane
     (unless (or *mon-default-start-load-sanity-WARN-ONLY* ,just-warn)
       (push ,fncn *mon-default-start-load-sanity*))))
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
Adds values of following variables to `load-path':
 `*mon-emacs-root*', `*mon-site-lisp-root*',
 `*mon-user-emacsd*', `*mon-naf-mode-root*',
 `*mon-ebay-tmplt-mode-root*'\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
funiction is already a member of variable `*mon-default-start-load-sanity*' as per 
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
funiction is already a member of variable `*mon-default-start-load-sanity*' as per 
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
     (cond  (IS-BUG-P (color-theme-ld-dark)) ;; (color-theme-euphoria))       
            (IS-MON-P (color-theme-ld-dark))))))
;;
;; (mon-set-color-themes-init t)
(mon-set-color-themes-init)

;;; ==============================
;; :SHOW-POINT-MODE
;; :NOTE To ensure we've setup a show-point environment do it now.
;;; :NOTE :AFTER `mon-set-emacs-temp-file/dir-init'
(require 'show-point-mode)

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
 `temporary-file-directory', `thumbs-thumbsdir', `thumbs-temp-dir'
 `doc-view-cache-directory', `slime-temp-directory',\n
When these directories do not exist created them.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
funiction is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO `*mon-local-emacs-temp-dir*'.\n►►►"
  (mon-default-start-error/sane 
   'mon-set-emacs-temp-file/dir-init warn-only
   (require 'doc-view)
   (custom-set-variables
    '(temporary-file-directory 
      (concat *mon-local-emacs-temp-dir* "/misc-emacs-temp"))
    '(thumbs-thumbsdir 
      (expand-file-name (format "thumbs-%d" (user-uid))
       (concat *mon-local-emacs-temp-dir* "/emacs-thumbs")) t)
    '(thumbs-temp-dir
      (expand-file-name (format "thumbs-%d-temp" (user-uid)) thumbs-thumbsdir) t)
   '(doc-view-cache-directory
     (expand-file-name (format "docview%d" (user-uid)) 
      (concat *mon-local-emacs-temp-dir* "/docview-cache")) t))
   ;; Make sure the above directories exist.
   (dolist (ensure-dir '(thumbs-temp-dir thumbs-thumbsdir
					 temporary-file-directory doc-view-cache-directory))
     (when (null (car (file-attributes (symbol-value ensure-dir))))
       (mkdir (symbol-value ensure-dir) t)))))
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
funiction is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO `w32-init-info', `mon-get-env-vars-emacs',
`Info-default-directory-list', `Info-directory-list',
`Info-additional-directory-list', `Info-dir-contents'.\n►►►"
  (mon-default-start-error/sane 
   'mon-set-info-path-init warn-only
   (when IS-MON-P-W32
     (when (bound-and-true-p Info-dir-contents)
       (setq Info-dir-contents nil))
     (when (bound-and-true-p Info-dir-contents)
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
error if funiction is already a member of variable
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
                             (if IS-MON-P-GNU
                                 (format ".emacs-%s.bmk" (cdr (mon-gnu-system-conditionals)))
                               ".emacs.bmk")))))
     (bookmark-load new-bm t)
     (custom-set-variables 
      '(bookmark-default-file new-bm t)
      '(bookmark-save-flag 1)))
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
funiction is already a member of variable `*mon-default-start-load-sanity*' as per 
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
   (when IS-MON-P-W32
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
                                      '("usr/share/man" "usr/local/share/man" "usr/X11R6/share/man"
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
funiction is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO .\n►►►"
(mon-default-start-error/sane 
 'mon-set-C-source-directory-init warn-only
 (cond  (IS-MON-P-W32
         (let ((c-srcs 
                (file-expand-wildcards (concat (getenv "SP_BIN") "/*/emacs-cur-src/*/src") t)))
           (when (car c-srcs)
             (setq source-directory (car c-srcs))
             (setq find-function-C-source-directory (car c-srcs)))))
         (IS-MON-P-GNU 
          (or (car (file-expand-wildcards (concat *mon-HG-root-path* "*/*/*/src") t))
              ;; (car (file-expand-wildcards (concat (getenv "DEVHOME") "*/*/*/src") t))
              (car (file-expand-wildcards 
                    (concat (file-name-directory *mon-emacs-root*) "emacs-BZR/*/src") t))))
         ;; :TODO Something here once GNU systems are sorted.
         ) ;; (IS-MON-SYSTEM-P )) ;; Fill in as needed.
 ))
;;
;; (mon-set-C-source-directory-init t)
(mon-set-C-source-directory-init)

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-03T17:28:12-04:00Z}#{10136} - by MON KEY>
(defun mon-set-unicodedata-init ()
  "Set unicode related variables at loadtime.\n
Sets the following variables:
 `describe-char-unicodedata-file'
 `describe-char-unidata-list'\n
:SEE-ALSO `'.\n►►►"
  (mon-default-start-error/sane 
   'mon-set-unicodedata-init nil ;; just-warn
   (when IS-MON-P
     (setq describe-char-unicodedata-file  
           (expand-file-name "UnicodeData.txt" *mon-site-lisp-root*))
     (setq describe-char-unidata-list 
                '(name general-category decomposition
                       digit-value numeric-value old-name iso-10646-comment)))
   ))
;;
(mon-set-unicodedata-init)
;;; ==============================
;; :EXECUTABLES
;;; :NOTE Following are inits that rely on external executables in path.

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-02T23:08:06-04:00Z}#{10136} - by MON KEY>
(defun mon-set-doc-view-programs-init (&optional warn-only)
  "Initialize doc-view related programs on MON systems when `IS-MON-P-W32'.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
Set the following variables:
 `doc-view-dvipdf-program', `doc-view-dvipdfm-program',
`doc-view-pdftotext-program', `doc-view-ghostscript-program',
`doc-view-ps2pdf-program',
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
funiction is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane 
   'mon-set-doc-view-programs-init nil ;; just-warn
   (when IS-MON-P-W32
     (custom-set-variables
      '(doc-view-dvipdf-program nil)
      '(doc-view-dvipdfm-program 
        (or (file-truename (concat (getenv "SP_TEXLIV") "\\dvipdfm.exe"))
            (executable-find "dvipdfm")))
      '(doc-view-ghostscript-program 
        (file-truename (concat (getenv "SP_GS") "\\gs8.64\\bin\\gswin32c.exe")))
      '(doc-view-pdftotext-program
        (or (file-truename (concat (getenv "SP_TEXLIV")"\\pdftotext.exe"))
            (file-truename (concat (getenv "SP_XPDF") "\\pdftotext.exe"))
            (executable-find "pdftotext")))
      '(doc-view-ps2pdf-program
        (or (file-truename (concat (getenv "SP_TEXLIV") "\\ps2pdf.exe"))
            (executable-find "ps2pdf")))))
   ))
;;
;; (mon-set-doc-view-programs-init t)
(mon-set-doc-view-programs-init)

;;; ==============================
;;; :NOTE image-dired related settings:
;;;
;;; :IMAGE-DIRED
;;; (getenv "SP_GNUW32") (getenv "SP_EXIF")
;;; (setq image-dired-cmd-create-standard-thumbnail-command 
;;;       (concat "imconvert -size %wx%h \"%f\" -thumbnail \"%wx%h>\" png:\"%t\" ;"
;;; (convert-standard-filename (concat (getenv "SP_GNUW32") "/pngcrush.exe")) " -q -text b "
;;;               "\"Description\" \"Thumbnail of file://%f\" -text b "
;;;               "\"Software\" \"GNU Emacs\" -text b \"Thumb::MTime\" \"%m\" -text b "
;;;               "\"Thumb::URI\" \"file://%f\" %q %t ; rm %q"))
;;; (setq image-dired-cmd-create-temp-image-program "imconvert")
;;; (setq image-dired-cmd-create-thumbnail-program "imconvert")
;;; (setq image-dired-thumbnail-storage 'per-directory))
;;;
;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-04-02T20:00:29-04:00Z}#{10136} - by MON KEY>
;;; :CREATED <Timestamp: #{2009-10-21T20:01:43-04:00Z}#{09434} - by MON>
(defun mon-set-thumbs-conversion-program-init (&optional warn-only)
  "Set the `thumbs-conversion-program' on MON systems at init time.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
funiction is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:NOTE This is a W32 related Kludge related to the ImageMagicks convert.exe
:SEE :FILE mon-rename-image-utils.el for additional discussion.
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane 
   'mon-set-thumbs-conversion-program-init warn-only ;; nil ;; just-warn
   ;; :IMAGE-FILE lisp/image-file.el
   (custom-set-variables
    '(image-file-name-extensions
      '("png" "jpeg" "jpg" "gif" "tiff" "tif" "xbm" "xpm" "pbm" "pgm" "ppm"
        "pnm" "nef" "bmp" "png" "svg")))
   ;; :THUMBS lisp/thumbs.el
   (when IS-W32-P
     ;;(setq thumbs-conversion-program
     (custom-set-variables 
      '(thumbs-conversion-program
        (if (getenv "SP_IMGCK")
            (let ((xp-convert (car (directory-files (getenv "SP_IMGCK") t "imconvert"))))
              (if xp-convert xp-convert (executable-find "imconvert")))
          (executable-find "imconvert")))))
   ))
;;
;; (mon-set-thumbs-conversion-program-init t)
(mon-set-thumbs-conversion-program-init)

;;; ==============================
;;; :ISPELL

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-03T17:10:18-04:00Z}#{10136} - by MON KEY>
(defun mon-set-ispell-init (&optional warn-only)
  "Set the ispell related prefs on MON local systems.\n
Set the following variables:
 `ispell-program-name'\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
funiction is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO \n►►►"
(mon-default-start-error/sane 
 'mon-set-ispell-init warn-only ;; nil ;; just-warn
 (custom-set-variables
  '(ispell-program-name 
    (cond (IS-MON-P-GNU (executable-find "aspell"))
          (IS-W32-P (or (convert-standard-filename (executable-find "aspell"))
                        (convert-standard-filename 
                         (concat (getenv "SP_ASPLL") "\\aspell.exe")))))))
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
funiction is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane
   'mon-set-help-mode-init warn-only ;; nil ;; just-warn
   ;; :NOTE I don't like the new 23.0 style UPCASE args in help.  Where clarity
   ;;        is the concern would be nicer to map them to a face.
   ;; (setq help-downcase-arguments t)
   (custom-set-variables 
    '(help-downcase-arguments t))
   ;;
   ;; :NOTE Ensure evaluating examples in help mode returns a nice long list.
   (add-hook 'help-mode-hook 
             #'(lambda () (set (make-local-variable 'print-length) nil)))
   ;;   
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
;; :IBUFFER

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
funiction is already a member of variable `*mon-default-start-load-sanity*' as per 
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
;; :IDO

;;; ==============================
;;; :NOTE Sometimes ido should ignore the *Completions* buffer.
;;; (add-to-list 'ido-ignore-buffers "\\*Completions\\*")
;;; :CHANGESET 1747 <Timestamp: #{2010-05-19T17:55:18-04:00Z}#{10203} - by MON KEY>
;;; :CREATED <Timestamp: #{2010-04-03T17:33:11-04:00Z}#{10136} - by MON KEY>
(defun mon-set-ido-init (&optional warn-only)
  "Sets the following variables:
 `describe-char-unicodedata-file'
 `describe-char-unidata-list'
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
funiction is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO `'.\n►►►"
  (mon-default-start-error/sane
   'mon-set-ido-init warn-only
   ;; :NOTE .ido.last used to be "ido_last"
   (when IS-MON-SYSTEM-P 
     (let ((mk-ido-d (concat (getenv "HOME") "/.emacs.d/ido")))
       (unless (file-directory-p mk-ido-d) (mkdir mk-ido-d))
       (unless (file-exists-p (concat mk-ido-d "/.ido.last"))
         (with-temp-file (concat mk-ido-d "/.ido.last") (newline)))
       (setq mk-ido-d (convert-standard-filename (concat mk-ido-d "/.ido.last")))
       (custom-set-variables 
        '(ido-save-directory-list-file mk-ido-d t)
        '(ido-everywhere nil t)
        '(ido-enable-prefix 1 t)
        '(ido-enable-flex-matching 1 t)
        '(ido-mode 'both t))
       (when IS-MON-P-W32
         (custom-set-variables 
          '(ido-work-directory-list-ignore-regexps '("c:/WINDOWS/.NtUninstall.*") t)))))
     ))
;;
;; (mon-set-ido-init t)
(mon-set-ido-init)

;;; ==============================
;; :CUSTOM-FILE

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-03T15:56:38-04:00Z}#{10136} - by MON KEY>
(defun mon-set-customizations-before-custom-file-init (&optional warn-only)
  "Set custom customizations before loading the custom file on MON systems.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
funiction is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane
   'mon-set-customizations-before-custom-file-init warn-only
   (fset 'yes-or-no-p 'y-or-n-p)
   (custom-set-variables 
    ;; :PATHS-FILES
    ;; '(delete-by-moving-to-trash t)
    '(desktop-dirname (file-name-as-directory *mon-user-emacsd*))
    '(auto-save-default 1)
    '(dired-recursive-copies 'top)
    '(dired-recursive-deletes t)
    '(confirm-kill-emacs 'y-or-n-p)
    '(enable-recursive-minibuffers 1)
    '(require-final-newline t)
    '(shell-input-autoexpand t)
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
    '(initial-buffer-choice t) ;; *scratch*
    '(inhibit-startup-message 1) 
    '(ring-bell-function 'ignore)
    '(cursor-type '(bar . 3))
    '(cursor-in-non-selected-windows nil)
    '(visual-line-fringe-indicators '(nil right-curly-arrow))
    )
   (when IS-MON-P
     (custom-set-variables 
      '(ffap-rfc-directories
        (concat (nth 5 (mon-get-mon-emacsd-paths)) "/RFCs-HG") t)
      '(initial-major-mode 'emacs-lisp-mode t)
      '(mouse-avoidance-mode 'exile t)
      '(backward-delete-char-untabify-method 'hungry t))) ;; 'all
   ))
;;
;; (mon-set-customizations-before-custom-file-init t)
(mon-set-customizations-before-custom-file-init)

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
funiction is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane
   'mon-set-custom-file-init warn-only
   (let* ((*mon-user-emacsd* (file-name-as-directory *mon-user-emacsd*))
          (mscf (cond (IS-MON-P-W32 
                       (concat *mon-user-emacsd* (nth 2 (assoc 1 *mon-emacsd*))))
                      (IS-BUG-P     
                       (concat *mon-user-emacsd* (nth 2 (assoc 3 *mon-emacsd*))))
                      (IS-MON-P-GNU 
                       (concat *mon-user-emacsd* (nth 2 (assoc 2 *mon-emacsd*)))))))
     (setq custom-file mscf))))
;;
;; (mon-set-custom-file-init t)
(mon-set-custom-file-init)

;;; :NOTE Eval'ing: (load custom-file t) the `t' flips the `no-error'.
(load custom-file)

;;; ==============================
;;; :REMAINING-PACKAGES

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
;;; 
;;; :NOTE This _should_ be set in site-local-private.el but just in case.
;;; :MODIFICATIONS <Timestamp: #{2010-04-02T20:03:26-04:00Z}#{10136} - by MON KEY>r
(defun mon-set-common-lisp-hspec-init (&optional warn-only)
  "Set the Common Lisp hspec on MON systems at init time.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
funiction is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:NOTE Evaluate before other slime initializations to ensure the path values
aren't clobbered for the variables:\n
 `common-lisp-hyperspec-root'
 `common-lisp-hyperspec-issuex-table'
 `common-lisp-hyperspec-symbol-table'\n
:SEE-ALSO `mon-purge-cl-symbol-buffers-on-load'.\n►►►"
  (mon-default-start-error/sane
   'mon-set-common-lisp-hspec-init warn-only
   (when (or IS-MON-P-GNU IS-MON-P-W32)
     (unless (and (bound-and-true-p common-lisp-hyperspec-root)
                  (bound-and-true-p common-lisp-hyperspec-issuex-table)
                  (bound-and-true-p common-lisp-hyperspec-symbol-table))
       (setq common-lisp-hyperspec-root (nth 8 (mon-get-mon-emacsd-paths)))
       (setq common-lisp-hyperspec-issuex-table 
             (concat common-lisp-hyperspec-root "Data/Map_IssX.txt")) ;; "Issue-Cross-Refs.text"))
     (setq common-lisp-hyperspec-symbol-table 
           (concat common-lisp-hyperspec-root "Data/Map_Sym.txt")))) ;; "Symbol-Table.text"))))
   ))
;;
;; (mon-set-common-lisp-hspec-init t)
(mon-set-common-lisp-hspec-init)

;;; ==============================
(cond ((or IS-MON-P-W32 IS-BUG-P) (require 'mon-w32-load))
      (IS-MON-P-GNU (require 'mon-GNU-load)))

;;; ==============================
;; :MON-UTILS
;;; :NOTE :FILE mon-utils.el contains require statements for mon-*.el packages.
(require 'mon-utils)

;;; ==============================
;; :NAF-MODE
(require 'naf-mode)

;;; ==============================
;;; :NOTE Automode files with '.naf' file extensions. '.naf' -> NAME AUTHORITY
;;;       FILE `.naf' is a prefered default extension for anything `naf-mode'
;;;       related.
;;; :CREATED <Timestamp: #{2008-12-10} - by MON KEY> 
(add-to-list 'auto-mode-alist '("\\.naf\\'" . naf-mode))

;;; ==============================
;; :SLIME
;; :EMACS-LISP-MODE-HOOKS

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-02T20:29:38-04:00Z}#{10136} - by MON KEY>
(defun mon-set-lisp-init (&optional warn-only)
  "Load MON slime preferences and add `emacs-lisp-mode-hook's at init time.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
funiction is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane
   'mon-set-slime-init warn-only
   (when IS-MON-P-W32 (load-file "slime-loads.el"))
   ;; (when IS-MON-P-GNU (load-file "slime-loads-GNU.el"))
   (when IS-MON-P
     (add-hook 'emacs-lisp-mode-hook
               (function (lambda () 
                 (set (make-local-variable 'mouse-avoidance-mode) 'banish))))
     (add-hook 'emacs-lisp-mode-hook 
               (function (lambda () 
                 (set (make-local-variable 'indent-tabs-mode) nil))))
     (add-hook 'emacs-lisp-mode-hook 
               (function (lambda () 
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
     ;;
     ;; (setq-default lisp-indent-function 'common-lisp-indent-function)
     )))
;;
;; (mon-set-lisp-init t)
(mon-set-lisp-init)

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
funiction is already a member of variable `*mon-default-start-load-sanity*' as per 
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
   (when IS-MON-P-GNU (setq proced-format 'medium))))
;;
;; (mon-set-proced-init t)
(mon-set-proced-init)

;;; ==============================
;; :DIRED-DETAILS
(require 'dired-details)
(dired-details-install)

;;; ==============================
;; :DOREMI
;;; :NOTE This is Drew Adams' Do Re Mi commands consolidated to one file.
;;;       Doesn't include the frame-fns.
;;; :CREATED <Timestamp: Tuesday February 24, 2009 @ 02:35.49 PM - by MON>
(require 'mon-doremi nil t)

;;; ==============================
;; :AUCTEX

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-02T20:49:15-04:00Z}#{10136} - by MON KEY>
(defun mon-set-auctex-init (&optional warn-only)
  "Initialize auctex on MON systems at init time.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
funiction is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane
   'mon-set-auctex-init warn-only
   (cond (IS-MON-P-GNU
          (load "auctex.el" nil t t)
          (load "preview-latex.el" nil t t)
         (IS-MON-SYSTEM-P ))) ;; (t ) Fill in as needed.
   ))
;;
;; (mon-set-auctex-init t)
(mon-set-auctex-init)

;;; ==============================
;; :REQUIRE-PACKAGES
;;; :NOTE External/Third-Party packages we want available but don't want loaded
;;;       from mon-*.el packages or which need to be in the environment before
;;;       loading them.  Also, useful when we are testing a new feature and not
;;;       sure if we want to use it for the long haul.
(require 'uniq)
(require 'regexpl)
(require 'register-list)
(require 'boxquote)
(require 'dired-efap)
(require 'align-let)

;;; :WAS (require 'color-occur)
;;; :NOTE mon-color-occur.el is a patched version of Matsushita Akihisa color-cccur.el
;;; :SEE (URL `http://www.bookshelf.jp/elc/color-occur.el')
(require 'mon-color-occur)

;;; :NOTE These are Niels Giesen's css-check and css-complete.el with minor mods.
(require 'mon-css-check)
(require 'mon-css-complete) 

;;; ==============================
;; :CEDET
;;; :NOTE Lets see what happens with Emacs CVS CEDET merge. In the meantime load
;;;       manually.  CEDET is distributed with Lennart's:
;;;       GNU Emacs 23.1.50.1 (i386-mingw-nt5.1.2600)
;;;       of 2009-10-13 on LENNART-69DE564 (patched)
;;;
;;; :MODIFICATIONS <Timestamp: #{2009-10-14T14:03:04-04:00Z}#{09423} - by MON>
;;; :CREATED <Timestamp: Saturday June 20, 2009 @ 02:39.26 PM - by MON>
;;; (if (and IS-MON-P (not (featurep 'cedet)))
;;;     (load-file  (concat *mon-site-lisp-root* "/cedet-cvs/common/cedet.el"))
;;;     (message "CEDET already loaded or your of a MONish way."))
;;; :USE `mon-load-cedet' instead. :SEE ./naf-mode/mon-utils.el
;;; ==============================

;;; ==============================
;; :DVC

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
funiction is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane
   'mon-set-dvc-init warn-only
   (add-to-list 'load-path 
                (mon-build-path-for-load-path *mon-site-lisp-root* "dvc/lisp"))
   (if (featurep 'dvc-core)
       (dvc-reload)
       (require 'dvc-autoloads))
   (when (or IS-MON-P-W32 IS-BUG-P)
     (setq dvc-sh-executable  
           (cond (IS-BUG-P ;; (plist-get (cadr (assoc 'the-sh-pth *mon-misc-path-alist*)) :cygwin))
                  (cadr (assoc 'cygwin (cadr (assoc 'the-sh-pth *mon-misc-path-alist*)))))
                 (IS-MON-P-W32 ;;(plist-get (cadr (assoc 'the-sh-pth *mon-misc-path-alist*)) :msys)))))
                  (cadr (assoc 'msys (cadr (assoc 'the-sh-pth *mon-misc-path-alist*))))))))
   (if IS-W32-P ;; (eq system-type 'windows-nt)
       (if (executable-find "bzr")
           (setq bzr-executable (executable-find "bzr"))
           (setq bzr-executable "bzr"))
       (if (executable-find "hg")
           (setq xhg-executable (executable-find "hg"))
           (when (file-executable-p (concat (getenv "SP_HG") "hg.exe"))
             (setq xhg-executable   (concat (getenv "SP_HG") "\\hg.exe")))))
   ;; Turn off the ever so pervasive dvc-tips buffer.
   (setq dvc-tips-enabled nil)))
;;
;; (mon-set-dvc-init)
(mon-set-dvc-init)

;;; ==============================              
;; :JST-MODE
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;; ==============================
;;; :LOAD-SPECIFIC-PACKAGES
;; :NOTE These are only loaded when `IS-MON-P'

(when IS-MON-P
;;
;;; ==============================
;; :TRAVERSELISP
;;; :NOTE Thiery's traverselisp is still a moving target.
;;;        Should periodically check that we've pulled a current version.
(add-to-list 'load-path (mon-build-path-for-load-path *mon-site-lisp-root* "traverselisp"))
(require 'traverselisp)
(add-to-list 'traverse-ignore-files ".naf~")

;;; ==============================       
;; :APACHE-MODE

(require 'apache-mode)
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))
;;
) ;; :CLOSE (when IS-MON-P.


;;; ==============================
;; :WEB-BROWSER-RELATED

;;; ==============================
;;; :TODO Entire conditional can be replaced with one call to `mon-get-mon-emacsd-paths'.
;;; :MODIFICATIONS <Timestamp: #{2010-04-02T21:29:00-04:00Z}#{10136} - by MON KEY>
;;; :CREATED <Timestamp: #{2010-01-29T16:00:47-05:00Z}#{10045} - by MON KEY>
(defun mon-set-browser-init (&optional warn-only)
  "Set web browser related preferences on MON systems at init time.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
funiction is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane
   'mon-set-browser-init warn-only
   (custom-set-default 'browse-url-firefox-program (nth 10 (mon-get-mon-emacsd-paths)))  
   (setq browse-url-firefox-new-window-is-tab t)
   (setq browse-url-mozilla-new-window-is-tab 1)
   (setq url-privacy-level 'paranoid)
   (cond (IS-BUG-P
          (custom-set-default 'browse-url-browser-function 'browse-url-generic)
          (custom-set-default 'browse-url-generic-program (nth 10 (mon-get-mon-emacsd-paths))))
         (IS-BUG-P-REMOTE
          (custom-set-default 'browse-url-browser-function 'browse-url-generic)
          (custom-set-default 'browse-url-generic-program (nth 10 (mon-get-mon-emacsd-paths))))
         (IS-MON-P-W32 
          (custom-set-default 'browse-url-browser-function 'browse-url-generic)
          (custom-set-default 'browse-url-generic-program (nth 9 (mon-get-mon-emacsd-paths))))
         (IS-MON-P-GNU
          (custom-set-default 'browse-url-browser-function 'browse-url-generic)
          (custom-set-default 'browse-url-generic-program (nth 9 (mon-get-mon-emacsd-paths)))))))
;;
;; (mon-set-browser-init t)
(mon-set-browser-init)

;;; ==============================
;; :COMMENT-PREFIXING

;;; ==============================
;;; `mon-set-buffer-local-comment-start'
;;; :CREATED <Timestamp: #{2009-09-26T19:08:09-04:00Z}#{09396} - by MON>
(defun mon-set-buffer-local-comment-start ()
  "Make \";;;\" a `buffer-local-value' for `comment-start' in `fundamental-mode'.\n
When `IS-MON-SYSTEM-P' evaluated on the `first-change-hook'.\n
:SEE-ALSO `*mon-default-comment-start*', `mon-comment-divider->col',
`mon-comment-divider-to-col', `mon-comment-divider-to-col-four',
`mon-comment-divider-w-len', `mon-comment-lisp-to-col'.\n►►►"
  (when IS-MON-SYSTEM-P
     (let ((is-fundamental 
            (eq (cdr (assoc 'major-mode (buffer-local-variables))) 'fundamental-mode)))
       (when is-fundamental
         (unless (buffer-local-value 'comment-start (current-buffer))
           (set (make-local-variable 'comment-start) ";;;"))))
     (add-hook 'first-change-hook 'mon-set-buffer-local-comment-start nil t)))
;;
;; (remove-hook 'first-change-hook 'mon-set-buffer-local-comment-start)
;;
;; (mon-set-buffer-local-comment-start)
;;

;; :CSS-MODE-COMMENT
(add-hook 'css-mode-hook #'(lambda () (set (make-local-variable 'comment-start) "/*")))

;; :SHELL-MODE-COMMENT
;;; :NOTE Maybe should be less aggressive with this one.
;;; :SEE `sh-make-vars-local' which can set the hook in a more context sensitive manner.
(add-hook 'shell-mode-hook #'(lambda () (set (make-local-variable 'comment-start) "#")))
;;
;;; :NOTE To have comment prefix for '.dbc' files default to ";;;".
(add-hook 'find-file-hook 
	  (function (lambda ()
            (when (string-equal (file-name-extension (buffer-file-name)) "dbc")
              (set (make-local-variable 'comment-start) ";;;")))))

;;; ==============================
;;; :CHANGESET 1711
;;; :CREATED <Timestamp: #{2010-05-01T17:30:09-04:00Z}#{10176} - by MON KEY>
(defun mon-set-csstidy-path-init (&optional warn-only)
  "Set path to csstidy exectuable on MON systems at init time.\n
When optional arg WARN-ONLY is non-nil message a warning instead of an error if
funiction is already a member of variable `*mon-default-start-load-sanity*' as per 
`mon-default-start-error/sane'.\n
:SEE :FILE site-lisp/mon-css-check.el | css-check.el
:SEE-ALSO .\n►►►"
(mon-default-start-error/sane
   'mon-set-csstidy-path-init warn-only
  (unless (bound-and-true-p css-check-csstidy-path)
    (let ((csstidy-path
           (or (executable-find "csstidy")
               (executable-find "csstidy.exe")
               (car (file-expand-wildcards
                     (concat (getenv "SP_BIN") "/csstidy/*.exe"))))))
      (when csstidy-path (setq css-check-csstidy-path csstidy-path)))) ))
;;
;; (mon-set-csstidy-path-init t)
(mon-set-csstidy-path-init)

;;; ==============================
;; :TOGGLE-LONG-LINES-MODE

;;; ==============================
;; :LONG-LINES-MODE->AUTO-MODE-ALIST
;;; :NOTE See `NOTES:' section "DON'T AUTO-MODE-ALIST" of the file's header for
;;; additional discussion.
;;: (add-to-list 'auto-mode-alist '("\\.dbc\\'" . longlines-mode))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-04-02T20:15:55-04:00Z}#{10136} - by MON KEY>
(defun mon-set-longlines-init ()
  "Ensure that `longlines-mode' gets invoked least once at Emacs init.\n
This is _necessary_ because a lot of `naf-mode' procedures fail if 
`longlines-mode' isn't in the environment.\n
Signal an error when `IS-NOT-MON-SYSTEM'.\n
:SEE-ALSO .\n►►►"
  (mon-default-start-error/sane
   'mon-set-longlines-init nil ;; just-warn
   (save-excursion
     (let (llm-test)
       (setq llm-test
             (with-temp-buffer
               (when (not (bound-and-true-p longlines-mode))
                 (longlines-mode))))
       (when llm-test 
         (message "Initialized `longlines-mode' at startup"))))))
;;
(mon-set-longlines-init)

(eval-when-compile (require 'rst nil t))
;;; ==============================
;;; :CHANGESET 1740
;;; :CREATED <Timestamp: #{2010-05-17T09:49:59-04:00Z}#{10201} - by MON KEY>
(defun mon-rst-mode-facification ()
  "Run on the `rst-mode-hook' to set rst-mode faces.\n
Hook added add loadtime with \`mon-default-start-error/sane'.\n
:SEE-ALSO `mon-set-rst-mode-faces'.\n►►►"
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
(defun mon-set-rst-mode-faces ()
  "Set rst-mode faces to MON preferred style.
When IS-MON-SYSTEM-P evaluated on the `rst-mode-hook'.
Signal an error when `IS-NOT-MON-SYSTEM'.\n
:EXAMPLE\n\n\(pp-display-expression 
 \(symbol-function 'mon-rst-mode-facification\) \"*MON-RST-MOD-FACIFICATION-FACES*\"\)\n
:SEE-ALSO `mon-rst-mode-facification'.\n►►►"
  (mon-default-start-error/sane
   'mon-set-rst-mode-faces nil ;; just-warn
   (add-hook 'rst-mode-hook 'mon-rst-mode-facification)))
;;
;; (remove-hook 'rst-mode-hook 'mon-set-rst-mode-faces)
;;
(mon-set-rst-mode-faces)

;;; ==============================
;;; :NOTE :BEFORE :FILE mon-keybindings.el
;;; :CREATED <Timestamp: #{2010-04-03T17:40:37-04:00Z}#{10136} - by MON KEY>
(defun mon-w32-key-init ()
  "Initialize w32 related keys on MON systems.\n
Binds the following variables:
 `w32-pass-rwindow-to-system', `w32-pass-lwindow-to-system'
 `w32-rwindow-modifier',  `w32-pass-multimedia-buttons-to-system'
:SEE :FILE src/w32fns.c
:SEE-ALSO `w32-alt-is-meta', `w32-pass-alt-to-system', `w32-quit-key',
`w32-phantom-key-code', `w32-enable-num-loc', `w32-enable-caps-lock',
`w32-scroll-lock-modifier', `w32-apps-modifier', `w32-mouse-button-tolerance',
`w32-mouse-move-interval', `w32-pass-extra-mouse-buttons-to-system',
`w32-pass-multimedia-buttons-to-system', `w32-register-hot-key',
`w32-unregister-hot-key', `w32-registered-hot-keys', `w32-reconstruct-hot-key',
`w32-toggle-lock-key'.\n►►►"
  (mon-default-start-error/sane
   'mon-w32-key-init nil ;; just-warn
   (when IS-MON-P-W32
     (setq-default
      w32-pass-lwindow-to-system nil
      w32-pass-rwindow-to-system nil
      w32-pass-multimedia-buttons-to-system nil
      w32-lwindow-modifier 'super
      w32-rwindow-modifier 'hyper
      ))))
;;
(mon-w32-key-init)

;;; ==============================
;;; :NOTE Load keybindings last to ensure everything is loaded in first.
(require 'mon-keybindings)

;;; ==============================
(provide 'mon-default-start-loads)
;;; ==============================

(eval-after-load "mon-default-start-loads" '(require 'mon-post-load-hooks))

;;; ================================================================
;;; mon-default-start-loads.el ends here
;;; EOF


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
;; `mon-build-path-for-load-path', `mon-set-bookmark-file', `mon-conkeror',
;; `mon-firefox',`mon-terminal', `mon-cmd', `mon-toggle-show-point-mode'
;; `mon-update-tags-tables', `mon-buffer-local-comment-start'
;; FUNCTIONS:◄◄◄
;; 
;; MACROS:
;;
;; CONSTANTS:
;;
;; VARIABLES:
;; `*mon-tags-table-list*'
;;
;; ADVISED:
;; `find-function-search-for-symbol' , `find-variable-noselect' 
;;
;; ALIASED/ADIVISED/SUBST'D:
;;
;; DEPRECATED:
;;
;; RENAMED:
;; `mon-actvt-show-point-mode' -> `mon-toggle-show-point-mode'
;; `bld-path-for-load-path'    -> `mon-build-path-for-load-path'
;; `mon-switch-bookmark-file'  -> `mon-set-bookmark-file'
;;
;; MOVED:
;; `mon-cmd'         -> mon-utils.el
;; `mon-terminal'    -> mon-utils.el
;;
;; REQUIRES:
;; mon-color-occur.el 
;; :NOTE mon-color-occur.el is a patched version of Matsushita Akihisa color-cccur.el
;; :SEE (URL `http://www.bookshelf.jp/elc/color-occur.el')
;;
;; TODO:
;; These should be hardwired to a more specific local path that we can keep our eye on.

;; `temporary-file-directory'
;; `small-temporary-file-directory'
;; `thumbs-temp-dir'
;; `thumbs-thumbsdir-auto-clean' 
;; `thumbs-thumbsdir-max-size' 
;; `thumbs-cleanup-thumbsdir'

;; :NOTE `auto-save-file-name-transforms' 
;;; (set (make-local-variable 'delete-auto-save-files) t)

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

;;; ==============================
;; :INFOPATH-SETUP

;;; ==============================
;;; :NOTE Evauluated by `mon-run-post-load-hooks'
;;; :CREATED <Timestamp: #{2010-03-22T17:31:14-04:00Z}#{10121} - by MON KEY>
(defun mon-set-infopath ()
  "Put current running Emacs' info directory on `Info-directory-list'.\n
This is mostly a W32 related fncn. Currenlty does nothing on GNU systems.\n
Evauluated by `mon-run-post-load-hooks'.\n
:NOTE to reset info :SEE info node `(emacs)General Variables'.\n
:NOTE The hook: \(add-hook 'before-init-hook 'w32-init-info\)
in :FILE lisp/w32-fns.el must be commented out for this to work properly.\n
:SEE-ALSO `w32-init-info', `mon-get-env-vars-emacs',
`Info-default-directory-list', `Info-directory-list',
`Info-additional-directory-list', `Info-dir-contents'.\n►►►"
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
               (delete "c:/emacs/info/" Info-default-directory-list))))))
;;
;;; :TEST-ME Info-directory-list
;;; :TEST-ME (mon-set-infopath)

;;; ==============================
;; :LOAD-PATH-SETUP

;;; ==============================
;;; :RENAMED `bld-path-for-load-path' -> `mon-build-path-for-load-path'
(defun mon-build-path-for-load-path (expand-path suffix-path) 
  "Return a path for `load-path' by concat'ing EXPAND-PATH and SUFFIX-PATH.\n
:SEE-ALSO `mon-build-path'.\n►►►"
  (concat (file-name-as-directory expand-path) suffix-path))

(add-to-list 'load-path  mon-emacs-root)
(add-to-list 'load-path  mon-site-lisp-root)
(add-to-list 'load-path  mon-user-emacsd)
(add-to-list 'load-path  mon-naf-mode-root)
(add-to-list 'load-path  mon-ebay-tmplt-mode-root)


;;; :NOTE This may cause problems when (not IS-BUG-P-REMOTE).
(when (and (bound-and-true-p IS-MON-SYSTEM-P) IS-MON-SYSTEM-P)
  (setq user-emacs-directory (file-name-as-directory mon-user-emacsd)))

;;; ==============================
;; :FONT-LOCK-FACES
;;; :NOTE This should be set before any code requiring color themes. I load here
;;;       instead of from custom to make sure that it happens now.

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;;; ==============================
;; :COLOR-THEME
;;; :NOTE The color-theme library needs to be loaded early _BEFORE_ `naf-mode'!.
;;;       Eventually we need to move away from the color-theme package it is
;;;       rapidly becoming obsolete with newer emacs 23.* and breaking changes
;;;       are in effect.
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(cond  (IS-BUG-P (color-theme-ld-dark)) ;; (color-theme-euphoria))       
       (IS-MON-P (color-theme-ld-dark)))

;;; ==============================
;; :PRE-CUSTOM-CUSTOMIZATIONS
;;; :NOTE Start setting customizations that we want before loading custom file.

(setq url-privacy-level 'paranoid)

(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)
;; (global-set-key [(f12)] 'ibuffer)

;;; ==============================
;; :IDO
;;; :NOTE Sometimes ido should ignore the *Completions* buffer.
;;; (add-to-list 'ido-ignore-buffers "\\*Completions\\*")

;;; ==============================
;;; :RENAMED `mon-switch-bookmark-file' -> `mon-set-bookmark-file'
;;; :COURTESY stefan@xsteve.at :VERSION 23.01.2001 :HIS xsteve-functions.el
;;; :MODIFICATIONS <Timestamp: 2009-08-09-W32-7T03:31:36-0400Z - by MON KEY>
(defun mon-set-bookmark-file (file)
  "Relocate where Emacs stores the bookmark file.\n
:NOTE The bookmark file typically has a .bmk extension.\n
:SEE info node `(emacs)Bookmarks'\n
:SEE-ALSO `bookmark-load', `bookmark-default-file', `bookmark-all-names'
`bookmark-save-flag', `bookmark-relocate'.\n►►►"
  (bookmark-load file t)
  (setq bookmark-default-file file))
;;
(mon-set-bookmark-file
 (concat (file-name-as-directory mon-user-emacsd) ".emacs.bmk"))

(when IS-MON-SYSTEM-P
  (setq bookmark-save-flag 1))

;;; ==============================
;; :HELP-MODE-MODS
;;; :NOTE I don't like the new 23.0 style UPCASE args in help.
;;; Where clarity is the concern would be nicer to map them to a face.
(setq help-downcase-arguments t)
;;
;;; :NOTE Ensure evaluating examples in help mode returns a nice long list.
(add-hook 'help-mode-hook 
          #'(lambda () (set (make-local-variable 'print-length) nil)))

;;; ==============================
;;; :COURTESY Tom Rauchenwald  
;;; :NOTE :SEE `NOTES:' section of this file's header for discussion.
;;; :CREATED <Timestamp: Thursday July 30, 2009 @ 06:17.42 PM - by MON>
(defadvice find-function-search-for-symbol 
;; CLASS NAME   [POSITION] [ARGLIST]             FLAG
  (after mon-adv1 last     (symbol type library) activate)
  (with-current-buffer (car ad-return-value)
    (unless view-mode (view-mode 1))))
;;
(defadvice find-variable-noselect 
;; CLASS NAME   [POSITION] [ARGLIST]             FLAG
  (after mon-adv2 last (variable &optional file) activate)
  (with-current-buffer (car ad-return-value)
    (unless view-mode (view-mode 1))))

;;; ==============================
;; :WOMAN-PATH

;;; :TODO Consider adding `convert-standard-filename' here for W32 path frobbing.
;;; :NOTE New cygwin/mingw/emacs installs and twiddling keep manpath wacky on w32.
;;; :MODIFICATIONS <Timestamp: #{2009-10-26T19:12:52-04:00Z}#{09441} - by MON>
;;; :MODIFICATIONS <Timestamp: #{2009-08-14T12:35:21-04:00Z}#{09335} - by MON>

;; C:\msys\share\man
(require 'woman)
(when IS-MON-P-W32
  (let ((wmn-p 
         (mapcar #'(lambda (this-csf) 
                     (let ((this-csf-file
                           (convert-standard-filename (file-truename this-csf))))
                       ;;(unless (null this-csf-file)
                       (when (file-exists-p this-csf-file) this-csf-file)))
                 `( ;; :CROSS-SITE-MAN
                   ,(concat (nth 5 (assoc 1 *mon-emacsd*)) "/cross-site-man")
                    
                    ;; :GNUWIN32
                    ,(when (getenv "SP_GNUW32")
                      (concat (file-name-directory (getenv "SP_GNUW32")) "man"))

                    ;; :MINGW
                    ,(when (getenv "MINGW")
                           (concat (file-name-directory (getenv "MINGW")) "share/man"))

                    ;; :MSYS
                    ,(when (getenv "MSYS") (concat (getenv "MSYS") "/share/man"))
                 
                    ;; :CYGWIN
                    ,@(when (and (getenv "SP_CYGWN") (file-exists-p (getenv "SP_CYGWN")))
                           (let ((cygman-root (file-name-directory (getenv"SP_CYGWN")))
                                 (cygmans
                                  '("usr/share/man" "usr/local/share/man" "usr/X11R6/share/man"
                                    "usr/ssl/man" "usr/local/man"))
                                 cygman-csf)
                             (dolist (csf cygmans (setq cygman-csf (nreverse cygman-csf)))
                               (push (concat cygman-root csf) cygman-csf))))))))
    (mapc #'(lambda (addp) 
              (unless (not addp) (add-to-list 'woman-manpath addp))) wmn-p)))
;;
;;; :TEST-ME woman-manpath

;;; ==============================
;;; :CREATED <Timestamp: #{2009-10-21T20:01:43-04:00Z}#{09434} - by MON>
(when IS-W32-P
  (setq thumbs-conversion-program
        (if (getenv "SP_IMGCK")
            (let ((xp-convert (car (directory-files (getenv "SP_IMGCK") t "imconvert"))))
              (if xp-convert xp-convert (executable-find "imconvert")))
            (executable-find "imconvert"))))

;;; ==============================
;; :CUSTOM-FILE
;;; :NOTE This sets the default `custom-file' to an alternate location.  This
;;;       helps to maintain portability, the location of custom is apparently
;;;       arbitrary across systems e.g. Debian, w32, etc. esp. where
;;;       upgrades/multiple installations are concerned. Likewise, as the custom
;;;       facility generally sucks and apt to overwrite my configs at _its_
;;;       perogative I find it cleaner and often helpful to keep a version
;;;       controlled copy in a location of _my_ choosing.
(setq custom-file
      (let ((mon-user-emacsd (file-name-as-directory mon-user-emacsd)))
	(cond 
	 (IS-MON-P-W32 (concat mon-user-emacsd (nth 2 (assoc 1 *mon-emacsd*))))
	 (IS-BUG-P (concat mon-user-emacsd (nth 2 (assoc 3 *mon-emacsd*))))
	 (IS-MON-P-GNU (concat mon-user-emacsd (nth 2 (assoc 2 *mon-emacsd*)))))))
;; (load custom-file t) ;no-error
(load custom-file)

;;; ==============================
;; :TEMP-FILES

;;; :TODO These should be hardwired to a more specific local path that we can keep our eye on.
;;; `temporary-file-directory'
;;; `small-temporary-file-directory'
;;; `thumbs-temp-dir'
;;; `thumbs-thumbsdir-auto-clean' 
;;; `thumbs-thumbsdir-max-size' 
;;; `thumbs-cleanup-thumbsdir'

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
;; :BEFORE-SLIME
;;; :NOTE This _should_ be set in site-local-private.el but just in case.
;;; `common-lisp-hyperspec-root', `common-lisp-hyperspec-issuex-table'
;;; `common-lisp-hyperspec-symbol-table'
(when (or IS-MON-P-GNU IS-MON-P-W32)
  (unless (and (bound-and-true-p common-lisp-hyperspec-root)
               (bound-and-true-p common-lisp-hyperspec-issuex-table)
               (bound-and-true-p common-lisp-hyperspec-symbol-table)))
  (setq common-lisp-hyperspec-root (nth 8 (mon-get-mon-emacsd-paths)))
  (setq common-lisp-hyperspec-issuex-table 
	(concat common-lisp-hyperspec-root "Data/Map_IssX.txt")) ;; "Issue-Cross-Refs.text"))
  (setq common-lisp-hyperspec-symbol-table 
	(concat common-lisp-hyperspec-root "Data/Map_Sym.txt"))) ;; "Symbol-Table.text"))))

;;; ==============================
(cond ((or IS-MON-P-W32 IS-BUG-P) (require 'mon-w32-load))
      (IS-MON-P-GNU (require 'mon-GNU-load)))

;;; ==============================
;; :TAGS-TABLES

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-21T19:02:12-04:00Z}#{09345} - by MON>
(defvar *mon-tags-table-list* 
  `(,mon-emacs-root ,mon-naf-mode-root ,mon-ebay-tmplt-mode-root ,mon-site-lisp-root)
  "*List of path for setting `tags-table-list'.
Look for TAGS files in directories specified by return values of:
 `mon-emacs-root', `mon-site-lisp-root'
 `mon-naf-mode-root', `mon-ebay-tmplt-mode-root'\n
:SEE-ALSO `mon-tags-apropos', `mon-tags-naf-apropos', `mon-update-tags-tables'.\n►►►")
;;
;;; :TEST-ME *mon-tags-table-list* 
;;
;;;(progn (makunbound '*mon-tags-table-list*) (unintern '*mon-tags-table-list*) )

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-21T19:27:44-04:00Z}#{09345} - by MON>
(defun mon-update-tags-tables ()
  "Update the 'TAGS' files in paths held by `*mon-tags-table-list*'\n
:SEE-ALSO `mon-tags-apropos',`mon-tags-naf-apropos', `*mon-tags-table-list*'.\n►►►"
  (interactive)
  (progn
    (shell-command (concat "etags " 
                           (nth 3 *mon-tags-table-list*)"/*.el " 
                           "--output=" (nth 3 *mon-tags-table-list*)"/TAGS"))
    (shell-command (concat  "etags " 
                            (nth 2 *mon-tags-table-list*)"/*.el "
                            "--output="(nth 2 *mon-tags-table-list*)"/TAGS "))
    (shell-command (concat "etags " 
                           (nth 1 *mon-tags-table-list*)"/*.el " 
                           "--include="(nth 2 *mon-tags-table-list*)"/TAGS " 
                           "--output=" (nth 1 *mon-tags-table-list*)"/TAGS"))
    (shell-command (concat  "etags " 
                            (nth 0 *mon-tags-table-list*)"/*.el " 
                            "--include="(nth 3 *mon-tags-table-list*)"/TAGS "
                            "--include="(nth 1 *mon-tags-table-list*)"/TAGS "
                            "--output=" (nth 0 *mon-tags-table-list*)"/TAGS"))
    t))
;;
;;; :TEST-ME (mon-update-tags-tables)
;;; :TEST-ME (call-interactively 'mon-update-tags-tables)

;;
;;; :NOTE Now load in the TAGS fils at startup.
(progn
  (when IS-MON-P  (mon-update-tags-tables))
  (setq tags-table-list *mon-tags-table-list*))

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
;;; :WORKING-AS-OF 
;;; :CREATED <Timestamp: Wednesday December 10, 2008 - by MON KEY> 
(add-to-list 'auto-mode-alist '("\\.naf\\'" . naf-mode))

;;; (autoload 'naf-mode "naf-mode" "A Mode for editing .naf files" t)

;;; ==============================
;; :SLIME
(when IS-MON-P-W32 (load-file "slime-loads.el"))

;;; ==============================
;; :EMACS-LISP-MODE-HOOKS
;;; :NOTE Following should be added/removed according to need:
;;; -*-truncate-lines: t; -*-
;;; (add-hook 'emacs-lisp-mode-hook '(lambda () (setq truncate-lines t)))
;;; (remove-hook 'emacs-lisp-mode-hook '(lambda () (setq truncate-lines t)))
(cond (IS-MON-P
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
                   (set (make-local-variable 'eval-expression-print-length) nil))))))

;;; ==============================
;;; :PROCED :SEE also `proced-auto-update-flag', `proced-auto-update-interval'
;;; :NOTE Load `proced' package at startup and have it update automatically.
;;; :TODO Should be loaded in a dedicated frame and marked not to kill.
;;; :CREATED <Timestamp: #{2009-12-18T00:53:55-05:00Z}#{09515} - by MON>
(progn
  (require 'proced)    
  (when proced-available
    ;; :NOTE `proced-toggle-auto-update' related fncns which clobber `match-data'.
    ;; (proced-toggle-auto-update 1)))
    (proced)))

;; Lets see the tty's when on a GNU/Linux box.
(when IS-MON-P-GNU
  (setq proced-format 'medium))

;;; ==============================
;; :DIRED-DETAILS
(require 'dired-details)
(dired-details-install)

;;; ==============================
;; :DOREMI
;;; :NOTE This is Drew Adams' Do Re Mi commands consolidated to one file.
;;;       Doesn't include the frame-fns.
;;; :CREATED <Timestamp: Tuesday February 24, 2009 @ 02:35.49 PM - by MON>
(when (or (file-exists-p (concat mon-site-lisp-root "/mon-doremi.elc"))
          (file-exists-p (concat mon-site-lisp-root "/mon-doremi.el")))
  (require 'mon-doremi))

;;; ==============================
;; :AUCTEX
(cond  (IS-MON-P-W32 
        (load "auctex.el" nil t t)
        (load "preview-latex.el" nil t t)))

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
(require 'mon-color-occur)
;;; :WAS (require 'color-occur)
;;; :NOTE mon-color-occur.el is a patched version of Matsushita Akihisa color-cccur.el
;;; :SEE (URL `http://www.bookshelf.jp/elc/color-occur.el')

;;; ==============================
;;; :NOTE No longer loading, but not deleting.
;;;
;;; (require 'google-define)  ;; Now inlined with google-define-redux.el 
;;; (require 'sregex)
;;; (require 'moccur-edit)
;;; (require 'color-moccur)
;;; (require 'auto-capitalize)
;;; (require 'dntw)  ;;; Deletes trailing whitespace from changed lines
;;; (require 'visible-mark)
;;; (require 'wordfreq)
;;; (require 'linum)
;;; (require 'breadcrumb)
;;; (require 'tagging)

;;; ==============================
;; :COMPLETION-INTERFACES
;;; (add-to-list 'load-path  (concat mon-site-lisp-root "/completion-ui"))
;;; (require 'completion-ui)
;;; (cond (IS-MON-P-W32 
;;;       (add-to-list 'load-path (concat mon-site-lisp-root "/company"))
;;;       (autoload 'company-mode "company" nil t)))

;;; (global-set-key ""  'company-complete-common)

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
;;;     (load-file  (concat mon-site-lisp-root "/cedet-cvs/common/cedet.el"))
;;;     (message "CEDET already loaded or your of a MONish way."))
;;; :USE `mon-load-cedet' instead. :SEE ./naf-mode/mon-utils.el

;;; ==============================
;; :DVC
(add-to-list 'load-path (mon-build-path-for-load-path mon-site-lisp-root "dvc/lisp"))
(if (featurep 'dvc-core)
    (dvc-reload)
    (require 'dvc-autoloads))

;;; :NOTE (dvc-current-active-dvc)
;;; :MODIFICATIONS <Timestamp: #{2010-03-20T17:24:42-04:00Z}#{10116} - by MON KEY>
(when (or IS-MON-P-W32 IS-BUG-P) 
  (setq dvc-sh-executable 
        ;; :WAS (cadr (assoc 'the-sh-pth *mon-misc-path-alist*))))
        (cond (IS-BUG-P
               ;; (plist-get (cadr (assoc 'the-sh-pth *mon-misc-path-alist*)) :cygwin))
               (cadr (assoc 'cygwin (cadr (assoc 'the-sh-pth *mon-misc-path-alist*)))))
              (IS-MON-P-W32
               ;;(plist-get (cadr (assoc 'the-sh-pth *mon-misc-path-alist*)) :msys)))))
               (cadr (assoc 'msys (cadr (assoc 'the-sh-pth *mon-misc-path-alist*))))))))

;;; :NOTE :SEE :FILE bzr-core.el :VARIABLE `bzr-executable'
;;; :CREATED <Timestamp: #{2010-01-01T11:58:49-05:00Z}#{10535} - by MON KEY>
(if IS-W32-P ;(eq system-type 'windows-nt)
    (if (executable-find "bzr")
        (setq bzr-executable (executable-find "bzr"))
        (setq bzr-executable "bzr")))

;;; :NOTE Turn off the ever so pervasive dvc-tips buffer.
(setq dvc-tips-enabled nil)

;;; ==============================
;;; :LOAD-SPECIFIC-PACKAGES
;; :NOTE These are only loaded when `IS-MON-P'
(when IS-MON-P
;;
;;; ==============================
;; :SHOW-POINT-MODE
;; :NOTE To ensure we've setup a show-point environment do it now.
(require 'show-point-mode)

;;; ==============================
;;; :NOTE Keep this here b/c it is needed when debugging.
;;; (add-hook 'emacs-lisp-mode-hook (function (lambda () (mon-toggle-show-point-mode)))
;;; :RENAMED `mon-actvt-show-point-mode' -> `mon-toggle-show-point-mode' 
(defun mon-toggle-show-point-mode ()
  "Toggle show-point-mode for current-buffer.\n
Used for hooks othwerwise is equivalent to calling `show-point-mode'.
:SEE-ALSO `mon-toggle-dired-dwim-target', `mon-toggle-eval-length',
`mon-toggle-menu-bar', `mon-toggle-truncate-line',
`mon-naf-mode-toggle-restore-llm', `mon-toggle-read-only-point-motion'
`mon-inhibit-modification-hooks', `mon-inhibit-point-motion-hooks',
`mon-inhibit-read-only'.\n►►►"
  (let ((is-show-point-mode 
         (buffer-local-value show-point-mode (current-buffer))))
    (cond ((not is-show-point-mode)(show-point-mode)))))
;;
;;; :TEST-ME (mon-toggle-show-point-mode)
;;


;;; ==============================
;; :TRAVERSELISP
;;; :NOTE Thiery's traverselisp is still a moving target. Should periodically
;;;       check that we've pulled a current version.
(add-to-list 'load-path (mon-build-path-for-load-path mon-site-lisp-root "traverselisp"))
(require 'traverselisp)
(add-to-list 'traverse-ignore-files ".naf~")

;;; ==============================              
;; :JST-MODE
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

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

;;; ==============================
;;; :TODO Entire conditional can be replaced with one call to `mon-get-mon-emacsd-paths'.
;; :WEB-BROWSER-RELATED
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
       (custom-set-default 'browse-url-generic-program (nth 9 (mon-get-mon-emacsd-paths)))))

;;; ==============================
;;; :MODIFICATIONS <Timestamp: #{2010-01-29T16:00:47-05:00Z}#{10045} - by MON KEY>
(custom-set-default 'browse-url-firefox-program (nth 10 (mon-get-mon-emacsd-paths)))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-21T20:39:57-05:00Z}#{10035} - by MON KEY>
(when (or IS-MON-P-GNU IS-MON-P-W32)
  (custom-set-default 'ffap-rfc-directories
                      (concat (nth 5 (mon-get-mon-emacsd-paths)) "/RFCS-HG")))

;;; ==============================
;; :LONG-LINES-MODE->AUTO-MODE-ALIST
;;; :NOTE :SEE `NOTES' section: "DON'T AUTO-MODE-ALIST" of file header for
;;;       discussion.
;;
;;: (add-to-list 'auto-mode-alist '("\\.dbc\\'" . longlines-mode))

;;; ==============================
;; :COMMENT-PREFIXING
;;; :NOTE To have comment prefix for '.dbc' files default to ";;;".
(add-hook 'find-file-hook 
	  (function (lambda ()
		      (when (string-equal (file-name-extension (buffer-file-name)) ".dbc"))
		      (set (make-local-variable 'comment-start) ";;;"))))
;;
;;; (set (make-local-variable 'comment-start) ";;;"))
;;; (setq-default comment-start ";;;")
;;; (default-value comment-start)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-09-26T19:08:09-04:00Z}#{09396} - by MON>
(defun mon-buffer-local-comment-start ()
  "Make \";;;\" a buffer-local-value for comment-start in fundamental-mode.\n
:CALLED-BY `'.\n►►►" 
  (let ((is-fundamental 
         (eq (cdr (assoc 'major-mode (buffer-local-variables)))'fundamental-mode)))
    (when is-fundamental
      (unless (buffer-local-value 'comment-start (current-buffer))
        (set (make-local-variable 'comment-start) ";;;")))))
;;
(add-hook 'first-change-hook 'mon-buffer-local-comment-start)
;;; (remove-hook 'first-change-hook 'mon-buffer-local-comment-start)

;;; ==============================
;; :TOGGLE-LONG-LINES-MODE
;;; Make sure we set `longlines-mode' at least once at Emacs startup.
;;; This is _necessary_ because a lot of `naf-mode' procedures DTWT otherwise.
(save-excursion
  (let (llm-test)
    (setq llm-test
	  (with-temp-buffer
	    (when (not (bound-and-true-p lonlines-mode))
	      (longlines-mode))))
    (when llm-test (message "longlines-mode initialized at startup"))))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-01-29T14:12:23-05:00Z}#{10045} - by MON KEY>
(add-hook 'rst-mode-hook
	  (function (lambda ()
            (progn
              (set-face-background 'rst-level-1-face  "SteelBlue")
              (set-face-foreground 'rst-level-2-face  "dark slate gray")
              (set-face-background 'rst-level-2-face  "slate blue")
              (set-face-foreground 'rst-level-2-face  "dark slate gray")
              (set-face-background 'rst-level-3-face  "medium slate blue")
              (set-face-foreground 'rst-level-3-face  "dark slate gray")
              (set-face-background 'rst-level-4-face  "light late blue")
              (set-face-foreground 'rst-level-4-face  "dark slate gray")))))

;;; ==============================
;;; :NOTE Load keybindings last to ensure everything is loaded in first.
(require 'mon-keybindings)

(setq-default lisp-indent-function 'common-lisp-indent-function)
;;; (setq lisp-indent-function 'lisp-indent-function)

;;; ==============================
(provide 'mon-default-start-loads)
;;; ==============================

(eval-after-load "mon-default-start-loads" '(require 'mon-post-load-hooks))

;;; ================================================================
;;; mon-default-start-loads.el ends here
;;; EOF


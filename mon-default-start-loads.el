;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; no-byte-compile: t; -*-
;;; this is mon-default-start-loads.el
;;; ================================================================
;;; DESCRIPTION:
;;; Provides functions required when initializing the Emacs startup 
;;; Environment. Makes numerous condtional tests on current user 
;;; `IS-BUG-P', `IS-MON-P-GNU', `IS-MON-P-W32'. Accordingly, makes the initial
;;; `require' calls for (among others) the following packages: 
;;; mon-utils.el, mon-w32-load.el, mon-GNU-load.el, color-theme.el, naf-mode.el,
;;; dired-details.el, mon-doremi.el, google-define.el, uniq.el, regexpl.el,
;;; register-list.el, color-occur.el, boxquote.el, dvc-autoloads.el,
;;; traverselisp.el, show-point-mode.el, apache-mode.el, mon-keybindings.el,
;;;
;;; FUNCTIONS:►►►
;;; `bld-path-for-load-path', `mon-switch-bookmark-file', `mon-conkeror',
;;; `mon-firefox',`mon-terminal', `mon-cmd', `mon-actvt-show-point-mode'
;;; `mon-update-tags-tables', `mon-buffer-local-comment-start'
;;; FUNCTIONS:◄◄◄
;;; 
;;; MACROS:
;;;
;;; CONSTANTS:
;;;
;;; VARIABLES:
;;; `*mon-tags-table-list*'
;;;
;;; ADVISE:
;;; `find-function-search-for-symbol' , `find-variable-noselect' 
;;;
;;; ALIASED/ADIVISED/SUBST'D:
;;;
;;; DEPRECATED:
;;;
;;; RENAMED:
;;;
;;; MOVED:
;;; `mon-cmd'         -> mon-utils.el
;;; `mon-terminal'    -> mon-utils.el
;;;
;;; REQUIRES:
;;;
;;; TODO:
;;;
;;; NOTES:
;;; == defavice for *Help* ==
;;; :SEE (URL `http://www.emacswiki.org/emacs-en/OpenQuestions#toc24')
;;; From *Help* buffer how to automatically examine *.el source in view-mode?
;;; C-h f view-mode
;;; With point over 'view.el' of Help buffer @ line 1
;;; M-x describe-text-properties
;;; :RETURNS
;;; "Here is a 'help-function-def' button labeled `view.el'. There are text
;;; properties here: button (t) category help-function-def-button help-args
;;; (view-mode "../emacs/lisp/view.el")"
;;; Most of the time, when I go to examine the source from Help I want to do so
;;; without having to worry about mucking it up accidentally esp. when the source is
;;; beneath "../emacs/lisp/*.el". In these situations I almost alwasy want to read
;;; the source not edit it. View-mode is my prefered way of examining source when I
;;; want only to read it as it allows me to page in a manner congruent with most
;;; GNU/nix environments/vi/less/more etc. Is it possible to hook into the help button
;;; actions to toggle view-mode when opening for an *.el file from Help? AFAICT Help
;;; is already leveraging view-mode in a non extensible manner. How does one get
;;; Emacs to jump over its own head?
;;;
;;; We used to add `.dbc'  extensions to `auto-mode-alist' with `longlines-mode'.
;;; However, as '.dbc' extension are used for any and _all_ notes/data re: DBC
;;; related material this was too broad a setting, and screwed up programmatic 
;;; creation of files with '.dbc' extension. 
;;;
;;; SNIPPETS:
;;;
;;; THIRD PARTY CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/mon-defaul-start-loads.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2009-09-23T12:18:55-04:00Z}#{09393} - by MON>
;;;
;;; FILE-CREATED:
;;; <Timestamp: Thursday January 15, 2009 @ 02:01.42 PM - by MON>
;;; HEADER-ADDED: <Timestamp: #{2009-08-17T13:08:30-04:00Z}#{09341} - by MON>
;;; ================================================================
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;;; Floor, Boston, MA 02110-1301, USA.
;;; ================================================================
;;; Permission is granted to copy, distribute and/or modify this
;;; document under the terms of the GNU Free Documentation License,
;;; Version 1.3 or any later version published by the Free Software
;;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;;; and no Back-Cover Texts. A copy of the license is included in
;;; the section entitled "GNU Free Documentation License".
;;; A copy of the license is also available from the Free Software
;;; Foundation Web site at:
;;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ==============================
;;; CODE:

;;; ==============================
;; :LOAD-PATH-SETUP
;; 
(defun bld-path-for-load-path (expand-path suffix-path) 
  "Concatenate EXPAND-PATH and SUFFIX-PATH.\n
:SEE-ALSO `mon-build-path'\n►►►"
  (concat (file-name-as-directory expand-path) suffix-path))

(add-to-list 'load-path  mon-emacs-root)
(add-to-list 'load-path  mon-site-lisp-root)
(add-to-list 'load-path  mon-user-emacsd)
(add-to-list 'load-path  mon-naf-mode-root)
(add-to-list 'load-path  mon-ebay-tmplt-mode-root)
;;
;;; :NOTE This may cause problems when (not IS-BUG-P-REMOTE).
;;; (cond (IS-MON-P-W32  
;;;       (setq user-emacs-directory (file-name-as-directory mon-user-emacsd))))

(setq user-emacs-directory (file-name-as-directory mon-user-emacsd))

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
;;; :COURTESY stefan@xsteve.at :VERSION 23.01.2001 :HIS xsteve-functions.el
;;; :MODIFICATIONS <Timestamp: 2009-08-09-W32-7T03:31:36-0400Z - by MON KEY>
(defun mon-switch-bookmark-file (file)
  "Relocate where Emacs stores the bookmark file.
bookmark file typically has a .bmk extension.\n
:SEE-ALSO `bookmark-load', `bookmark-default-file'.\n►►►"
  (bookmark-load file t)
  (setq bookmark-default-file file))
;;
(mon-switch-bookmark-file
 (concat (file-name-as-directory mon-user-emacsd) ".emacs.bmk"))

;;; ==============================
;; :WOMAN-PATH
;;; :NOTE New cygwin/mingw/emacs installs and twiddling keep manpath wacky on w32.
;;; :MODIFICATIONS <Timestamp: #{2009-10-26T19:12:52-04:00Z}#{09441} - by MON>
;;; :MODIFICATIONS <Timestamp: #{2009-08-14T12:35:21-04:00Z}#{09335} - by MON>
(cond ((and IS-MON-P-W32
       (file-exists-p (concat (getenv "HG_REPOS") "\\cross-site-man")))
       (require 'woman)
       (let ((wmn-p (mapcar 
                     (lambda (x) 
                       (replace-regexp-in-string  "\\\\" "/" x))
                     `(,(concat  (getenv "HG_REPOS") "\\cross-site-man")
                        ,(concat (getenv "HG_REPOS") "\\cross-site-man\\")
                        ,(concat (getenv "HOME") "\\bin\\Emacs\\EmacsW32\\gnuwin32\\man")
                        "C:/usr/share/man"
                        "C:/usr/local/share/man"
                        "C:/usr/X11R6/share/man" 
                        "C:/usr/ssl/man"
                        "C:/usr/local/man"))))
         (mapc (lambda (x) (add-to-list 'woman-manpath x)) wmn-p))))
;;
;;; :TEST-ME woman-manpath

;;; ==============================
;; :HELP-MODE-MODS
;;; :NOTE I don't like the new 23.0 style UPCASE args in help.
;;;       Nicer to map them for clarity to a face.
(setq help-downcase-arguments t)

;;; ==============================
;;; :COURTESY Tom Rauchenwald  
;;; :NOTE :SEE `NOTES:' section of this file's header for discussion.
;;; :CREATED <Timestamp: Thursday July 30, 2009 @ 06:17.42 PM - by MON>
(defadvice find-function-search-for-symbol 
;; CLASS NAME   [POSITION] [ARGLIST]             FLAG
  (after mon-adv1 last     (symbol type library) activate)
  (with-current-buffer (car ad-return-value)
    (view-mode 1)))
;;
(defadvice find-variable-noselect 
;; CLASS NAME   [POSITION] [ARGLIST]             FLAG
  (after mon-adv2 last (variable &optional file) activate)
  (with-current-buffer (car ad-return-value)
    (view-mode 1)))

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
	 (IS-MON-P-W32 (concat mon-user-emacsd (nth 2(assoc 1 *mon-emacsd*))))
	 (IS-BUG-P (concat mon-user-emacsd (nth 2(assoc 3 *mon-emacsd*))))
	 (IS-MON-P-GNU (concat mon-user-emacsd (nth 2(assoc 2 *mon-emacsd*)))))))
;; (load custom-file t) ;no-error
(load custom-file)

;;; ==============================
(cond ((or IS-MON-P-W32 IS-BUG-P)
       (require 'mon-w32-load))
      (IS-MON-P-GNU 
       (load (require 'mon-GNU-load))))

;;; ==============================
;; :TAGS-TABLES

;;; ==============================
;;; :CREATED <Timestamp: #{2009-08-21T19:02:12-04:00Z}#{09345} - by MON>
(defvar *mon-tags-table-list* 
  `(,mon-emacs-root ,mon-naf-mode-root ,mon-ebay-tmplt-mode-root ,mon-site-lisp-root)
  "*List of path for setting `tags-table-list'.
Look for TAGS files in `mon-emacs-root', `mon-naf-mode-root', and 
`mon-ebay-tmplt-mode-root'.\n
:SEE-ALSO `mon-tags-apropos', `mon-tags-naf-apropos', `mon-update-tags-tables'.
►►►")
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
(when IS-MON-P-W32 (load-file "./slime-loads.el"))

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
;; :PROCED
;;; :NOTE Load `proced' package at startup and have it update automatically.
;;; :TODO Should be loaded in a dedicated frame and marked not to kill.
;;; :CREATED <Timestamp: #{2009-12-18T00:53:55-05:00Z}#{09515} - by MON>
(progn
  (require 'proced)    
  (when proced-available
    (proced)
    (proced-toggle-auto-update 1)))

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
(require 'google-define)
(require 'uniq)
(require 'regexpl)
(require 'register-list)
(require 'color-occur)
(require 'boxquote)
(require 'dired-efap)
(require 'align-let)

;;; ==============================
;;; :NOTE No longer loading, but not deleting.
;;;
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
(add-to-list 'load-path (bld-path-for-load-path mon-site-lisp-root "dvc/lisp"))
(require 'dvc-autoloads)

;;; :NOTE When win32p dvc needs a sh executable.
(when (or IS-MON-P-W32 IS-BUG-P) 
  (setq dvc-sh-executable (cadr (assoc 'the-sh-pth *mon-misc-path-alist*))))

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
(defun mon-actvt-show-point-mode ()
  "Toggle show-point-mode for current-buffer.\n
Used for hooks othwerwise is equivalent to calling `show-point-mode'.\n►►►"
  (let ((is-show-point-mode 
         (buffer-local-value show-point-mode (current-buffer))))
    (cond ((not is-show-point-mode)(show-point-mode)))))
;;
;;; :TEST-ME (mon-actvt-show-point-mode)
;;; (add-hook 'emacs-lisp-mode-hook 'mon-actvt-show-point-mode)

;;; ==============================
;; :TRAVERSELISP
;;; :NOTE Thiery's traverselisp is still a moving target. Should periodically
;;;       check that we've pulled a current version.
(add-to-list 'load-path 
             (bld-path-for-load-path mon-site-lisp-root "traverselisp"))
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
;; :WEB-BROWSER-RELATED
(cond 
 (IS-BUG-P
  (setq browse-url-browser-function 'browse-url-generic)
  (setq	browse-url-generic-program 
        "c:/Program Files/Mozilla Firefox/firefox.exe"))
 (IS-MON-P-W32 
  (setq browse-url-browser-function 'browse-url-generic)
  (setq browse-url-generic-program 
        "c:/Program Files/Conkeror/conkeror-master/conkeror/conkeror.exe"))
 (IS-MON-P-GNU
  (setq browse-url-browser-function 'browse-url-generic)
  (setq	browse-url-generic-program "/usr/local/bin/conkeror")))

;;; ==============================
(cond (IS-MON-P-W32 
       (setq browse-url-firefox-program 
             "C:/Program Files/Mozilla Firefox/firefox.exe"))
      (IS-BUG-P 
       (setq browse-url-firefox-program  
             "C:/Program Files/Mozilla Firefox/firefox.exe")))

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
(defun mon-buffer-local-comment-start (&optional intrp)
  "Make \";;;\" a buffer-local-value for comment-start in fundamental-mode.\n
:CALLED-BY `'.\n►►►" 
  (let ((is-fundamental (eq (cdr (assoc 'major-mode (buffer-local-variables)))'fundamental-mode)))
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
  (let (test)
    (setq test
	  (with-temp-buffer
	    (when (not (bound-and-true-p lonlines-mode))
	      (longlines-mode))))
    (when test (message "longlines-mode initialized at startup"))))

;;; ==============================
;;; :NOTE Load keybindings last to ensure everything is loaded in first.
(require 'mon-keybindings)

(setq-default lisp-indent-function 'common-lisp-indent-function)
;;; (setq lisp-indent-function 'lisp-indent-function)

;;; ==============================
(provide 'mon-default-start-loads)
;;; ==============================

;;; ================================================================
;;; default-start-loads.el ends here
;;; EOF


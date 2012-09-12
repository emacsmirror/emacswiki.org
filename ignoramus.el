;;; ignoramus.el --- Ignore backups, build files, et al.
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/ignoramus
;; URL: http://raw.github.com/rolandwalker/ignoramus/master/ignoramus.el
;; Version: 0.5.0
;; Last-Updated:  3 Sep 2012
;; EmacsWiki: Ignoramus
;; Keywords: convenience, tools
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'dired-x)
;;     (require 'ignoramus)
;;
;;     (ignoramus-setup)    ; sets `vc-directory-exclusion-list',
;;                          ; `dired-omit-files', `ido-ignore-directories',
;;                          ; `completion-ignored-extensions', etc.
;;
;;     C-x C-j              ; backups and build files now omitted from dired
;;
;; Explanation
;;
;; Every library has its own method for defining uninteresting files
;; to ignore.  Ignoramus puts the listing of ignorable-file patterns
;; and the logic for applying those patterns together in one place.
;;
;; To use ignoramus, place the ignoramus.el library somewhere
;; Emacs can find it, and add the following to your ~/.emacs file:
;;
;;     (require 'ignoramus)
;;     (ignoramus-setup)
;;
;; By default, `ignoramus-setup' will apply every action that it
;; knows about for ignoring files.  Currently these are
;;
;;     comint
;;     completions
;;     dired
;;     eshell
;;     grep
;;     ido
;;     nav
;;     pcomplete
;;     projectile
;;     shell
;;     speedbar
;;     vc
;;
;; You can specify a shorter list of actions as an argument
;;
;;     (ignoramus-setup '(pcomplete shell ido))
;;
;; or customize the value of `ignoramus-default-actions'.
;;
;; See Also
;;
;;     M-x customize-group RET ignoramus RET
;;
;; Notes
;;
;; One function is provided to be called from Lisp:
;;
;;     `ignoramus-boring-p'
;;
;; Compatibility and Requirements
;;
;;     Tested on GNU Emacs versions 23.3 and 24.1
;;
;;     No external dependencies
;;
;; Bugs
;;
;;     The one-size-fits-all approach necessarily makes this library
;;     a blunt instrument.
;;
;;     Grep commands may become inconveniently long.
;;
;;     File names and directory names are conflated.
;;
;;     Works poorly on built-in completions (eg find-file).
;;
;;     Overwrites customizable variables from other libraries.
;;
;;     Hardcoded directory separator.
;;
;; TODO
;;
;;     Separate variables for directory names.
;;
;;     Complete eshell action.
;;
;;; License
;;
;; Simplified BSD License:
;;
;; Redistribution and use in source and binary forms, with or
;; without modification, are permitted provided that the following
;; conditions are met:
;;
;;    1. Redistributions of source code must retain the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials
;;       provided with the distribution.
;;
;; This software is provided by Roland Walker "AS IS" and any express
;; or implied warranties, including, but not limited to, the implied
;; warranties of merchantability and fitness for a particular
;; purpose are disclaimed.  In no event shall Roland Walker or
;; contributors be liable for any direct, indirect, incidental,
;; special, exemplary, or consequential damages (including, but not
;; limited to, procurement of substitute goods or services; loss of
;; use, data, or profits; or business interruption) however caused
;; and on any theory of liability, whether in contract, strict
;; liability, or tort (including negligence or otherwise) arising in
;; any way out of the use of this software, even if advised of the
;; possibility of such damage.
;;
;; The views and conclusions contained in the software and
;; documentation are those of the authors and should not be
;; interpreted as representing official policies, either expressed
;; or implied, of Roland Walker.
;;
;;; Code:
;;

;;; requires

(eval-when-compile
  ;; declarations for byte compiler
  (defvar dired-garbage-files-regexp)
  (defvar dired-omit-extensions)
  (defvar dired-omit-files)
  (defvar dired-omit-mode)
  (defvar eshell-cmpl-dir-ignore)
  (defvar eshell-cmpl-file-ignore)
  (defvar grep-find-ignored-directories)
  (defvar grep-find-ignored-files)
  (defvar ido-ignore-directories)
  (defvar ido-ignore-files)
  (defvar nav-boring-file-regexps)
  (defvar pcomplete-dir-ignore)
  (defvar pcomplete-file-ignore)
  (defvar shell-completion-fignore)
  (defvar speedbar-directory-unshown-regexp)
  (defvar speedbar-file-unshown-regexp)
  ;; for callf
  (require 'cl))

;;; variables

(defvar ignoramus-known-actions '(
                                  all
                                  comint
                                  completions
                                  dired
                                  eshell
                                  grep
                                  ido
                                  nav
                                  pcomplete
                                  projectile
                                  shell
                                  speedbar
                                  vc
                                  )
  "All known actions for ignoring unwanted files.")

(defvar ignoramus-boring-file-regexp nil "A computed regexp matching uninteresting files.")
(defvar ignoramus-boring-dir-regexp  nil "A computed regexp matching uninteresting directories.")

;;; customizable variables

;;;###autoload
(defgroup ignoramus nil
  "Ignore backups, build files, et al."
  :version "0.5.0"
  :link '(emacs-commentary-link "ignoramus")
  :prefix "ignoramus-"
  :group 'tools
  :group 'convenience)

(defcustom ignoramus-default-actions (remq 'all ignoramus-known-actions)
  "Which actions to use by default when ignoring unwanted files."
  :type `(repeat ,(append '(choice)
                          (mapcar #'(lambda (x)
                                      (list 'const x)) ignoramus-known-actions)))
  :group 'ignoramus)

;;;###autoload
(defgroup ignoramus-patterns nil
  "File patterns to ignore."
  :group 'ignoramus)

(defcustom ignoramus-file-endings
  '(
    ".386"                                    ; compiled binary
    ".a"                                      ; compiled binary
    ".acn"                                    ; latex
    ".acr"                                    ; latex
    ".alg"                                    ; latex
    ".ap_"                                    ; android
    ".apk"                                    ; android
    "_archive"                                ; emacs org-mode
    ".asv"                                    ; matlab
    "-autoloads.el"                           ; emacs package.el
    ".aux"                                    ; latex
    ".bak"                                    ; generic
    ".bbl"                                    ; bibtex
    ".beam"                                   ; erlang
    ".bin"                                    ; compiled binary
    ".blg"                                    ; bibtex
    ".cgo1.go"                                ; go
    ".cgo2.c"                                 ; go
    ".chi"                                    ; haskell
    ".chi.h"                                  ; haskell
    ".class"                                  ; java compiled
    ".com"                                    ; compiled binary
    ".cp"                                     ; texinfo
    ".cps"                                    ; texinfo
    ".d64fsl"                                 ; LISP
    ".dcu"                                    ; delphi
    ".dep"                                    ; make
    ".dex"                                    ; android
    ".dfsl"                                   ; LISP
    ".dll"                                    ; compiled binary
    ".drc"                                    ; delphi
    ".drv"                                    ; compiled binary
    ".dvi"                                    ; latex
    ".dx32fsl"                                ; LISP
    ".dx64fsl"                                ; LISP
    ".dxl"                                    ; LISP
    ".dylib"                                  ; compiled binary
    ".ear"                                    ; java
    ".elc"                                    ; emacs
    ".esproj"                                 ; espresso
    "-Ex.R"                                   ; R
    ".exe"                                    ; compiled binary
    ".fas"                                    ; LISP
    ".fasl"                                   ; LISP
    ".fdb_latexmk"                            ; latex
    ".fmx"                                    ; oracle
    ".fn"                                     ; texinfo
    ".fns"                                    ; texinfo
    ".fsl"                                    ; LISP
    ".fx32fsl"                                ; LISP
    ".fx64fsl"                                ; LISP
    ".gcda"                                   ; gcov
    ".gcno"                                   ; gcov
    ".gcov"                                   ; gcov
    ".glg"                                    ; latex
    ".glo"                                    ; latex
    ".gls"                                    ; latex
    ".gmo"                                    ; gettext
    ".hi"                                     ; haskell
    ".identcache"                             ; delphi
    ".ilg"                                    ; latex
    ".ilk"                                    ; visualstudio
    ".iml"                                    ; intellij
    ".ind"                                    ; latex
    ".ipr"                                    ; intellij
    ".ist"                                    ; latex
    ".iws"                                    ; intellij
    ".jar"                                    ; java
    ".ky"                                     ; texinfo
    ".kys"                                    ; texinfo
    ".la"                                     ; libtool
    ".lai"                                    ; libtool
    ".launch"                                 ; eclipse
    ".lbin"                                   ; compiled binary
    ".lib"                                    ; LISP
    ".lnk"                                    ; ms-windows
    ".lo"                                     ; libtool
    ".lock"                                   ; generic
    ".lof"                                    ; latex
    ".lot"                                    ; latex
    ".lx32fsl"                                ; LISP
    ".lx64fsl"                                ; LISP
    ".maf"                                    ; latex
    ".mem"                                    ; LISP
    ".min.js"                                 ; minified js
    "-min.js"                                 ; minified js
    ".mmx"                                    ; oracle
    ".mo"                                     ; gettext
    ".moved-aside"                            ; xcode
    ".mtc"                                    ; latex
    ".mtc0"                                   ; latex
    ".nav"                                    ; latex
    ".nlo"                                    ; latex
    ".o"                                      ; compiled binary
    ".obj"                                    ; compiled binary
    ".opensdf"                                ; visualstudio
    ".orig"                                   ; patch
    ".p64fsl"                                 ; LISP
    ".pdfsync"                                ; latex
    ".pfsl"                                   ; LISP
    ".pg"                                     ; texinfo
    ".pgs"                                    ; texinfo
    ".pid"                                    ; various
    ".pidb"                                   ; monodevelop
    ".plt"                                    ; erlang
    ".plx"                                    ; oracle
    ".pot"                                    ; django
    ".psess"                                  ; visualstudio
    ".Publish.xml"                            ; visualstudio
    ".pyc"                                    ; python
    ".pyd"                                    ; python
    ".pydevproject"                           ; eclipse
    ".pyo"                                    ; python
    ".rbc"                                    ; ruby
    ".rej"                                    ; patch
    ".sassc"                                  ; sass
    ".scc"                                    ; visualstudio
    ".sdf"                                    ; visualstudio
    ".seed"                                   ; node
    ".sln.docstates"                          ; visualstudio
    ".slo"                                    ; compiled binary
    ".snm"                                    ; latex
    ".so"                                     ; shared library
    ".sparcf"                                 ; LISP
    ".sublime-project"                        ; sublimetext
    ".sublime-workspace"                      ; sublimetext
    ".suo"                                    ; visualstudio
    ".swo"                                    ; vim
    ".swp"                                    ; vim
    ".sx32fsl"                                ; LISP
    ".sx64fsl"                                ; LISP
    ".synctex.gz"                             ; latex
    ".tfm"                                    ; latex
    ".toc"                                    ; latex
    ".tp"                                     ; texinfo
    ".tps"                                    ; texinfo
    ".ufsl"                                   ; LISP
    ".un~"                                    ; vim
    ".vr"                                     ; texinfo
    ".vrb"                                    ; latex
    ".vrs"                                    ; texinfo
    ".vsp"                                    ; visualstudio
    ".vspscc"                                 ; visualstudio
    ".vssscc"                                 ; visualstudio
    ".vxd"                                    ; ms-windows driver
    ".war"                                    ; java
    ".wx32fsl"                                ; LISP
    ".wx64fsl"                                ; LISP
    ".x86f"                                   ; LISP
    ".xdy"                                    ; latex
    ".zwc"                                    ; zsh
    "~"                                       ; emacs
;;  ".fmt"                                    ; latex
;;  ".idx"                                    ; latex
;;  ".log"                                    ; database
;;  ".out"                                    ; latex
;;  ".map"                                    ; various
;;  ".ln"                                     ; ms-windows
    )
  "List of file endings to ignore.

These are not regular expressions, but literal strings which
occur at the ends of file names to ignore."
  :type '(repeat string)
  :group 'ignoramus-patterns)

(defcustom ignoramus-file-beginnings
  '(
    ".#"                                   ; emacs
    "core."                                ; unix
    "._"                                   ; thumbnails
    "_cgo_export."                         ; go
    )
  "List of file beginnings to ignore.

These are not regular expressions, but literal strings which
occur at the beginnings of file or directory names to ignore.

The string to match comprises only the last element of a
fully-qualified pathname."
  :type '(repeat string)
  :group 'ignoramus-patterns)

(defcustom ignoramus-file-exact-names
  '(
    "$RECYCLE.BIN"                         ; ms-windows
    ".AppleDouble"                         ; OS X
    ".DS_Store"                            ; OS X
    ".DocumentRevisions-V100"              ; OS X
    ".LSOverride"                          ; OS X
    ".Rhistory"                            ; R
    ".Spotlight-V100"                      ; OS X
    ".TemporaryItems"                      ; OS X
    ".Trashes"                             ; OS X
    ".actionScriptProperties"              ; actionscript
    ".apt_generated"                       ; gwt
    ".build"                               ; perl
    ".buildpath"                           ; eclipse
    ".builds"                              ; visualstudio
    ".bzr"                                 ; bazaar
    ".cdv"                                 ; codeville
    ".classpath"                           ; eclipse
    ".com.apple.timemachine.donotpresent"  ; OS X
    ".com.apple.timemachine.supported"     ; OS X
    ".coverage"                            ; python
    ".cproject"                            ; eclipse
    ".directory"                           ; KDE
    ".dropbox"                             ; dropbox
    ".dropbox.cache"                       ; dropbox
    ".emacs.desktop"                       ; emacs desktop.el
    ".emacs.desktop.lock"                  ; emacs desktop.el
    ".eunit"                               ; erlang
    ".externalToolBuilders"                ; eclipse
    ".flexProperties"                      ; actionscript
    ".fseventsd"                           ; OS X
    ".git"                                 ; git
    ".hg"                                  ; mercurial
    ".idea"                                ; various
    ".ido.last"                            ; emacs ido-mode
    ".last_cover_stats"                    ; perl
    ".lein-deps-sum"                       ; leiningen
    ".loadpath"                            ; eclipse
    ".netrwhist"                           ; vim
    ".org-id-locations"                    ; emacs org-mode
    ".pc"                                  ; quilt
    ".project"                             ; eclipse
    ".projectile"                          ; emacs projectile
    ".recentf"                             ; emacs recentf
    ".redcar"                              ; redcar
    ".rspec"                               ; rails
    ".sass-cache"                          ; sass
    ".scala_dependencies"                  ; scala
    ".svn"                                 ; subversion
    ".tmproj"                              ; textmate
    ".tmproject"                           ; textmate
    ".tox"                                 ; python
    ".wmncach.el"                          ; emacs WoMan
    ".yardoc"                              ; yard
    "_MTN"                                 ; monotone
    "__history"                            ; delphi
    "_build"                               ; perl
    "_cgo_defun.c"                         ; go
    "_cgo_gotypes.go"                      ; go
    "_darcs"                               ; darcs
    "_obj"                                 ; go
    "_sgbak"                               ; vault
    "_site"                                ; jekyll
    "_test"                                ; go
    "_testmain.go"                         ; go
    "_yardoc"                              ; yard
    "aclocal.m4"                           ; automake
    "auto-save-list"                       ; emacs
    "autom4te.cache"                       ; autoconf
    "bin-debug"                            ; various
    "bin-release"                          ; various
    "blib"                                 ; perl
    "build"                                ; various
    "Build"                                ; various
    "Build.bat"                            ; perl
    "cmake_install.cmake"                  ; cmake
    "CMakeCache.txt"                       ; cmake
    "CMakeFiles"                           ; cmake
    "cover_db"                             ; perl
    "cscope.csd"                           ; cscope
    "cscope.files"                         ; cscope
    "cscope.inc"                           ; cscope
    "cscope.lst"                           ; cscope
    "cscope.out"                           ; cscope
    "cscope.out.po"                        ; cscope
    "cscope.tmplist"                       ; cscope
    "CVS"                                  ; CVS
    "Debug"                                ; various
    "debug"                                ; various
    "depcomp"                              ; automake
    "DerivedData"                          ; xcode
    "Desktop.ini"                          ; ms-windows
    "ehthumbs.db"                          ; ms-windows
    "gwt-unitCache"                        ; gwt
    "gwt_bree"                             ; gwt
    "install-sh"                           ; automake
    "install_manifest.txt"                 ; cmake
    "InstalledFiles"                       ; ruby
    "Makefile.in"                          ; automake
    "MCVS"                                 ; meta-CVS
    "META.yml"                             ; perl
    "minimal-session-saver-data.el"        ; emacs minimal-session-saver
    "MYMETA.yml"                           ; perl
    "nbbuild"                              ; netbeans
    "nbdist"                               ; netbeans
    "nosetests.xml"                        ; python
    "pm_to_blib"                           ; perl
    "Profile"                              ; various
    "profile"                              ; various
    "RCS"                                  ; RCS
    "Release"                              ; various
    "release"                              ; various
    "SCCS"                                 ; SCCS
    "Session.vim"                          ; vim
    "slprj"                                ; matlab
    "TAGS"                                 ; ctags/etags
    "tags"                                 ; ctags/etags
    "TestResult"                           ; visualstudio
    "testresult"                           ; visualstudio
    "Thumbs.db"                            ; ms-windows
    "tmtags"                               ; textmate
    "tramp"                                ; emacs tramp
    "xcuserdata"                           ; xcode
    "xhtml-loader.rnc"                     ; emacs nxhtml
    "{arch}"                               ; arch - todo is this correct?
    "~.dep"                                ; xcode
    "~.dot"                                ; xcode
    "~.nib"                                ; xcode
    "~.plst"                               ; xcode
    )
  "List of exact filenames to ignore.

These are not regular expressions, but literal strings which
exactly match a file or directory name to ignore.

The string to match comprises only the last element of a
fully-qualified pathname."
  :type '(repeat string)
  :group 'ignoramus-patterns)

(defcustom ignoramus-file-regexps
  '(
    "\\`#.*#\\'"                              ; emacs
    "\\`.*\\.mex[^.]*\\'"                     ; matlab
    "\\`Icon.?\\'"                            ; OS X thumbnails
    "\\`\\..*\\.sw[a-z]\\'"                   ; vim
    "\\`\\.yas.*\\.el\\'"                     ; emacs yasnippet
    "\\`bzr_log\\.[[:alnum:]]+\\'"            ; emacs
    "\\`hg-editor-[[:alnum:]]+\\.txt\\'"      ; emacs
    "\\`svn-commit\\.tmp\\'"                  ; emacs
    "\\`zshecl[0-9]+\\'"                      ; zsh
    )
  "List of regexps matching filenames to ignore.

The string to match comprises only the last element of a
fully-qualified pathname."
  :type '(repeat regexp)
  :group 'ignoramus-patterns)

;;; utility functions

(defun ignoramus-compute-common-regexps ()
  "Compute common regexps used by plugins."
  (setq ignoramus-boring-dir-regexp (concat
                                     "\\`" (regexp-opt ignoramus-file-beginnings)         "\\|"
                                     "\\`" (regexp-opt ignoramus-file-exact-names) "\\'"  "\\|"
                                     (mapconcat 'identity ignoramus-file-regexps "\\|")))
  (setq ignoramus-boring-file-regexp (concat
                                      "\\`" (regexp-opt ignoramus-file-beginnings)         "\\|"
                                            (regexp-opt ignoramus-file-endings)     "\\'"  "\\|"
                                      "\\`" (regexp-opt ignoramus-file-exact-names) "\\'"  "\\|"
                                      (mapconcat 'identity ignoramus-file-regexps "\\|"))))

;;;###autoload
(defun ignoramus-boring-p (file)
  "Return non-nil if ignoramus thinks FILE is uninteresting."
  (unless ignoramus-boring-file-regexp
    (ignoramus-compute-common-regexps))
  (string-match-p ignoramus-boring-file-regexp (file-name-nondirectory file)))

;;; configuration action plugins

;; Howto: Create a function named ignoramus-do-ignore-ACTION,
;;        and add ACTION to `ignoramus-known-actions'.


(defun ignoramus-do-ignore-vc ()
  "Tell `vc-mode' to ignore unwanted files."
  (setq vc-directory-exclusion-list ignoramus-file-exact-names))


(defun ignoramus-do-ignore-grep ()
  "Tell `grep-mode' to ignore unwanted files."
  (setq grep-find-ignored-files (cons ".#*" (delq nil (mapcar #'(lambda (pat)
                                                                  (concat "*" pat)) ignoramus-file-endings))))
  (setq grep-find-ignored-directories ignoramus-file-exact-names))


(defun ignoramus-do-ignore-shell ()
  "Tell `shell-mode' to ignore unwanted files."
  (setq shell-completion-fignore ignoramus-file-endings))


(defun ignoramus-do-ignore-comint ()
  "Tell `comint-mode' and derived modes to ignore unwanted files."
  (setq comint-completion-fignore ignoramus-file-endings))


(defun ignoramus-do-ignore-completions ()
  "Tell built-in completions to ignore unwanted files."
  (setq completion-ignored-extensions (append
                                       ignoramus-file-endings
                                       (mapcar (lambda (pat)
                                                 (concat pat "/")) ignoramus-file-exact-names))))


(defun ignoramus-do-ignore-nav ()
  "Tell `nav-mode' to ignore unwanted files."
  (setq nav-boring-file-regexps (list ignoramus-boring-file-regexp)))


(defun ignoramus-do-ignore-ido ()
  "Tell `ido-mode' to ignore unwanted files."
  (setq ido-ignore-directories (list "\\`\\.\\./" "\\`\\./" ignoramus-boring-dir-regexp))
  (setq ido-ignore-files       (list "\\`\\.\\./" "\\`\\./" ignoramus-boring-file-regexp)))


(defun ignoramus-do-ignore-eshell ()
  "Tell `eshell' to ignore unwanted files."
  (setq eshell-cmpl-file-ignore ignoramus-boring-file-regexp)
  ;; todo use more patterns here
  (setq eshell-cmpl-dir-ignore (concat "\\`"
                                       (regexp-opt (append '("." "..") ignoramus-file-exact-names))
                                       "/\\'")))


(defun ignoramus-do-ignore-dired ()
  "Tell `dired-mode' to ignore unwanted files."

  (setq dired-omit-mode t)
  (add-hook 'dired-mode-hook #'(lambda ()
                                 (when (eq major-mode 'dired-mode)
                                   (dired-omit-mode 1))))

  ;; Ignoramus merges the patterns for "garbage" (dired.el) and
  ;; "omit" (dired-x.el) so they are identical.

  ;; The "garbage" regexp is used for an interactive mark command in dired.el.
  (setq dired-garbage-files-regexp ignoramus-boring-file-regexp)

  ;; The "omit" regexps affect dired-x.el when `dired-omit-mode' is set.
  (setq dired-omit-extensions ignoramus-file-endings)
  (setq dired-omit-files ignoramus-boring-file-regexp))


(defun ignoramus-do-ignore-projectile ()
  "Tell `projectile-mode' to ignore unwanted files."

  ;; according to what projectile expects
  ;; (setq projectile-ignored-file-extensions (mapcar #'(lambda (ext)
  ;;                                                      (replace-regexp-in-string "\\`\\." "" ext))
  ;;                                                 ignoramus-file-endings))
  ;; (setq projectile-ignored-files ignoramus-file-exact-names)
  ;; (setq projectile-ignored-directories ignoramus-file-exact-names)

  ;; a better test than what projectile does - note no longer just extensions
  (eval-after-load "projectile"
    '(progn
       (defun projectile-ignored-extension-p (file)
         (let ((case-fold-search t))
           (string-match-p ignoramus-boring-file-regexp file))))))


(defun ignoramus-do-ignore-speedbar ()
  "Tell `speedbar-mode' to ignore unwanted files."
  (setq speedbar-directory-unshown-regexp ignoramus-boring-dir-regexp)
  (setq speedbar-file-unshown-regexp ignoramus-boring-file-regexp))


(defun ignoramus-do-ignore-pcomplete ()
  "Tell `pcomplete' to ignore unwanted files."
  (setq pcomplete-dir-ignore ignoramus-boring-dir-regexp)
  (setq pcomplete-file-ignore ignoramus-boring-file-regexp))

;;; principal external interface

;;;###autoload
(defun ignoramus-setup (&optional actions)
  "Turn on ignoring files for all members of sequence ACTIONS.

ACTIONS is optional, and defaults to the value of
`ignoramus-default-actions'.

If ACTIONS contains 'all, turn on ignoring files for all
actions in `ignoramus-known-actions'."
  (callf or actions ignoramus-default-actions)
  (when (symbolp actions)
    (callf list actions))
  (when (memq 'all actions)
    (setq actions ignoramus-known-actions))
  (setq actions (remq 'all actions))
  (ignoramus-compute-common-regexps)
  (dolist (action actions)
    (let ((func (intern-soft (format "ignoramus-do-ignore-%s" action))))
      (unless func
        (error "No such action %s" action))
      (funcall func))))

(provide 'ignoramus)

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; End:
;;
;; LocalWords: Ignoramus ARGS alist Howto pathname dired ignorable
;; LocalWords: callf
;;

;;; ignoramus.el ends here

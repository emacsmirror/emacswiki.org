;;; ignoramus.el --- Ignore backups, build files, et al.
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/ignoramus
;; URL: http://raw.github.com/rolandwalker/ignoramus/master/ignoramus.el
;; Version: 0.7.0
;; Last-Updated: 22 Oct 2013
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
;; Three functions are provided to be called from Lisp:
;;
;;     `ignoramus-boring-p'
;;     `ignoramus-register-datafile'
;;     `ignoramus-matches-datafile'
;;
;; Compatibility and Requirements
;;
;;     GNU Emacs version 24.4-devel     : yes, at the time of writing
;;     GNU Emacs version 24.3           : yes
;;     GNU Emacs version 23.3           : yes
;;     GNU Emacs version 22.2           : yes, with some limitations
;;     GNU Emacs version 21.x and lower : unknown
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
;;     Different results may be obtained from the datafiles
;;     functions depending on whether external libraries are
;;     loaded.
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
;;; Change Log:
;;
;; 09 Oct 2012
;;     Incompatible change: defcustom variable names changed in
;;     anticipation of separating file and directory components.
;;     Obsolete aliases are included.
;;
;;; Code:
;;

;;; requirements

;; for callf
(require 'cl)

(autoload 'dired-omit-mode "dired-x" "Toggle omission of uninteresting files in Dired (Dired-Omit mode)." t)

;;; declarations

(eval-when-compile
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
  (defvar vc-directory-exclusion-list)
  (defvar comint-completion-fignore))

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

(defvar ignoramus-datafile-basename '(
                                      desktop-base-file-name
                                      ".emacs.desktop.lock"
                                      )
  "List of symbols or strings holding file basenames used for persistence by Emacs packages.")

(defvar ignoramus-datafile-completepath '(
                                          abbrev-file-name
                                          ac-comphist-file
                                          bm-repository-file
                                          bmkp-bmenu-commands-file
                                          bmkp-bmenu-state-file
                                          bookmark-default-file
                                          flymake-log-file-name
                                          guess-style-override-file
                                          ido-save-directory-list-file
                                          minimal-session-saver-data-file
                                          recentf-save-file
                                          save-place-file
                                          savehist-file
                                          smex-save-file
                                          tramp-persistency-file-name
                                          woman-cache-filename
                                          )
  "List of symbols or strings holding complete paths used for persistence by Emacs packages.")

(defvar ignoramus-datafile-prefix '(
                                    auto-save-list-file-prefix
                                    )
  "List of symbols or strings holding file prefixes used for persistence by Emacs packages.

A prefix is a leading absolute path component plus leading fragment of basename.")

(defvar ignoramus-datafile-dirprefix '(
                                       ac-dictionary-directories
                                       ede-simple-save-directory
                                       eshell-directory-name
                                       pcache-directory
                                       semanticdb-default-save-directory
                                       tramp-auto-save-directory
                                       url-configuration-directory
                                       )
  "List of symbols or strings holding directory prefixes used for persistence by Emacs packages.

A directory prefix is a leading absolute path component.")

(defvar ignoramus-datafile-computed-basenames nil
  "A computed value based on `ignoramus-datafile-basename'.")

(defvar ignoramus-datafile-computed-completepaths nil
  "A computed value based on `ignoramus-datafile-completepath'.")

(defvar ignoramus-datafile-computed-prefixes nil
  "A computed value based on `ignoramus-datafile-prefix'.")

(defvar ignoramus-datafile-computed-dirprefixes nil
  "A computed value based on `ignoramus-datafile-dirprefix'.")

(defvar ignoramus-datafile-computed-basenames-regexp nil
  "A computed value based on `ignoramus-datafile-basename'.")

(defvar ignoramus-datafile-computed-completepaths-regexp nil
  "A computed value based on `ignoramus-datafile-completepath'.")

(defvar ignoramus-datafile-computed-prefixes-regexp nil
  "A computed value based on `ignoramus-datafile-prefix'.")

(defvar ignoramus-datafile-computed-dirprefixes-regexp nil
  "A computed value based on `ignoramus-datafile-dirprefix'.")

;;; customizable variables

;;;###autoload
(progn
  ;; obsolete forms
  (define-obsolete-variable-alias 'ignoramus-file-endings     'ignoramus-file-basename-endings)
  (define-obsolete-variable-alias 'ignoramus-file-beginnings  'ignoramus-file-basename-beginnings)
  (define-obsolete-variable-alias 'ignoramus-file-exact-names 'ignoramus-file-basename-exact-names)
  (define-obsolete-variable-alias 'ignoramus-file-regexps     'ignoramus-file-basename-regexps))

;;;###autoload
(defgroup ignoramus nil
  "Ignore backups, build files, et al."
  :version "0.7.0"
  :link '(emacs-commentary-link :tag "Commentary" "ignoramus")
  :link '(url-link :tag "GitHub" "http://github.com/rolandwalker/ignoramus")
  :link '(url-link :tag "EmacsWiki" "http://emacswiki.org/emacs/Ignoramus")
  :prefix "ignoramus-"
  :group 'tools
  :group 'convenience)

(defcustom ignoramus-default-actions (remq 'all ignoramus-known-actions)
  "Which actions to use by default when ignoring unwanted files."
  :type `(repeat ,(append '(choice)
                          (mapcar #'(lambda (x)
                                      (list 'const x)) ignoramus-known-actions)))
  :group 'ignoramus)

(defcustom ignoramus-use-known-datafiles t
  "Whether to read variables from other packages to recognize datafiles.

When this option is set, ignoramus reads the current settings of
variables such as `tramp-auto-save-directory' or `woman-cache-filename'
to supplement its lists of regular expressions."
  :type 'boolean
  :group 'ignoramus)

(defcustom ignoramus-case-insensitive t
  "Make string and regexp matches case-insensitive where possible.

This affects the results from `ignoramus-boring-p' and
`ignoramus-matches-datafile', but generally does not affect
the behavior of other libraries configured by ignoramus."
  :type 'boolean
  :group 'ignoramus)

;;;###autoload
(defgroup ignoramus-patterns nil
  "File patterns to ignore."
  :group 'ignoramus)

(defcustom ignoramus-file-basename-endings
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
    ".ttc"                                    ; template toolkit
    ".tfm"                                    ; latex
    ".tmproj"                                 ; textmate
    ".tmproject"                              ; textmate
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

(defcustom ignoramus-file-basename-beginnings
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

(defcustom ignoramus-file-basename-exact-names
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
    ".prove"                               ; perl
    ".puppet-bak"                          ; puppet
    ".recentf"                             ; emacs recentf
    ".redcar"                              ; redcar
    ".rspec"                               ; rails
    ".sass-cache"                          ; sass
    ".scala_dependencies"                  ; scala
    ".svn"                                 ; subversion
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
    "COMMIT_EDITMSG"                       ; git
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
    "git-rebase-todo"                      ; git
    "gwt-unitCache"                        ; gwt
    "gwt_bree"                             ; gwt
    "install-sh"                           ; automake
    "install_manifest.txt"                 ; cmake
    "InstalledFiles"                       ; ruby
    "Makefile.in"                          ; automake
    "Makefile.old"                         ; perl
    "MCVS"                                 ; meta-CVS
    "META.yml"                             ; perl
    "MERGE_MSG"                            ; git
    "minimal-session-saver-data.el"        ; emacs minimal-session-saver
    "MYMETA.yml"                           ; perl
    "nbbuild"                              ; netbeans
    "nbdist"                               ; netbeans
    "nosetests.xml"                        ; python
    "nytprof"                              ; perl
    "nytprof.out"                          ; perl
    "perltidy.ERR"                         ; perl
    "pm_to_blib"                           ; perl
    "Profile"                              ; various
    "profile"                              ; various
    "RCS"                                  ; RCS
    "Release"                              ; various
    "release"                              ; various
    "SCCS"                                 ; SCCS
    "Session.vim"                          ; vim
    "slprj"                                ; matlab
    "SQUASH_MSG"                           ; git
    "TAGS"                                 ; ctags/etags
    "TAG_EDITMSG"                          ; git
    "tags"                                 ; ctags/etags
    "TestResult"                           ; visualstudio
    "testresult"                           ; visualstudio
    "Thumbs.db"                            ; ms-windows
    "tmtags"                               ; textmate
    "xcuserdata"                           ; xcode
    "xhtml-loader.rnc"                     ; emacs nxhtml
    "{arch}"                               ; arch - todo is this correct?
    "~.dep"                                ; xcode
    "~.dot"                                ; xcode
    "~.nib"                                ; xcode
    "~.plst"                               ; xcode
    "test.out"                             ; generic testing
    "test_out"                             ; generic testing
    "test.output"                          ; generic testing
    "test_output"                          ; generic testing
    )
  "List of exact filenames to ignore.

These are not regular expressions, but literal strings which
exactly match a file or directory name to ignore.

The string to match comprises only the last element of a
fully-qualified pathname."
  :type '(repeat string)
  :group 'ignoramus-patterns)

(defcustom ignoramus-file-basename-regexps
  '(
    "\\`#.*#\\'"                              ; emacs
    "\\`.*\\.mex[^.]*\\'"                     ; matlab
    "\\`Icon.?\\'"                            ; OS X thumbnails
    "\\`\\..*\\.sw[a-z]\\'"                   ; vim
    "\\`\\.yas.*\\.el\\'"                     ; emacs yasnippet
    "\\`\\..*~undo-tree~\\'"                  ; emacs undo-tree
    "\\`bzr_log\\.[[:alnum:]]+\\'"            ; emacs
    "\\`hg-editor-[[:alnum:]]+\\.txt\\'"      ; emacs
    "\\`svn-commit\\.tmp\\'"                  ; emacs
    "\\`zshecl[0-9]+\\'"                      ; zsh
    "\\`bash-fc-[0-9]+\\'"                    ; bash
    )
  "List of regexps matching filenames to ignore.

The string to match comprises only the last element of a
fully-qualified pathname."
  :type '(repeat regexp)
  :group 'ignoramus-patterns)

;;; compatibility functions

(unless (fboundp 'string-match-p)
  ;; added in 23.x
  (defun string-match-p (regexp string &optional start)
    "Same as `string-match' except this function does not change the match data."
    (let ((inhibit-changing-match-data t))
      (string-match regexp string start))))

(unless (fboundp 'string-prefix-p)
  ;; added in 23.x
  (defun string-prefix-p (str1 str2 &optional ignore-case)
    "Return non-nil if STR1 is a prefix of STR2.
If IGNORE-CASE is non-nil, the comparison is done without paying attention
to case differences."
    (eq t (compare-strings str1 nil nil
                           str2 0 (length str1) ignore-case))))

;;; utility functions

;; generic functions

(defun ignoramus--overly-broad-path-p (str-val)
  "Identify path elements which would match too broadly to be useful.

Also identify bogons."
  (or (not (string-match-p "[^ ]" str-val))
      (string-match-p "\\`/*\\'" str-val)
      (string-match-p "\\`~/*\\'" str-val)
      (string-match-p (downcase (concat "\\`" (expand-file-name "~") "/*\\'")) (downcase str-val))))

(defun ignoramus--string-or-symbol (str-or-sym)
  "Return the string for STR-OR-SYM."
  (cond
    ((null str-or-sym)
     nil)
    ((and (symbolp str-or-sym)
          (boundp str-or-sym))
     (ignoramus--string-or-symbol (symbol-value str-or-sym)))
    ((and (stringp str-or-sym)
          (ignoramus--overly-broad-path-p str-or-sym))
     nil)
    ((stringp str-or-sym)
     str-or-sym)
    ((consp str-or-sym)
     (mapcar 'ignoramus--string-or-symbol str-or-sym))
    (t
     nil)))

(defun ignoramus-list-flatten (list)
  "Flatten LIST which may contain other lists."
  (cond
    ((null list)
     nil)
    ((and (listp list)
          (consp (car list)))
     (append (ignoramus-list-flatten (car list)) (ignoramus-list-flatten (cdr list))))
    ((listp list)
     (cons (car list) (ignoramus-list-flatten (cdr list))))
    (t
     (list list))))

(defun ignoramus--extract-strings (arg)
  "Return a list of strings which may be contained in or referred to in ARG."
  (remove-if-not 'stringp
                 (ignoramus-list-flatten
                  (ignoramus--string-or-symbol arg))))

(defun ignoramus--extract-pathstrings (arg)
  "Return a list of path strings which may be contained in or referred to in ARG."
  (mapcar 'expand-file-name
          (ignoramus--extract-strings arg)))

(defun ignoramus-strip-trailing-slash (path)
  "Remove any trailing slashes from directory string PATH.

On non-POSIX systems, remove the appropriate directory separator
character for that system."
  (while (not (string-equal path (setq path (directory-file-name path)))) t)
  path)

(defun ignoramus-ensure-trailing-slash (path)
  "Ensure that directory string PATH has a trailing slash.

On non-POSIX systems, ensure the appropriate directory separator
character for that system."
  (setq path (ignoramus-strip-trailing-slash path))
  (file-name-as-directory path))

;; regular expression functions
(defun ignoramus-compute-common-regexps ()
  "Compute common regexps used by plugins."
  (setq ignoramus-boring-dir-regexp ignoramus-file-basename-regexps)
  (when ignoramus-file-basename-exact-names
    (push (concat "\\`" (regexp-opt (append ignoramus-file-basename-exact-names
                                            (if ignoramus-use-known-datafiles
                                                (ignoramus--extract-strings ignoramus-datafile-basename)
                                              nil))) "\\'")
          ignoramus-boring-dir-regexp))
  (when ignoramus-file-basename-beginnings
    (push (concat "\\`" (regexp-opt ignoramus-file-basename-beginnings))
          ignoramus-boring-dir-regexp))
  (when ignoramus-boring-dir-regexp
    (setq ignoramus-boring-dir-regexp (mapconcat 'identity ignoramus-boring-dir-regexp "\\|")))
  (setq ignoramus-boring-file-regexp ignoramus-file-basename-regexps)
  (when ignoramus-file-basename-exact-names
    (push (concat "\\`" (regexp-opt (append ignoramus-file-basename-exact-names
                                            (if ignoramus-use-known-datafiles
                                                (ignoramus--extract-strings ignoramus-datafile-basename)
                                              nil))) "\\'")
          ignoramus-boring-file-regexp))
  (when ignoramus-file-basename-endings
    (push (concat       (regexp-opt ignoramus-file-basename-endings) "\\'")
          ignoramus-boring-file-regexp))
  (when ignoramus-file-basename-beginnings
    (push (concat "\\`" (regexp-opt ignoramus-file-basename-beginnings))
          ignoramus-boring-file-regexp))
  (when ignoramus-boring-file-regexp
    (setq ignoramus-boring-file-regexp (mapconcat 'identity ignoramus-boring-file-regexp "\\|")))

  (setq ignoramus-datafile-computed-basenames     (ignoramus--extract-strings ignoramus-datafile-basename))
  (setq ignoramus-datafile-computed-completepaths (ignoramus--extract-pathstrings ignoramus-datafile-completepath))
  (setq ignoramus-datafile-computed-prefixes      (ignoramus--extract-pathstrings ignoramus-datafile-prefix))
  (setq ignoramus-datafile-computed-dirprefixes   (mapcar 'ignoramus-ensure-trailing-slash
                                                          (ignoramus--extract-pathstrings ignoramus-datafile-dirprefix)))

  (setq ignoramus-datafile-computed-basenames-regexp       (regexp-opt ignoramus-datafile-computed-basenames))
  (setq ignoramus-datafile-computed-completepaths-regexp   (regexp-opt ignoramus-datafile-computed-completepaths))
  (setq ignoramus-datafile-computed-prefixes-regexp        (regexp-opt ignoramus-datafile-computed-prefixes))
  (setq ignoramus-datafile-computed-dirprefixes-regexp     (regexp-opt ignoramus-datafile-computed-dirprefixes)))


;;; configuration action plugins

;; Howto: Create a function named ignoramus-do-ignore-ACTION,
;;        and add ACTION to `ignoramus-known-actions'.


(defun ignoramus-do-ignore-vc ()
  "Tell `vc-mode' to ignore unwanted files."
  (setq vc-directory-exclusion-list ignoramus-file-basename-exact-names))


(defun ignoramus-do-ignore-grep ()
  "Tell `grep-mode' to ignore unwanted files."
  (setq grep-find-ignored-files (cons ".#*" (delq nil (mapcar #'(lambda (pat)
                                                                  (concat "*" pat)) ignoramus-file-basename-endings))))
  (setq grep-find-ignored-directories ignoramus-file-basename-exact-names))


(defun ignoramus-do-ignore-shell ()
  "Tell `shell-mode' to ignore unwanted files."
  (setq shell-completion-fignore ignoramus-file-basename-endings))


(defun ignoramus-do-ignore-comint ()
  "Tell `comint-mode' and derived modes to ignore unwanted files."
  (setq comint-completion-fignore ignoramus-file-basename-endings))


(defun ignoramus-do-ignore-completions ()
  "Tell built-in completions to ignore unwanted files."
  (setq completion-ignored-extensions (append
                                       ignoramus-file-basename-endings
                                       (mapcar (lambda (pat)
                                                 (concat pat "/")) ignoramus-file-basename-exact-names))))


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
                                       (regexp-opt (append '("." "..") ignoramus-file-basename-exact-names))
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
  (setq dired-omit-extensions ignoramus-file-basename-endings)
  (setq dired-omit-files ignoramus-boring-file-regexp))


(defun ignoramus-do-ignore-projectile ()
  "Tell `projectile-mode' to ignore unwanted files."

  ;; according to what projectile expects
  ;; (setq projectile-ignored-file-extensions (mapcar #'(lambda (ext)
  ;;                                                      (replace-regexp-in-string "\\`\\." "" ext))
  ;;                                                 ignoramus-file-basename-endings))
  ;; (setq projectile-ignored-files ignoramus-file-basename-exact-names)
  ;; (setq projectile-ignored-directories ignoramus-file-basename-exact-names)

  ;; a better test than what projectile does - note no longer just extensions
  (eval-after-load "projectile"
    '(progn
       (defun projectile-ignored-extension-p (file)
         (let ((case-fold-search ignoramus-case-insensitive))
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
(defun ignoramus-matches-datafile (file &optional file-basename expanded)
  "Return non-nil if FILE is used for data storage by a known Lisp library.

This function identifies specific files used for persistence by
tramp, semantic, woman, etc.

If a Lisp library is loaded after ignoramus, its files may not
be recognized, in which case `ignoramus-compute-common-regexps'
maybe called.

FILE-BASENAME may also be given as an optimization, in case the
caller has already computed the basename.

As an optimization, EXPANDED may be set to t to indicate that FILE
has already been expanded."
  (when (stringp file)
    (unless ignoramus-boring-file-regexp
      (ignoramus-compute-common-regexps))
    (when (file-remote-p file)
      (setq file (substring file (length (file-remote-p file))))
      (setq expanded nil))
    (unless expanded
      (setq file (expand-file-name file)))
    (unless file-basename
      (setq file-basename (file-name-nondirectory file)))
    (let ((case-fold-search ignoramus-case-insensitive))
      (catch 'known
        (when (string-match-p ignoramus-datafile-computed-basenames-regexp file-basename)
          (dolist (basename ignoramus-datafile-computed-basenames)
            (when (compare-strings basename nil nil file-basename nil nil ignoramus-case-insensitive)
              (throw 'known (list file 'basename basename file-basename)))))
        (when (string-match-p ignoramus-datafile-computed-completepaths-regexp file)
          (dolist (completepath ignoramus-datafile-computed-completepaths)
            (when (compare-strings completepath nil nil file nil nil ignoramus-case-insensitive)
              (throw 'known (list file 'completepath completepath file)))))
        (when (string-match-p ignoramus-datafile-computed-prefixes-regexp file)
          (dolist (prefix ignoramus-datafile-computed-prefixes)
            (when (string-prefix-p prefix file ignoramus-case-insensitive)
              (throw 'known (list file 'prefix (expand-file-name prefix) file)))))
        (when (string-match-p ignoramus-datafile-computed-dirprefixes-regexp file)
          (dolist (dirprefix ignoramus-datafile-computed-dirprefixes)
            (when (string-prefix-p dirprefix file ignoramus-case-insensitive)
              (throw 'known (list file 'dirprefix dirprefix file)))))))))

;;;###autoload
(defun ignoramus-register-datafile (symbol-or-string type &optional unregister)
  "Register a generated file used for data storage.

This generated file will be ignored by ignoramus.

SYMBOL-OR-STRING may be the name of a symbol to consult, or a
string.  If a symbol, it should refer to a string or list of
strings.

TYPE may be one of 'basename, 'completepath, 'prefix, or
'dirprefix.

Optional UNREGISTER tells ignoramus to forget about
SYMBOL-OR-STRING."
  (assert (memq type '(basename completepath prefix dirprefix)) nil "bad TYPE")
  (let ((sym (intern (format "ignoramus-datafile-%s" type))))
    (if unregister
        (set sym (delete symbol-or-string (symbol-value sym)))
      ;; else
      (push symbol-or-string (symbol-value sym))))
  (ignoramus-compute-common-regexps))

;;;###autoload
(defun ignoramus-boring-p (file &optional file-basename expanded)
  "Return non-nil if ignoramus thinks FILE is uninteresting.

FILE-BASENAME may also be given as an optimization, in case the
caller has already computed the basename.

As an optimization, EXPANDED may be set to t to indicate that FILE
has already been expanded."
  (unless ignoramus-boring-file-regexp
    (ignoramus-compute-common-regexps))
  (unless file-basename
    (setq file-basename (file-name-nondirectory file)))
  (let ((case-fold-search ignoramus-case-insensitive))
    (or (string-match-p ignoramus-boring-file-regexp file-basename)
        (and ignoramus-use-known-datafiles
             (ignoramus-matches-datafile file file-basename expanded)))))

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
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: Ignoramus ARGS alist Howto pathname dired ignorable
;; LocalWords: callf datafiles datafile completepath dirprefix
;;

;;; ignoramus.el ends here

;;; auto-install-batch-list.el --- Data file of auto-install.el
;; $Id: auto-install-batch-list.el,v 1.3 2010/05/01 01:32:55 rubikitch Exp rubikitch $

;;; This file is used by auto-install.el internally.  M-x
;;; auto-install-batch downloads this file before installation to
;;; adapt fileset change.  This file in EmacsWiki is only a copy for
;;; security reason.
;;;
;;; FEEL FREE TO MODIFY by M-x `auto-install-batch-edit'!!
;;; I'll update the original file.
;;;
;;; (progn (scp-archive) (emacswiki-post "auto-install-batch-list.el"))

(setq auto-install-batch-list-internal
      (append
       '(;; espresso.el
         ("espresso" nil nil
          ("http://download.savannah.gnu.org/releases-noredirect/espresso/json.el"
           "http://download.savannah.gnu.org/releases-noredirect/espresso/moz.el"
           "http://download.savannah.gnu.org/releases-noredirect/espresso/espresso.el"))
         ;; sql.el
         ("sql" nil nil
          ("https://www.emacswiki.org/emacs/download/sql.el"
           "https://www.emacswiki.org/emacs/download/sql-indent.el"
           "https://www.emacswiki.org/emacs/download/sql-transform.el"
           "https://www.emacswiki.org/emacs/download/sql-complete.el"))
         ;; mcomplete.el
         ("mcomplete" nil nil
          ("http://homepage1.nifty.com/bmonkey/emacs/elisp/mcomplete.el"
           "http://www.bookshelf.jp/elc/mcomplete-history.el"))
         ;; sr-speedbar.el
         ("sr-speedbar" nil nil
          ("https://www.emacswiki.org/emacs/download/sr-speedbar.el"
           "https://www.emacswiki.org/emacs/download/speedbar-extension.el"))
         ;; Icicles.
         ("icicles" 21 5
          (
           ;; Required ----------------------------------------
           "https://www.emacswiki.org/emacs/download/icicles.el" ; Main library
           "https://www.emacswiki.org/emacs/download/icicles-cmd1.el" ; Top-level Icicles commands, part 1
           "https://www.emacswiki.org/emacs/download/icicles-cmd2.el" ; Top-level Icicles commands, part 2
           "https://www.emacswiki.org/emacs/download/icicles-face.el" ; Faces
           "https://www.emacswiki.org/emacs/download/icicles-fn.el" ; Non-interactive functions
           "https://www.emacswiki.org/emacs/download/icicles-mac.el" ; Macros
           "https://www.emacswiki.org/emacs/download/icicles-mcmd.el" ; Minibuffer commands
           "https://www.emacswiki.org/emacs/download/icicles-mode.el" ; Icicle (Icy) mode
           "https://www.emacswiki.org/emacs/download/icicles-opt.el" ; User options
           "https://www.emacswiki.org/emacs/download/icicles-var.el" ; Internal variables

           ;; Optional, loaded by default ---------------------
           "https://www.emacswiki.org/emacs/download/apropos-fn+var.el" ; `apropos' enhancements
           "https://www.emacswiki.org/emacs/download/hexrgb.el" ; Color manipulation
           "https://www.emacswiki.org/emacs/download/icicles-chg.el" ; Change logs
           "https://www.emacswiki.org/emacs/download/icicles-doc1.el" ; Doc, part 1 (comments only)
           "https://www.emacswiki.org/emacs/download/icicles-doc2.el" ; Doc, part 2 (comments only)
           "https://www.emacswiki.org/emacs/download/lacarte.el" ; Menu-bar access from keyboard
           "https://www.emacswiki.org/emacs/download/mb-depth+.el" ; Extensions to `mb-depth.el'
           "https://www.emacswiki.org/emacs/download/thingatpt+.el" ; Extensions to `thingatpt.el'
           ;; These belong together:
           "https://www.emacswiki.org/emacs/download/crosshairs.el" ; Crosshairs highlighting
           "https://www.emacswiki.org/emacs/download/hl-line+.el" ; Extensions to `hl-line.el'
           "https://www.emacswiki.org/emacs/download/col-highlight.el" ; Column highlighting
           "https://www.emacswiki.org/emacs/download/vline.el" ; Column highlighting
           ;; These belong together:
           "https://www.emacswiki.org/emacs/download/doremi.el" ; Change *Completions* display incrementally
           "https://www.emacswiki.org/emacs/download/ring+.el" ; Extensions to `ring.el'
           ;; These belong together:
           "https://www.emacswiki.org/emacs/download/doremi-frm.el" ; Zoom *Completions*
           "https://www.emacswiki.org/emacs/download/faces+.el" ; Extensions to `faces.el'
           "https://www.emacswiki.org/emacs/download/frame-fns.el" ; Frame functions

           ;; Optional, not loaded by default -----------------
            ;; "https://www.emacswiki.org/emacs/download/bookmark+.el" ; Extensions to `bookmark.el'.  With:
            ;; "https://www.emacswiki.org/emacs/download/bookmark+-1.el"
            ;; "https://www.emacswiki.org/emacs/download/bookmark+-bmu.el"
            ;; "https://www.emacswiki.org/emacs/download/bookmark+-doc.el"
            ;; "https://www.emacswiki.org/emacs/download/bookmark+-key.el"
            ;; "https://www.emacswiki.org/emacs/download/bookmark+-lit.el"
            ;; "https://www.emacswiki.org/emacs/download/bookmark+-mac.el"
           ;; "https://www.emacswiki.org/emacs/download/dired+.el" ; Dired enhancements
           ;; "https://www.emacswiki.org/emacs/download/ffap-.el" ; Extensions to `ffap.el'
           ;; "https://www.emacswiki.org/emacs/download/fit-frame.el" ; Fit frames to their (sole) buffers
           ;; "https://www.emacswiki.org/emacs/download/frame-cmds.el" ; Frame commands
           ;; "https://www.emacswiki.org/emacs/download/fuzzy-match.el" ; Fuzzy matching
           ;; "https://www.emacswiki.org/emacs/download/icomplete+.el" ; Enhancements to `icomplete.el'
           ;; "https://www.emacswiki.org/emacs/download/info+.el" ; Extensions to `info.el'
           ;; "https://www.emacswiki.org/emacs/download/linkd.el" ; Provides hypertext links for Icicles doc
           ;; "https://www.emacswiki.org/emacs/download/menu-bar+.el" ; Extensions to `menu-bar.el'
           ;; "https://www.emacswiki.org/emacs/download/misc-cmds.el" ; Miscellaneous commands
           ;; "https://www.emacswiki.org/emacs/download/palette.el" ; Pick up foreground/background color
           ;; "https://www.emacswiki.org/emacs/download/pp+.el" ; Extensions to `pp.el'
           ;; "https://www.emacswiki.org/emacs/download/synonyms.el" ; Look up synonyms (thesaurus)
           ;; "https://www.emacswiki.org/emacs/download/wid-edit+.el" ; Extensions to `wid-edit.el'
           ))

         ;; AutoComplete development version.
         ("auto-complete development version" nil nil
          (
           "https://raw.githubusercontent.com/auto-complete/popup-el/master/popup.el"
           "https://raw.githubusercontent.com/auto-complete/fuzzy-el/master/fuzzy.el"
           "https://raw.githubusercontent.com/auto-complete/auto-complete/master/auto-complete.el"
           "https://raw.githubusercontent.com/auto-complete/auto-complete/master/auto-complete-config.el"
           ))
         ;; Anything
         ("anything-minimal" nil nil
          (
           "https://www.emacswiki.org/emacs/download/anything.el" ; Main library
           "https://www.emacswiki.org/emacs/download/anything-match-plugin.el" ; Matching algorithm humanely
           "https://www.emacswiki.org/emacs/download/anything-config.el" ; Configuration for anything.el
           ))
         ("anything-kyr" nil nil
          (
           "https://www.emacswiki.org/emacs/download/anything.el" ; Main library
           "https://www.emacswiki.org/emacs/download/anything-kyr.el" ; 
           ))
         ("anything-completion-minimal" nil nil
          (
           "anything-minimal"
           "https://www.emacswiki.org/emacs/download/anything-show-completion.el" ; Show completion prettily
           
           ))
         ("anything" 30 5
          (
           "anything-minimal"
           "https://www.emacswiki.org/emacs/download/anything-migemo.el" ; Migemo extension for Japanese
           "https://www.emacswiki.org/emacs/download/anything-obsolete.el" ; obsolete functions
           "https://www.emacswiki.org/emacs/download/anything-complete.el" ; Completion
           "https://www.emacswiki.org/emacs/download/anything-show-completion.el" ; Show completion prettily
           "https://www.emacswiki.org/emacs/download/anything-auto-install.el" ; auto-install extension
           "https://www.emacswiki.org/emacs/download/descbinds-anything.el" ; describe-key replacement
           "https://www.emacswiki.org/emacs/download/anything-grep.el" ; Grep with anything
           "https://www.emacswiki.org/emacs/download/anything-menu.el" ; menu selection outside emacs
           "https://www.emacswiki.org/emacs/download/anything-gtags.el" ; GNU GLOBAL interface
           "https://www.emacswiki.org/emacs/download/ipa.el" ; In-Place-Annotation
           "https://www.emacswiki.org/emacs/download/anything-ipa.el" ; In-Place-Annotation
           "https://www.emacswiki.org/emacs/download/anything-startup.el" ; Startup file
           ))
         ;; SDCV (Interface for StartDict console version)
         ("sdcv" nil nil
          (
           "https://www.emacswiki.org/emacs/download/showtip.el" ; Basic tooltip show library
           "https://www.emacswiki.org/emacs/download/sdcv.el" ; sdcv.el
           ))
         ;; Lazy search
         ("lazy-search" nil nil
          (
           "https://www.emacswiki.org/emacs/download/one-key.el" ; Basic library for lazy-search.el
           "https://www.emacswiki.org/emacs/download/lazy-search.el" ; Main library
           ))
         ;; PHP completion
         ("php-completion" nil nil
          (
           "anything-completion-minimal"
           "https://www.emacswiki.org/emacs/download/php-completion.el"
           ))
         ;; Perl completion
         ("perl-completion" nil nil
          (
           "anything-completion-minimal"
           "https://www.emacswiki.org/emacs/download/perl-completion.el"
           ))
         ;; Text Translator
         ("text translator" nil nil
          (
           "https://www.emacswiki.org/emacs/download/text-translator.el"
           "https://www.emacswiki.org/emacs/download/text-translator-vars.el"
           "https://www.emacswiki.org/emacs/download/text-translator-load.el"
           ))
         ("test-case-mode" nil nil
          (
           "https://nschum.de/src/emacs/fringe-helper/fringe-helper.el"
           "https://nschum.de/src/emacs/test-case-mode/test-case-mode.el"))
         ("el-expectations" nil nil
          (
           "https://www.emacswiki.org/emacs/download/ert-expectations.el"
           "https://www.emacswiki.org/emacs/download/el-mock.el"
           "https://www.emacswiki.org/emacs/download/el-expectations-success-sample.el"
           "https://www.emacswiki.org/emacs/download/el-expectations-failure-sample.el"))
         ("sequential-command" nil nil
          (
           "https://www.emacswiki.org/emacs/download/sequential-command.el"
           "https://www.emacswiki.org/emacs/download/sequential-command-config.el"))
         ("col-highlight" nil nil
          (
           "https://www.emacswiki.org/emacs/download/vline.el"
           "https://www.emacswiki.org/emacs/download/column-marker.el"
           "https://www.emacswiki.org/emacs/download/col-highlight.el"))
         ("crosshairs" nil nil
          (
           "col-highlight"
           "https://www.emacswiki.org/emacs/download/hl-line+.el"
           "https://www.emacswiki.org/emacs/download/crosshairs.el"))
         ("e2wm" nil nil
          (
           "https://github.com/kiwanami/emacs-window-layout/raw/master/window-layout.el"
           "https://github.com/kiwanami/emacs-window-manager/raw/master/e2wm.el"))
         ("helm" nil nil
          (
           "https://raw.github.com/emacs-helm/helm/master/helm.el"))
         )                              ;/appende
       auto-install-batch-list))        ;/setq

;; Local Variables:
;; no-byte-compile:   t
;; End:

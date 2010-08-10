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
          ("http://www.emacswiki.org/emacs/download/sql.el"
           "http://www.emacswiki.org/emacs/download/sql-indent.el"
           "http://www.emacswiki.org/emacs/download/sql-transform.el"
           "http://www.emacswiki.org/emacs/download/sql-complete.el"))
         ;; mcomplete.el
         ("mcomplete" nil nil
          ("http://homepage1.nifty.com/bmonkey/emacs/elisp/mcomplete.el"
           "http://www.bookshelf.jp/elc/mcomplete-history.el"))
         ;; sr-speedbar.el
         ("sr-speedbar" nil nil
          ("http://www.emacswiki.org/emacs/download/sr-speedbar.el"
           "http://www.emacswiki.org/emacs/download/speedbar-extension.el"))
         ;; Icicles.
         ("icicles" 21 10
          (
           "http://www.emacswiki.org/emacs/download/icicles.el" ; Main library
           "http://www.emacswiki.org/emacs/download/icicles-chg.el" ; Change logs
           "http://www.emacswiki.org/emacs/download/icicles-cmd1.el" ; Top-level Icicles commands, part 1
           "http://www.emacswiki.org/emacs/download/icicles-cmd2.el" ; Top-level Icicles commands, part 2
           "http://www.emacswiki.org/emacs/download/icicles-doc1.el" ; Doc, part 1
           "http://www.emacswiki.org/emacs/download/icicles-doc2.el" ; Doc, part 2
           "http://www.emacswiki.org/emacs/download/icicles-face.el" ; Faces
           "http://www.emacswiki.org/emacs/download/icicles-fn.el" ; Non-interactive functions
           "http://www.emacswiki.org/emacs/download/icicles-mac.el" ; Macros
           "http://www.emacswiki.org/emacs/download/icicles-mcmd.el" ; Minibuffer commands
           "http://www.emacswiki.org/emacs/download/icicles-mode.el" ; Icicle (Icy) mode
           "http://www.emacswiki.org/emacs/download/icicles-opt.el" ; User options
           "http://www.emacswiki.org/emacs/download/icicles-var.el" ; Internal variables
           "http://www.emacswiki.org/emacs/download/lacarte.el" ; Menu-bar access from keyboard
           "http://www.emacswiki.org/emacs/download/icomplete+.el" ; Enhancements to `icomplete.el'
           "http://www.emacswiki.org/emacs/download/hexrgb.el" ; Color manipulation
           "http://www.emacswiki.org/emacs/download/synonyms.el" ; Look up synonyms
           ))
         ;; AutoComplete development version.
         ("auto-complete development version" nil nil
          (
           "http://github.com/m2ym/auto-complete/raw/master/popup.el"
           "http://github.com/m2ym/auto-complete/raw/master/fuzzy.el"
           "http://github.com/m2ym/auto-complete/raw/master/auto-complete.el"
           "http://github.com/m2ym/auto-complete/raw/master/auto-complete-config.el"
           ))
         ;; Anything
         ("anything-minimal" nil nil
          (
           "http://www.emacswiki.org/emacs/download/anything.el" ; Main library
           "http://www.emacswiki.org/emacs/download/anything-config.el" ; Configuration for anything.el
           "http://www.emacswiki.org/emacs/download/anything-match-plugin.el" ; Matching algorithm humanely
           ))
         ("anything-completion-minimal" nil nil
          (
           "anything-minimal"
           "http://www.emacswiki.org/emacs/download/anything-show-completion.el" ; Show completion prettily
           
           ))
         ("anything" 21 10
          (
           "anything-minimal"
           "http://www.emacswiki.org/emacs/download/anything-migemo.el" ; Migemo extension for Japanese
           "http://www.emacswiki.org/emacs/download/anything-complete.el" ; Completion
           "http://www.emacswiki.org/emacs/download/anything-show-completion.el" ; Show completion prettily
           "http://www.emacswiki.org/emacs/download/anything-auto-install.el" ; auto-install extension
           "http://www.emacswiki.org/emacs/download/descbinds-anything.el" ; describe-key replacement
           "http://www.emacswiki.org/emacs/download/anything-grep.el" ; Grep with anything
           "http://www.emacswiki.org/emacs/download/anything-menu.el" ; menu selection outside emacs
;;           "http://www.emacswiki.org/emacs/download/anything-gtags.el" ; GNU GLOBAL interface
           "http://www.emacswiki.org/emacs/download/ipa.el" ; In-Place-Annotation
           "http://www.emacswiki.org/emacs/download/anything-ipa.el" ; In-Place-Annotation
           "http://www.emacswiki.org/emacs/download/anything-startup.el" ; Startup file
           ))
         ;; SDCV (Interface for StartDict console version)
         ("sdcv" nil nil
          (
           "http://www.emacswiki.org/emacs/download/showtip.el" ; Basic tooltip show library
           "http://www.emacswiki.org/emacs/download/sdcv.el" ; sdcv.el
           ))
         ;; Lazy search
         ("lazy-search" nil nil
          (
           "http://www.emacswiki.org/emacs/download/one-key.el" ; Basic library for lazy-search.el
           "http://www.emacswiki.org/emacs/download/lazy-search.el" ; Main library
           ))
         ;; PHP completion
         ("php-completion" nil nil
          (
           "anything-completion-minimal"
           "http://www.emacswiki.org/emacs/download/php-completion.el"
           ))
         ;; Perl completion
         ("perl-completion" nil nil
          (
           "anything-completion-minimal"
           "http://www.emacswiki.org/emacs/download/perl-completion.el"
           ))
         ;; Text Translator
         ("text translator" nil nil
          (
           "http://www.emacswiki.org/emacs/download/text-translator.el"
           "http://www.emacswiki.org/emacs/download/text-translator-vars.el"
           "http://www.emacswiki.org/emacs/download/text-translator-load.el"
           ))
         ("test-case-mode" nil nil
          (
           "http://nschum.de/src/emacs/fringe-helper/fringe-helper.el"
           "http://nschum.de/src/emacs/test-case-mode/test-case-mode.el"))
         ("el-expectations" nil nil
          (
           "http://www.emacswiki.org/emacs/download/el-expectations.el"
           "http://www.emacswiki.org/emacs/download/el-mock.el"
           "http://www.emacswiki.org/emacs/download/el-expectations-success-sample.el"
           "http://www.emacswiki.org/emacs/download/el-expectations-failure-sample.el"))
         ("sequential-command" nil nil
          (
           "http://www.emacswiki.org/emacs/download/sequential-command.el"
           "http://www.emacswiki.org/emacs/download/sequential-command-config.el"))
         ("col-highlight" nil nil
          (
           "http://www.emacswiki.org/emacs/download/vline.el"
           "http://www.emacswiki.org/emacs/download/column-marker.el"
           "http://www.emacswiki.org/emacs/download/col-highlight.el"))
         ("crosshairs" nil nil
          (
           "col-highlight"
           "http://www.emacswiki.org/emacs/download/hl-line+.el"
           "http://www.emacswiki.org/emacs/download/crosshairs.el"))
         ("e2wm" nil nil
          (
           "http://github.com/kiwanami/emacs-window-layout/raw/master/window-layout.el"
           "http://github.com/kiwanami/emacs-window-manager/raw/master/e2wm.el"))
         )                              ;/appende
       auto-install-batch-list))        ;/setq

;; Local Variables:
;; no-byte-compile:   t
;; End:

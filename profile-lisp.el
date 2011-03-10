;;;----------------------------------------------------------------------
;; lisp.el
;; Primary Author: Mike Mattie
;; Copyright: Mike Mattie 2009
;;----------------------------------------------------------------------

;; third party extensions lisp support.

;;----------------------------------------------------------------------
;; parentheses matching
;;----------------------------------------------------------------------

;; mic-paren fancy paren/delimited highlighting. It is particularly
;;           valuable for reverse highlighting regions.

(grail-load 'mic-paren (grail-define-installer "mic-paren"
                         "file"
                         "http://www.emacswiki.org/cgi-bin/emacs/download/mic-paren.el"))

(grail-load 'quack     (grail-define-installer "quack"
                         "file"
                         "http://www.neilvandyke.org/quack/quack.el"))

(grail-load 'slime     (grail-define-installer "slime"
                         "cvs"
                         ":pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot"))

(setq
  paren-showing t
  show-paren-style 'parenthesis
  show-paren-delay 1
  paren-sexp-mode 'match)

(grail-set-faces
  (paren-face-match (background "grey20")))

(paren-activate)

;;----------------------------------------------------------------------
;; scheme
;;----------------------------------------------------------------------

(setq-default
  quack-dir (grail-garuntee-dir-path (concat grail-state-path "quack/"))
  quack-default-program default-scheme-interpreter

  ;; don't always prompt for the scheme interpreter. Use the default.
  quack-run-scheme-always-prompts-p nil

  ;; don't use customize to save settings.
  quack-options-persist-p nil

  ;; fontify using Emacs faces, don't use a drscheme theme clone.
  quack-fontify-style 'emacs

  ;; tabs are evil
  quack-tabs-are-evil-p t)

;; quack adds all the typical extensions when loaded, so only add non-standard
;; ones.
(setq
  auto-mode-alist (append '(("\\.scheme$"    . scheme-mode)) auto-mode-alist ))

;;----------------------------------------------------------------------
;; common lisp
;;----------------------------------------------------------------------

(defconst slime-project-page "http://common-lisp.net/project/slime/"
  "the SLIME project page")

(setq inferior-lisp-program "sbcl")
(slime-setup)

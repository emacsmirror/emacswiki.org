;;; yang-mode.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Note: The interface used in this file requires CC Mode 5.30 or
;; later.

;; Version:
;;   00.7 - 2016-03-15
;;        draft-ietf-netmod-rfc6020bis-11 compliant
;;        added support for new 1.1 keywords
;;   00.6 - 2012-02-01
;;        removed unused defcustom yang-font-lock-extra-types
;;          made emacs24 to give a warning
;;   00.5 - 2010-10-07
;;        rfc6020 compliant
;;        classify all keywords as decl-start gives better indentation
;;   00.4 - 2010-04-30
;;        draft-ietf-netmod-yang-12 compliant,
;;        added instructions for emacs 23
;;   00.3 - 2009-12-19
;;        draft-ietf-netmod-yang-09 compliant,
;;   00.2 - 2008-11-04
;;        draft-ietf-netmod-yang-02 compliant.
;;   00.1 - 2007-11-14
;;        Initial version, draft-bjorklund-netconf-yang-00 compliant.

;; Useful tips:
;;
;;   For use with emacs 23, put this in your .emacs:
;;     (autoload 'yang-mode "yang-mode" "Major mode for editing YANG modules."
;;               t)
;;     (add-to-list 'auto-mode-alist '("\\.yang$" . yang-mode))
;;
;;   Some users have reported other errors with emacs 23, and have found
;;   that removing the byte-compiled cc-mode.elc file fixes these problems.
;;   (e.g. /usr/share/emacs/23.1/lisp/progmodes/cc-mode.elc)
;;
;;   For use with emacs 22, just use:
;;     (require 'yang-mode nil t)
;;
;;   For editing somewhat larger YANG modules, add this to your .emacs
;;     (setq blink-matching-paren-distance nil)
;;
;;   Common YANG layout:
;;     (defun my-yang-mode-hook ()
;;       "Configuration for YANG Mode. Add this to `yang-mode-hook'."
;;       (if window-system
;;         (progn
;;           (c-set-style "BSD")
;;           (setq indent-tabs-mode nil)
;;           (setq c-basic-offset 2)
;;           (setq font-lock-maximum-decoration t)
;;           (font-lock-mode t))))
;;
;;     (add-hook 'yang-mode-hook 'my-yang-mode-hook)
;;
;;   Use the oultine minor mode for YANG.
;;   This is very useful to get a good overview of the structure of
;;   a module.
;;
;;   Put this in your .emacs:
;;
;;     (defun show-onelevel ()
;;       "show entry and children in outline mode"
;;       (interactive)
;;       (show-entry)
;;       (show-children))
;;
;;     (defun my-outline-bindings ()
;;       "sets shortcut bindings for outline minor mode"
;;       (interactive)
;;       (local-set-key [?\C-,] 'hide-body)
;;       (local-set-key [?\C-.] 'show-all)
;;       (local-set-key [C-up] 'outline-previous-visible-heading)
;;       (local-set-key [C-down] 'outline-next-visible-heading)
;;       (local-set-key [C-left] 'hide-subtree)
;;       (local-set-key [C-right] 'show-onelevel)
;;       (local-set-key [M-up] 'outline-backward-same-level)
;;       (local-set-key [M-down] 'outline-forward-same-level)
;;       (local-set-key [M-left] 'hide-subtree)
;;       (local-set-key [M-right] 'show-subtree))
;;
;;     (add-hook
;;      'outline-minor-mode-hook
;;      'my-outline-bindings)
;;
;;     (defconst sort-of-yang-identifier-regexp "[-a-zA-Z0-9_\\.:]*")
;;
;;     (add-hook
;;      'yang-mode-hook
;;      '(lambda ()
;;         (outline-minor-mode)
;;         (setq outline-regexp
;;           (concat "^ *" sort-of-yang-identifier-regexp " *"
;;                   sort-of-yang-identifier-regexp
;;                   " *{"))))

;;; Code:

(require 'cc-mode)

;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(eval-and-compile
  ;; Make our mode known to the language constant system.  Use Java
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'yang-mode 'java-mode))

;; YANG has no primitive types in the C/Java sense.
(c-lang-defconst c-primitive-type-kwds
  yang '())

(c-lang-defconst c-modifier-kwds
  yang '())

(c-lang-defconst c-label-kwds
  yang '())

(c-lang-defconst c-before-label-kwds
  yang '())

(c-lang-defconst c-class-decl-kwds
  yang '())

(c-lang-defconst c-decl-start-kwds
  yang '(
         "action"
         "anydata"
         "anyxml"
         "argument"
         "augment"
         "base"
         "belongs-to"
         "bit"
         "case"
         "choice"
         "config"
         "contact"
         "container"
         "default"
         "description"
         "deviate"
         "deviation"
         "enum"
         "error-app-tag"
         "error-message"
         "extension"
         "feature"
         "fraction-digits"
         "grouping"
         "identity"
         "if-feature"
         "import"
         "include"
         "input"
         "key"
         "leaf"
         "leaf-list"
         "length"
         "list"
         "mandatory"
         "max-elements"
         "min-elements"
         "modifier"
         "module"
         "must"
         "namespace"
         "notification"
         "ordered-by"
         "organization"
         "output"
         "path"
         "pattern"
         "position"
         "prefix"
         "presence"
         "range"
         "reference"
         "refine"
         "require-instance"
         "revision"
         "revision-date"
         "rpc"
         "status"
         "submodule"
         "type"
         "typedef"
         "unique"
         "units"
         "uses"
         "value"
         "when"
         "yang-version"
         "yin-element"
         ))

;; No cpp in this language. (The definitions for the extra keywords
;; above are enough to incorporate them into the fontification regexps
;; for types and keywords, so no additional font-lock patterns are
;; required.)
(c-lang-defconst c-cpp-matchers
  yang
      ;; There are some other things in `c-cpp-matchers' besides the
      ;; preprocessor support, so include it.
      (c-lang-const c-cpp-matchers))

;; '-' is part of an identifier in YANG
;; FIXME: how do I make '.' part of the identifier?
(c-lang-defconst c-identifier-syntax-modifications
  yang (append '((?- . "w") (?: . "w"))
               (c-lang-const c-identifier-syntax-modifications)))

(c-lang-defconst c-symbol-chars
  yang (concat c-alnum ":_-"))

;; YANG does not have labels
(c-lang-defconst c-recognize-colon-labels
  yang nil)

(defconst yang-font-lock-keywords-1 (c-lang-const c-matchers-1 yang)
  "Minimal highlighting for YANG mode.")

(defconst yang-font-lock-keywords-2 (c-lang-const c-matchers-2 yang)
  "Fast normal highlighting for YANG mode.")

(defconst yang-font-lock-keywords-3 (c-lang-const c-matchers-3 yang)
  "Accurate normal highlighting for YANG mode.")

(defvar yang-font-lock-keywords yang-font-lock-keywords-3
  "Default expressions to highlight in YANG mode.")

(defvar yang-mode-syntax-table nil
  "Syntax table used in yang-mode buffers.")
(or yang-mode-syntax-table
    (setq yang-mode-syntax-table
          (funcall (c-lang-const c-make-mode-syntax-table yang))))

(defvar yang-mode-abbrev-table nil
  "Abbreviation table used in yang-mode buffers.")
(c-define-abbrev-table 'yang-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the
  ;; syntactic context, and which therefore should trig reindentation
  ;; when they are completed.
  '())

(defvar yang-mode-map (let ((map (c-make-inherited-keymap)))
                      ;; Add bindings which are only useful for YANG
                      map)
  "Keymap used in yang-mode buffers.")

(easy-menu-define yang-menu yang-mode-map "YANG Mode Commands"
                  ;; Can use `yang' as the language for `c-mode-menu'
                  ;; since its definition covers any language.  In
                  ;; this case the language is used to adapt to the
                  ;; nonexistence of a cpp pass and thus removing some
                  ;; irrelevant menu alternatives.
                  (cons "YANG" (c-lang-const c-mode-menu yang)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.yang\\'" . yang-mode))

;;;###autoload
(defun yang-mode ()
  "Major mode for editing YANG modules.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `yang-mode-hook'.

Key bindings:
\\{yang-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table yang-mode-syntax-table)
  (setq major-mode 'yang-mode
        mode-name "YANG"
        local-abbrev-table yang-mode-abbrev-table
        abbrev-mode t)
  (use-local-map c-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars yang-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'yang-mode)
  ;; Allow auto-fill in strings
  (setq c-ignore-auto-fill '(cpp code))
  (easy-menu-add yang-menu)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'yang-mode-hook)
  (c-update-modeline))

(provide 'yang-mode)

;;; yang.el ends here

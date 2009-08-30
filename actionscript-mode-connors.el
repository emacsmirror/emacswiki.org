;;; actionscript-mode.el --- actionscript mode derived from cc-mode

;; Author:     2007 John Connors
;; Maintainer: John Connors <johnc at yagc dot co dot uk>
;; Created:    March 2007
;; Version:    See cc-mode.el
;; Keywords:   actionscript cc-mode languages oop

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
  (c-add-language 'actionscript-mode 'java-mode))

; Constants
(c-lang-defconst c-constant-kwds 
  actionscript
  '("true" "false" "null" "undefined" "NaN" "Infinity" "-Infinity"))

;; actionscript has no boolean but a string and a vector type.
(c-lang-defconst c-primitive-type-kwds
  actionscript 
   '("Boolean" "int" "Null" "Number" "String" "uint" "void" 
     "Object" "Array" "Date" "Error" "Function" "RegExp" "XML" "XMLList" ))

;; Keywords introducing other declaration-level constructs.
(c-lang-defconst c-other-decl-kwds
	actionscript
    '("import"))

;; Statement keywords followed directly by a block.
(c-lang-defconst c-block-stmt-1-kwds
  actionscript
  '("do" "else" "finally" "try"))

(c-lang-defconst c-block-stmt-2-kwds
  actionscript
  '("for" "if" "while" "switch" "catch"))
;;; 
;; Statement keywords followed by an expression or nothing.
(c-lang-defconst c-simple-stmt-kwds
  actionscript
  '("break" "continue" "return" "throw"))

(c-lang-defconst c-class-decl-kwds
  actionscript
  '("class" "interface"))

(c-lang-defconst c-opt-cpp-prefix
  actionscript
  "\\s *\\[\\s *")

(c-lang-defconst c-opt-ccp-include-directives
  actionscript
  '("embed"))

;; Function declarations begin with "function" in this language.
;; There's currently no special keyword list for that in CC Mode, but
;; treating it as a modifier works fairly well.
(c-lang-defconst c-modifier-kwds
  actionscript 
 '("function" "dynamic" "public" "private" "final" "override" "native"))


(c-lang-defconst c-other-block-decl-kwds
  actionscript
  '("package"))

(c-lang-defconst c-typeless-decl-kwds
  actionscript
  '("var"))

(c-lang-defconst c-primary-expr-kwds
  actionscript
  '("super"))

(c-lang-defconst c-other-kwds
  actionscript
  '("delete" "get" "set" "with"))

(defgroup actionscript nil
  "Major mode for editing ActionScript code."
  :group 'languages
  :prefix "actionscript-")

(defcustom actionscript-mode-hook nil
  "Hook for customizing `actionscript-mode'."
  :group 'actionscript
  :type 'hook)

(defcustom actionscript-font-lock-extra-types nil
  "*List of extra types (aside from the type keywords) to recognize in Actionscript mode.
Each list item should be a regexp matching a single identifier.")

(defconst actionscript-font-lock-keywords-1 
  (c-lang-const c-matchers-1 actionscript)
  "Minimal highlighting for ACTIONSCRIPT mode.")

(defconst actionscript-font-lock-keywords-2 
  (c-lang-const c-matchers-2 actionscript)
  "Fast normal highlighting for ACTIONSCRIPT mode.")

(defconst actionscript-font-lock-keywords-3 
  (c-lang-const c-matchers-3 actionscript)
  "Accurate normal highlighting for ACTIONSCRIPT mode.")

(defvar actionscript-font-lock-keywords actionscript-font-lock-keywords-3
  "Default expressions to highlight in ACTIONSCRIPT mode.")

(defvar actionscript-mode-syntax-table nil
  "Syntax table used in actionscript-mode buffers.")
(or actionscript-mode-syntax-table
    (setq actionscript-mode-syntax-table
      (funcall (c-lang-const c-make-mode-syntax-table actionscript))))

(defvar actionscript-mode-abbrev-table nil
  "Abbreviation table used in actionscript-mode buffers.")

(c-define-abbrev-table 'actionscript-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the
  ;; syntactic context, and which therefore should trig reindentation
  ;; when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)))

(defvar actionscript-mode-map (let ((map (c-make-inherited-keymap)))
              ;; Add bindings which are only useful for ACTIONSCRIPT
              map)
  "Keymap used in actionscript-mode buffers.")

(easy-menu-define actionscript-menu actionscript-mode-map "ACTIONSCRIPT Mode Commands"
          ;; Can use `actionscript' as the language for `c-mode-menu'
          ;; since its definition covers any language.  In
          ;; this case the language is used to adapt to the
          ;; nonexistence of a cpp pass and thus removing some
          ;; irrelevant menu alternatives.
          (cons "ACTIONSCRIPT" (c-lang-const c-mode-menu actionscript)))

;;;###Autoload
(add-to-list 'auto-mode-alist '("\\.as\\'" . actionscript-mode))

;;;###autoload
(defun actionscript-mode ()
  "Major mode for editing ACTIONSCRIPT Actionscript is an 
EcmaScript variant for coding for the Flash Player Runtime
by Adobe.
 
The hook `c-mode-common-hook' is run with no args at mode
initialization, then `actionscript-mode-hook'.

Key bindings:
\\{actionscript-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table actionscript-mode-syntax-table)
  (setq major-mode 'actionscript-mode
    mode-name "Actionscript"
    local-abbrev-table actionscript-mode-abbrev-table
    abbrev-mode t)
  (use-local-map c-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars actionscript-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'actionscript-mode)
  (easy-menu-add actionscript-menu)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'actionscript-mode-hook)
  (c-update-modeline))

 
(provide 'actionscript-mode)

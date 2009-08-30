;;; as3-mode.el --- A simple mode for editing Actionscript 3 files
 
;; Copyright (C) 2008 Austin Haas
 
;; Author: Austin Haas
;; Keywords: language modes

;; Tabbing stolen from: Aemon Cannon, http://github.com/aemoncannon/as3-mode/tree/master/as3-mode.el
;; Copyright (C) 2007 Aemon Cannon

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
 
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 
;;; Commentary:
;;
;;------------------
;;; TODO

;; Imenu (imenu-generic-expression or imenu-create-index-function)

;;------------------

;;; Code:

(require 'font-lock)
(eval-when-compile
  (require 'regexp-opt))

(defconst actionscript-mode-version "7.0"
  "Actionscript Mode version number.")

(defgroup actionscript nil
  "Major mode for editing Actionscript code."
  :group 'languages)

(defvar actionscript-mode-syntax-table nil
  "Syntax table used in actionscript-mode buffers.")

(if actionscript-mode-syntax-table
		()
	(setq actionscript-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_  "w"     actionscript-mode-syntax-table)
;;  (modify-syntax-entry ?_  "_"     actionscript-mode-syntax-table)
  (modify-syntax-entry ?$  "w"     actionscript-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\"    actionscript-mode-syntax-table)
  (modify-syntax-entry ?+  "."     actionscript-mode-syntax-table)
  (modify-syntax-entry ?-  "."     actionscript-mode-syntax-table)
  (modify-syntax-entry ?=  "."     actionscript-mode-syntax-table)
  (modify-syntax-entry ?%  "."     actionscript-mode-syntax-table)
  (modify-syntax-entry ?<  "."     actionscript-mode-syntax-table)
  (modify-syntax-entry ?>  "."     actionscript-mode-syntax-table)
  (modify-syntax-entry ?&  "."     actionscript-mode-syntax-table)
  (modify-syntax-entry ?|  "."     actionscript-mode-syntax-table)
  (modify-syntax-entry ?\' "\""    actionscript-mode-syntax-table)
  (modify-syntax-entry ?\240 "."   actionscript-mode-syntax-table)
	(modify-syntax-entry ?/  ". 124b" actionscript-mode-syntax-table)
	(modify-syntax-entry ?*  ". 23"   actionscript-mode-syntax-table)
  (modify-syntax-entry ?\n "> b"  actionscript-mode-syntax-table)
  (modify-syntax-entry ?\^m "> b" actionscript-mode-syntax-table))

(defvar actionscript-mode-map ()
  "Keymap used in actionscript-mode buffers.")

(if actionscript-mode-map
		()
	(setq actionscript-mode-map (make-sparse-keymap))
	(define-key actionscript-mode-map [(control meta a)] 'as-beginning-of-defun)
	(define-key actionscript-mode-map [(control meta e)] 'as-end-of-defun)
	(define-key actionscript-mode-map [(control meta h)] 'as-mark-defun)
	(define-key actionscript-mode-map "\C-c\C-c" 'comment-region)
	(define-key actionscript-mode-map "\C-c\C-u" 'uncomment-region))

(defcustom actionscript-mode-hook nil
  "*Hook called by `actionscript-mode'."
  :type 'hook
  :group 'actionscript)

;; Preprocessor directives (for cpp, not Actionscript).
(defconst preprocessor-kwds
	'("#include" "#define" "#else" "#endif" "#ifdef" "#ifndef"))

;; Constants
(defconst actionscript-constant-kwds 
	'("true" "false" "null" "undefined" "NaN" "Infinity" "-Infinity"))

;; Global funcs
(defconst actionscript-global-funcs
  '("Array" "Boolean" "decodeURI" "decodeURIComponent" "encodeURI" 
		"encodeURIComponent" "escape" "int" "isFinite" "isNaN" "isXMLName"
		"Number" "Object"	"parseFloat" "parseInt"	"String" "trace" "uint" 
		"unescape" "XML" "XMLList"))

;; Top Level Classes
(defconst actionscript-global-classes
	'("ArgumentError" "arguments" "Array" "Boolean" "Class" "Date"
		"DefinitionError" "Error" "EvalError" "Function" "int" "Math"
		"Namespace" "Number" "Object" "QName" "RangeError" "ReferenceError"
		"RegExp" "SecurityError" "String" "SyntaxError" "TypeError" "uint"
		"URIError" "VerifyError" "XML" "XMLList"))

;; Global props
(defconst actionscript-global-props
	'("this"))

;; Operators
(defconst actionscript-symbol-operators
	'("+" "+=" "[]" "=" "&" "&=" "<<" "<<=" 
		"~" "|" "|=" ">>" ">>=" ">>>" ">>>=" 
		"^" "^=" "/*" "*/" "," "?:" "--" "/" 
		"/=" "." "==" ">" ">=" "++" "!=" "<>" 
		"<" "<=" "//" "&&" "!" "||" "%" "%=" 
		"*" "*=" "{}" "()" "===" "!==" "\"" 
		"-" "-=" ":"))

(defconst actionscript-word-operators
	'("as" "is" "instanceof" "new" "typeof" "void"))

;; Declaration specifier keywords.
(defconst actionscript-specifier-kwds
  '("override" "instrinsic" "private" "protected" "public" "static" "dynamic"))

;; Class/struct declaration keywords.
(defconst actionscript-class-kwds 
	'("class" "interface"))

(defconst actionscript-package-kwds
	'("package"))

;; Keywords introducing other declaration-level constructs.
(defconst actionscript-other-decl-kwds
	'("import"))

;; Variable and Function declarations
(defconst actionscript-other-decl-2-kwds
	'("var" "function" "const"))

;; Keywords that occur in declaration-level constructs.
(defconst actionscript-decl-level-kwds 
	'("extends" "implements"))

;; Conditionals
;; *Not sure if "catch" should be here,
;; but it's assumed to be here for the actionscript-conditional-key.
(defconst actionscript-conditional-kwds
	'("for" "for each" "if" "while" "switch" "catch"))

;; Statement keywords followed directly by a block.
(defconst actionscript-block-stmt-1-kwds
  '("do" "else" "finally" "try"))

;; Statement keywords followed by an expression or nothing.
(defconst actionscript-simple-stmt-kwds
  '("break" "continue" "return" "throw"))

;; Keywords introducing labels in blocks.
(defconst actionscript-label-kwds 
	'("case" "default"))

;; Keywords that can occur anywhere in expressions.
(defconst actionscript-expr-kwds 
	'("super"))

;; Other keywords that we haven't grouped properly.
(defconst actionscript-other-kwds
	'("delete" "get" "set" "with"))

;;  (defconst actionscript-builtin-props
;; 	(regexp-opt
;; 	 (append actionscript-object-props
;; 					 actionscript-function-props
;; 					 actionscript-array-props
;; 					 actionscript-string-props
;; 					 actionscript-date-props
;; 					 actionscript-number-props
;; 					 actionscript-math-props)))

;; (defconst actionscript-builtin-funcs(regexp-opt
;; 	 (append actionscript-global-funcs
;; 					 actionscript-math-funcs
;; 					 actionscript-array-funcs
;; 					 actionscript-date-funcs)))

(defconst actionscript-keywords
	(regexp-opt
	 (append actionscript-constant-kwds
					 actionscript-global-funcs
					 actionscript-global-classes
					 actionscript-global-props
					 actionscript-specifier-kwds
					 actionscript-class-kwds
					 actionscript-package-kwds
					 actionscript-other-decl-kwds
					 actionscript-other-decl-2-kwds
					 actionscript-decl-level-kwds
					 actionscript-conditional-kwds
					 actionscript-block-stmt-1-kwds
					 actionscript-simple-stmt-kwds
					 actionscript-label-kwds
					 actionscript-expr-kwds
					 actionscript-other-kwds) 'words))

(defconst actionscript-identifier-re "[a-zA-Z_$][a-zA-Z0-9_$]*"
	"Regexp to match any valid identifier in actionscript.")

(defcustom actionscript-mode-hook nil
  "*Hook called by `actionscript-mode'."
  :type 'hook
  :group 'actionscript)

(defcustom actionscript-font-lock-level 2
	"*What level of syntax highlighting do we want. 1-3"
	:type '(radio (const :tag "Only keywords." 1)
					(const :tag "Keywords and contextual tags." 2)
					(const :tag "All of the above plus all of Actionscript's builtin classes. (not implemented)" 3))
	:group 'actionscript)

;;;; Faces -------------------------------------------------------------------

(defvar actionscript-font-lock-default-face 'actionscript-font-lock-default-face)

(let ((red "#a35757")
			(green "#7ac470")
			(yellow "#dfe14e")
			(orange "#ef6d22")
			(blue "#5083b2")
			(magenta "#b781ac")
			(cyan "#b0b5d2")
			(white "#f0f0f0"))

	(defface actionscript-preprocessor-kwds-face 
			`((t (:foreground ,yellow)))
		"*Face for preprocesor directives."
		:group 'actionscript-faces :group 'faces)

	(defface actionscript-constant-kwds-face 
			`((t (:foreground ,cyan)))
		"*"
		:group 'actionscript-faces :group 'faces)

	(defface actionscript-global-funcs-face 
			`((t (:foreground ,red)))
		"*"
		:group 'actionscript-faces :group 'faces)

	(defface actionscript-global-classes
			`((t (:foreground ,blue)))
		"*"
		:group 'actionscript-faces :group 'faces)

	(defface actionscript-global-props-face 
			`((t (:foreground ,blue)))
		"*"
		:group 'actionscript-faces :group 'faces)

	(defface actionscript-operators-face 
			`((t (:foreground ,yellow)))
		"*"
		:group 'actionscript-faces :group 'faces)

	(defface actionscript-specifier-kwds-face 
			`((t (:foreground ,magenta)))
		"*"
		:group 'actionscript-faces :group 'faces)

	(defface actionscript-package-kwds-face 
			`((t (:foreground ,yellow)))
		"*"
		:group 'actionscript-faces :group 'faces)

	(defface actionscript-class-kwds-face 
			`((t (:foreground ,yellow)))
		"*"
		:group 'actionscript-faces :group 'faces)

	(defface actionscript-other-decl-kwds-face 
			`((t (:foreground ,yellow)))
		"*"
		:group 'actionscript-faces :group 'faces)

	(defface actionscript-other-decl-2-kwds-face 
			`((t (:foreground ,blue)))
		"* function, var"
		:group 'actionscript-faces :group 'faces)

	(defface actionscript-decl-level-kwds-face 
			`((t (:foreground ,yellow)))
		"*"
		:group 'actionscript-faces :group 'faces)

	(defface actionscript-conditional-kwds-face
			`((t (:foreground ,yellow)))
		"*"
		:group 'actionscript-faces :group 'faces)

	(defface actionscript-block-stmt-1-kwds-face
			`((t (:foreground ,yellow)))
		"*"
		:group 'actionscript-faces :group 'faces)

	(defface actionscript-simple-stmt-kwds-face
			`((t (:foreground ,yellow)))
		"*"
		:group 'actionscript-faces :group 'faces)

	(defface actionscript-label-kwds-face
			`((t (:foreground ,yellow)))
		"*"
		:group 'actionscript-faces :group 'faces)

	(defface actionscript-expr-kwds-face
			`((t (:foreground ,red)))
		"*"
		:group 'actionscript-faces :group 'faces)

	(defface actionscript-other-kwds-face
			`((t (:foreground ,red)))
		"*"
		:group 'actionscript-faces :group 'faces)

	(defface actionscript-package-name-face
			`((t (:foreground ,green)))
		"*"
		:group 'actionscript-faces :group 'faces)

	(defface actionscript-class-name-face
			`((t (:foreground ,cyan)))
		"*"
		:group 'actionscript-faces :group 'faces)

	(defface actionscript-function-name-face
			`((t (:foreground ,green)))
		"*"
		:group 'actionscript-faces :group 'faces)

	(defface actionscript-variable-name-face
			`((t (:foreground ,cyan)))
		"*"
		:group 'actionscript-faces :group 'faces)
	)

(defconst actionscript-font-lock-keywords-1
	 ;; The following only highlight specific words that exist in the language.
   (list
		`(,(regexp-opt preprocessor-kwds 'words) 0 'actionscript-preprocessor-kwds-face)
		`(,(regexp-opt actionscript-constant-kwds 'words) 0 'actionscript-constant-kwds-face)
		`(,(regexp-opt actionscript-global-funcs 'words) 0 'actionscript-global-funcs-face)
		`(,(regexp-opt actionscript-global-props 'words) 0 'actionscript-global-props-face)
;;		`(,(regexp-opt actionscript-symbol-operators) 0 'actionscript-operators-face)
		`(,(regexp-opt actionscript-word-operators 'words) 0 'actionscript-operators-face)
		`(,(regexp-opt actionscript-specifier-kwds 'words) 0 'actionscript-specifier-kwds-face)
		`(,(regexp-opt actionscript-class-kwds 'words) 0 'actionscript-class-kwds-face)
		`(,(regexp-opt actionscript-package-kwds 'words) 0 'actionscript-package-kwds-face)
		`(,(regexp-opt actionscript-other-decl-kwds 'words) 0 'actionscript-other-decl-kwds-face)
		`(,(regexp-opt actionscript-other-decl-2-kwds 'words) 0 'actionscript-other-decl-2-kwds-face)
		`(,(regexp-opt actionscript-decl-level-kwds 'words) 0 'actionscript-decl-level-kwds-face)
		`(,(regexp-opt actionscript-conditional-kwds 'words) 0 'actionscript-conditional-kwds-face)
		`(,(regexp-opt actionscript-block-stmt-1-kwds 'words) 0 'actionscript-block-stmt-1-kwds-face)
		`(,(regexp-opt actionscript-simple-stmt-kwds 'words) 0 'actionscript-simple-stmt-kwds-face)
		`(,(regexp-opt actionscript-label-kwds 'words) 0 'actionscript-label-kwds-face)
		`(,(regexp-opt actionscript-expr-kwds 'words) 0 'actionscript-expr-kwds-face)
		`(,(regexp-opt actionscript-other-kwds 'words) 0 'actionscript-other-kwds-face))
	"Subdued level highlighting for Actionscript mode.")

(defconst actionscript-font-lock-keywords-2
  (append
   actionscript-font-lock-keywords-1
	 ;;;; The rules in this section highlight words in the buffer by determining their context.
	 (list 
		;; Fontify package names in import directives.
		;; TODO: support '*' as the last symbol in the package name.
		(list (concat (regexp-opt actionscript-other-decl-kwds 'words) "[ \t]*\\(?:" actionscript-identifier-re "\\.\\)*\\(" actionscript-identifier-re "\\)?")
					'(2 'actionscript-class-name-face nil t)
					(list (concat "[ \t]*\\(" actionscript-identifier-re "\\)\\.") '(goto-char (match-end 1)) '(goto-char (match-end 0)) '(1 'actionscript-package-name-face nil t)))

		;; Fontify package names.
		(list (concat (regexp-opt (append actionscript-package-kwds) 'words) "[ \t]*\\(" actionscript-identifier-re "\\)?")
					'(2 'actionscript-package-name-face nil t))

		;; Fontify class names.
		(list (concat (regexp-opt (append actionscript-class-kwds actionscript-decl-level-kwds) 'words) "[ \t]*\\(" actionscript-identifier-re "\\)?")
					'(2 'actionscript-class-name-face nil t))

		;; Function names.
		(list (concat "\\<function\\>[ \t\n]+\\(?:\\(?:get\\|set\\)[ \t\n]+\\)?\\(?:\\(" actionscript-identifier-re "\\)\\)?")
					'(1 'actionscript-function-name-face nil t))

		;; The 'in' in 'for..in.'
		(list (concat "\\<for\\>[ \t\n]*([ \t\n]*\\(?:var[ \t\n]+\\)?" actionscript-identifier-re "[ \t\n]*\\(?::[ \t\n]*\\([a-zA-Z0-9_$*]*\\)\\)?[ \t\n]+\\(in\\)[ \t\n]+")
					'(2 'actionscript-other-kwds-face nil t))

		;; The 'each' and the 'in' in 'for each..in.'
;; 		(list (concat "\\<for\\>[ \t\n]+\\(?:\\(each\\)[ \t\n]*\\)([ \t\n]*\\(?:var[ \t\n]+\\)?" actionscript-identifier-re "[ \t\n]*\\(?::[ \t\n]*\\([a-zA-Z0-9_$*]*\\)\\)?[ \t\n]+\\(in\\)[ \t\n]+")
;; 					'(1 'actionscript-other-kwds-face nil t)
;; 					'(3 'actionscript-other-kwds-face nil t))

		;; Local variables.
		(list (concat "\\<var\\>"
									"\\([ \t]*"
									actionscript-identifier-re
									"\\)")
					;; Fontify each declaration item.
					'(font-lock-match-c-style-declaration-item-and-skip-to-next
						;; Start and finish with point after the type specifier.
						(goto-char (match-beginning 1))
						(goto-char (match-beginning 1))
						(1 'actionscript-variable-name-face)))

		;; Objects and their functions
		;; package(s) class property
		;; TODO: We'd like to be able to distinguish true class names from a variable, so
		;;       that String.parse() would look different than mystring.parse().
		;;      -We might also want to distinguish local function calls, like parse().
;; 		(list (concat "\\<" "\\(?:[A-Za-z_]\\sw*\\.\\)*" "\\(?:[A-Za-z_]\\sw*\\.\\)" "\\([A-Za-z_]*\\)")
;; 					'(1 'actionscript-function-name-face))
    ))
	"Medium level highlighting for Actionscript mode.")

(defconst actionscript-font-lock-keywords-3
  (append
   actionscript-font-lock-keywords-2
   (list
		;; TODO: Add all the builtin objects in Actionscript.

		;; Builtin props.
;;		(list actionscript-builtin-props 1 'actionscript-builtin-props-face)

		;; Builtin funcs.
;;		(list actionscript-builtin-funcs 1 'actionscript-builtin-funcs-face)

    ))
  "Gaudy level highlighting for Actionscript mode.")

;; --------------------------------------------------------------------------------

(defun as-get-function-re(&optional function-name)
	"Returns a regular expression that will match the function signature
containing the supplied function-name. If function-name is omitted,
the regexp will match any function."
	(unless function-name
		(setq function-name actionscript-identifier-re))
	(let ((visibility-kwd (regexp-opt '("public" "protected" "private") nil))
				(other-kwd (regexp-opt '("final" "static" "override") nil))
				(get-set-kwd (regexp-opt '("get" "set") nil)))
		(concat "\\(?:^[ \t\n]*\\)"                                          ; Start of line, followed by any amount of whitespace.
						"\\(?:\\(" visibility-kwd "\\|" other-kwd "\\)[ \t\n]+\\)?"  ; (1) Any of the optional keywords, which must be followed by whitespace.
						"\\(?:\\(" visibility-kwd "\\|" other-kwd "\\)[ \t\n]+\\)?"  ; (2) A second optional keyword, which must be followed by whitespace.
						"\\<function\\>"                                             ; 'function' keyword.
						"[ \t\n]+"                                                   ; Mandatory whitespace.
						"\\(?:" get-set-kwd "[ \t\n]+\\)?"                           ; Optional get/set keyword, which must be followed by whitespace.
						"\\(" function-name "\\)"                                    ; (3) Function name.
						"[ \t\n]*"                                                   ; Optional whitespace.
						"("                                                          ; Opening paren for function parameters.
						"[ \t\n]*"                                                   ; Optional whitespace.
						"\\([\"a-zA-Z\-0-9_$*,:= \t\n]*?\\(?:\\.\\.\\.[a-zA-Z\-0-9_$]+\\)?\\)"           ; (4) Function parameters, including any trailing '...args'.
						")"                                                          ; Closing paren for function parameters.
						"[ \t\n]*"                                                   ; Optional whitespace.
						"\\(?::[ \t\n]*\\(" actionscript-identifier-re "\\|*\\)\\)?"    ; (5) Optional return value type specifier.
						"[ \t\n]*"                                                   ; Optional whitespace.
						"{")))                                                       ; Opening brace for function body.

(defconst as-function-re (as-get-function-re)
	"A regexp that matches a function signature in Actionscript 3.0.")

(defun as-get-attribute-re(&optional attribute-name)
	"Returns a regular expression that will match the class attribute
definition containing the supplied attribute-name. If attribute-name
is omitted, the regexp will match any class attribute."
	(unless attribute-name
		(setq attribute-name actionscript-identifier-re))
	(let ((visibility-kwd (regexp-opt '("public" "protected" "private") nil))
				(var-kwd (regexp-opt '("var" "const") nil))
				(other-kwd "static"))
		(concat "\\(?:^[ \t\n]*\\)"                                          ; Start of line, followed by any amount of whitespace.
						"\\(?:\\(" visibility-kwd "\\|" other-kwd "\\)[ \t\n]+\\)?"  ; (1) Any of the optional keywords, which must be followed by whitespace.
						"\\(?:\\(" visibility-kwd "\\|" other-kwd "\\)[ \t\n]+\\)?"  ; (2) A second optional keyword, which must be followed by whitespace.
						"\\<\\(" var-kwd "\\)\\>"                                    ; (3) var keyword.
						"[ \t\n]+"                                                   ; Mandatory whitespace.
						"\\(" attribute-name "\\)"                                   ; (4) Attribute name.
						"[ \t\n]*"                                                   ; Optional whitespace.
						"\\(?::[ \t\n]*\\(" actionscript-identifier-re "*\\)\\)?"    ; (5) Optional type specifier.
						)))

(defconst as-attribute-re (as-get-attribute-re)
	"A regexp that matches a class attribute definition in Actionscript 3.0.")

;; Support for imenu
(defvar as-imenu-generic-expression
	`((nil ,as-function-re 3)))

(defun as-imenu-init (mode-generic-expression)
  (setq imenu-generic-expression mode-generic-expression
				imenu-case-fold-search nil))

(defun as-get-beginning-of-defun()
	;; Returns the position.
	(save-excursion
		(when	(re-search-backward as-function-re nil t)
			(match-beginning 1))))

(defun as-get-end-of-defun()
	;; This only works if we are inside a defun.
	(save-excursion
		(when	(re-search-backward as-function-re nil t)
			(goto-char (match-end 0))
			;; Move back a char, so that point is right on the opening
			;; brace.
			(backward-char)
			(forward-list)
			(point))))

(defun as-get-end-of-defun2()
	;; This should work if we are not inside any defuns.
	(save-excursion
		(beginning-of-line) ; hack, in case point is currently inside a function sig.
		(when	(re-search-forward as-function-re nil t)
			(goto-char (match-end 0))
			;; Move back a char, so that point is right on
			;; the opening brace.
			(backward-char)
			(forward-list)
			(point))))

(defun as-beginning-of-defun()
	(interactive)
	(let ((pos (as-get-beginning-of-defun)))
		(if pos
				(goto-char pos)
				(message "Can't find any functions."))))

(defun as-inside-defun?()
	(let ((cur (point))
				(start (as-get-beginning-of-defun))
				(end (as-get-end-of-defun)))
		(and start
				 end
				 (> cur start)
				 (< cur end))))

(defun as-end-of-defun()
	(interactive)
	(if (as-inside-defun?)
			(goto-char (as-get-end-of-defun))
		(let ((pos (as-get-end-of-defun2)))
			(if pos
					(goto-char pos)
				(message "Can't find any functions.")))))

(defun as-mark-defun()
	(interactive)
	(let ((start (as-get-beginning-of-defun))
				(end (as-get-end-of-defun)))
		(if (not (or start end))
				(message "Can't find any functions.")
			(set-mark end)
			(goto-char start)
			(beginning-of-line))))

;; --------------------------------------------------------------------------------

;; Indentation (by Aemon Cannon: http://github.com/aemoncannon/as3-mode/tree/master/as3-mode.el)
 
(defun actionscript-indent-line ()
  "Indent current line of As3 code."
  (interactive)
  (indent-line-to (max 0 (as3-calculate-indentation))))

(defun as3-calculate-indentation ()
  "Return the column to which the current line should be indented."
  (save-excursion
    (as3-maybe-skip-leading-close-delim)
    (let ((pos (point)))
      (beginning-of-line)
      (if (not (search-backward-regexp "[^\n\t\r ]" 1 0))
   0
  (progn
   (as3-maybe-skip-leading-close-delim)
   (+ (current-indentation) (* standard-indent (as3-count-scope-depth (point) pos))))))))
 
(defun as3-maybe-skip-leading-close-delim ()
  (beginning-of-line)
  (forward-to-indentation 0)
  (if (looking-at "\\s)")
      (forward-char)
    (beginning-of-line)))
 
(defun as3-face-at-point (pos)
  "Return face descriptor for char at point."
  (plist-get (text-properties-at pos) 'face))
 
(defun as3-count-scope-depth (rstart rend)
  "Return difference between open and close scope delimeters."
  ;;Attempting Steve Yegge's solution..
  ;; (save-excursion
  ;; (let ((result (parse-partial-sexp rstart rend)))
  ;; (if (or (nth 3 result) (nth 4 result) (nth 7 result))
  ;;   0
  ;;  (nth 0 result)))))
  (save-excursion
    (goto-char rstart)
    (let ((open-count 0)
          (close-count 0)
          opoint)
      (while (and (< (point) rend)
                  (progn (setq opoint (point))
                         (re-search-forward "\\s)\\|\\s(" rend t)))
        (if (= opoint (point))
            (forward-char 1)
          (cond
           ;; Don't count if in string or comment.
           ((as3-face-at-point (- (point) 1)))
           ((looking-back "\\s)")
            (incf close-count))
           ((looking-back "\\s(")
            (incf open-count))
           )))
      (- open-count close-count))))
 
;; --------------------------------------------------------------------------------

;;;###autoload
(defun actionscript-mode ()
  "Major mode for editing Actionscript files.
\\{actionscript-mode-map}"
	(interactive)
	(kill-all-local-variables)
	(use-local-map actionscript-mode-map)
	(setq major-mode 'actionscript-mode
				mode-name "Actionscript")
;;				local-abbrev-table actionscript-mode-abbrev-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'actionscript-indent-line)
	(set-syntax-table actionscript-mode-syntax-table)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'comment-start)
	(setq comment-start "//")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        `((,(cond 
						 ((= actionscript-font-lock-level 1) 'actionscript-font-lock-keywords-1)
						 ((= actionscript-font-lock-level 2) 'actionscript-font-lock-keywords-2)
						 ((= actionscript-font-lock-level 3) 'actionscript-font-lock-keywords-3)))
          nil nil))
	(run-mode-hooks 'actionscript-mode-hook))

;; We need to make an adjustment to hideshow to work properly with AS syntax.
(add-to-list 'hs-special-modes-alist '(actionscript-mode "{" "}" "/[*/]" nil hs-c-like-adjust-block-beginning))

(provide 'actionscript-mode)

;; For testing
(defun reload-actionscript-mode ()
	(interactive)
	(with-current-buffer (get-buffer "actionscript-mode.el")
		(eval-buffer))
	(with-current-buffer (get-buffer "test.as")
		(actionscript-mode)
		(message "actionscript-mode reloaded.")))

(define-key global-map [f5] 'reload-actionscript-mode)


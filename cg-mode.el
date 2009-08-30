;;; cg-mode.el --- mode for editing cg programs

;; Copyright (C) 2005

;; Authors:    2005- Sean D'Epagnier
;; Maintainer: seanvbd@yahoo.com
;; Created:    Dec 2005
;; Keywords:   Cg languages

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

(defconst cg-version "$Revision: 0.01 $"
  "`cg-mode' version number.")

(defvar cg-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.cg\\'" . cg-mode))


(defconst cg-font-lock-keywords-1 nil
  "Subdued level highlighting for Cg mode.")

(defconst cg-font-lock-keywords-2 nil
  "Medium level highlighting for Cg mode.
See also `cg-font-lock-extra-types'.")

(defconst cg-font-lock-keywords-3 nil
  "Gaudy level highlighting for Cg mode.
See also `cg-font-lock-extra-types'.")

;; taken largely from the c mode from font-lock.el
(let* ((cg-keywords
        (eval-when-compile
          (regexp-opt '("break" "continue" "do" "else" "for" "if" "return"
                        "while"
                        "asm" "asm_fragment"
                        "technique" "pass" "compile"
                        "in" "out" "inout"
                        "typedef" "static" "const" "uniform" "packed"))))
        (cg-type-specs
         (eval-when-compile
           (regexp-opt '("struct" "interface"))))
        (cg-type-specs-depth
         (regexp-opt-depth cg-type-specs))
        (cg-type-names
        `(mapconcat 'identity
          (cons
           ,(eval-when-compile
              (regexp-opt
               '("void" "string"
                 "fragout" "fragout_float"
                 "sampler" "sampler1D" "sampler2D" "sampler3D"
                 "samplerCube" "samplerRECT" )))
           '("\\(bool\\|double\\|c?float\\|fixed\\|half\\|c?int\\)\\([1234]\\(x[1234]\\)?\\)?"))
          "\\|"))
       (cg-type-names-depth
        `(regexp-opt-depth ,cg-type-names))
       (cg-reserved-names
        (eval-when-compile
          (regexp-opt
             ;; reserved but not supported (Cg is UGLY!)
           '("short" "dword" "long" "signed"
             "auto" "catch" "char" "class" "column major"
             "const_cast" "decl" "default" "delete"
             "discard" "dynamic_cast" "emit" "enum" "explicit"
             "extern"  "friend" "get"   "goto" "inline"
             "long" "mutable" "namespace" "new" "operator"
             "pixelfragment" "pixelshader" "private"
             "protected" "public" "register"    "reinterpret_cast"
             "row_major" "sampler_state" "shared" "sizeof"
             "static_cast" "template" "this" "throw"
             "try" "typeid" "typename" "union" "using"
             "virtual" "volatile" "__identifier"
             "switch" "case" "default"))))
       (cg-reserved-names-depth
        `(regexp-opt-depth ,cg-reserved-names))
       (cg-bindings
        (eval-when-compile
          (regexp-opt
           '("COLOR" "COLOR0" "COLOR1" "COLOR2" "COLOR3"
             "POSITION" "BLENDWEIGHT" "NORMAL" "DIFFUSE"
             "SPECULAR" "FOGCOORD" "PSIZE" "ATTR6" "TANGENT"
             "TEXCOORD0" "TEXCOORD1" "TEXCOORD2" "TEXCOORD3"
             "TEXCOORD4" "TEXCOORD5" "TEXCOORD6" "TEXCOORD7"
             "HPOS" "PSIZ" "FOG" "FOGC" "COL0" "COL1" "BCOL0"))))
       (cg-bindings-depth
        (regexp-opt-depth cg-bindings))
       (cg-math-calls
        (eval-when-compile
          (regexp-opt
           '(;; Mathmatical Functions
             "abs" "acos" "all" "any" "asin" "atan" "atan2" "ceil" "clamp"
             "cos" "cosh" "cross" "degrees" "determinant" "dot" "exp" "exp2"
             "floor" "fmod" "frac" "frexp" "isfinite" "isinf" "isnan" "ldexp"
             "lerp" "lit" "log" "log2" "log10" "max" "min" "modf" "mul" "noise"
             "pow" "radians" "round" "rsqrt" "saturate" "sign" "sin" "sincos"
             "sinh" "smoothstep" "step" "sqrt" "tan" "tanh" "transpose"
             ;; Geometric Functions
             "distance" "faceforward" "length" "normalize" "reflect" "refract"
             ;; Texture Map Functions
             "tex1D" "tex1Dproj" "tex2D" "tex2Dproj" "texRECT" "texRECTproj"
             "tex3D" "tex3Dproj" "texCUBE texCUBEproj"
             ;; Derivitive Functions
             "ddx" "ddy"
             ;; Debugging Function
             "debug"
             ))))
       (cg-math-calls-depth
        (regexp-opt-depth cg-math-calls))
       (cg-preprocessor-directives
        (eval-when-compile
          (regexp-opt
           '("define"  "else" "endif" "if" "ifdef" "elif"
             "ifndef" "include" "line" "pragma" "undef"))))
       (cg-preprocessor-directives-depth
        (regexp-opt-depth cg-preprocessor-directives)))
 (setq cg-font-lock-keywords-1
  (list
   ;;
   ;; These are all anchored at the beginning of line for speed.
   ;;
   ;; Fontify function name definitions (GNU style; without type on line).
   '("^\\(\\sw+\\)[ \t]*(" 1 font-lock-function-name-face)
   ;;
   ;; Fontify error directives.
   '("^#[ \t]*error[ \t]+\\(.+\\)" 1 font-lock-warning-face prepend)
   ;;
   ;; Fontify filenames in #include <...> preprocessor directives as strings.
   '("^#[ \t]*\\(import\\|include\\)[ \t]*\\(<[^>\"\n]*>?\\)"
     2 font-lock-string-face)
   ;;
   ;; Fontify function macro names.
   '("^#[ \t]*define[ \t]+\\(\\sw+\\)(" 1 font-lock-function-name-face)
   ;;
   ;; Fontify symbol names in #if ... defined preprocessor directives.
   '("^#[ \t]*\\(elif\\|if\\)\\>"
     ("\\<\\(defined\\)\\>[ \t]*(?\\(\\sw+\\)?" nil nil
      (1 font-lock-builtin-face) (2 font-lock-variable-name-face nil t)))
   ;;
   ;; Fontify otherwise as symbol names, and the preprocessor directive names.
   (list
    (concat "^#[ \t]*\\(" cg-preprocessor-directives
            "\\)\\>[ \t!]*\\(\\sw+\\)?")
    '(1 font-lock-builtin-face)
    (list (+ 2 cg-preprocessor-directives-depth)
          'font-lock-variable-name-face nil t))))

 (setq cg-font-lock-keywords-2
  (append cg-font-lock-keywords-1
   (list
    ;;
    ;; Simple regexps for speed.
    ;;
    ;; Fontify all type names.
    `(eval .
      (cons (concat "\\<\\(" ,cg-type-names "\\)\\>") 'font-lock-type-face))
    ;;
    ;; Fontify all bindings.
    `(eval .
      (cons (concat "\\<\\(" ,cg-bindings "\\)\\>") 'font-lock-constant-face))
    ;;
    ;; Fontify all math calls.
    `(eval .
      (cons (concat "\\<\\(" ,cg-math-calls "\\)\\>") 'font-lock-builtin-face))
    ;;
    ;; Fontify reserved but unimplemented keywords
    `(eval .
      (cons (concat "\\<\\(" ,cg-reserved-names "\\)\\>") 'font-lock-warning-face))
    ;;
    ;; Fontify all builtin keywords (except case, default and goto; see below).
    (concat "\\<\\(" cg-keywords "\\|" cg-type-specs "\\)\\>")
    ;;
    ;; Fontify case/goto keywords and targets, and case default/goto tags.
    '("\\<\\(case\\|goto\\)\\>"
      (1 font-lock-keyword-face)
      ("\\(-[0-9]+\\|\\sw+\\)"
       ;; Return limit of search.
       (save-excursion (skip-chars-forward "^:\n") (point))
       nil
       (1 font-lock-constant-face nil t)))
    ;; Anders Lindgren <andersl@andersl.com> points out that it is quicker to
    ;; use MATCH-ANCHORED to effectively anchor the regexp on the left.
    ;; This must come after the one for keywords and targets.
    '(":" ("^[ \t]*\\(\\sw+\\)[ \t]*:[ \t]*$"
           (beginning-of-line) (end-of-line)
           (1 font-lock-constant-face)))
    )))

 (setq cg-font-lock-keywords-3
  (append cg-font-lock-keywords-2
   ;;
   ;; More complicated regexps for more complete highlighting for types.
   ;; We still have to fontify type specifiers individually, as C is so hairy.
   (list
    ;;
    ;; Fontify builtin true and false constants
    '("\\(true\\|false\\)" 1 font-lock-constant-face)
    ;;
    ;; Fontify all storage types, plus their items.
    `(eval .
      (list (concat "\\<\\(" ,cg-type-names "\\)\\>"
                    "\\([ \t*&]+\\sw+\\>\\)*")
            ;; Fontify each declaration item.
            (list 'font-lock-match-c-style-declaration-item-and-skip-to-next
                  ;; Start with point after all type specifiers.
                  (list 'goto-char (list 'or
                                         (list 'match-beginning
                                               (+ ,cg-type-names-depth 2))
                                         '(match-end 1)))
                  ;; Finish with point after first type specifier.
                  '(goto-char (match-end 1))
                  ;; Fontify as a variable or function name.
                  '(1 (if (match-beginning 2)
                          font-lock-function-name-face
                        font-lock-variable-name-face)))))
    ;;
    ;; Fontify all storage specs and types, plus their items.
    `(eval .
      (list (concat "\\<\\(" ,cg-type-specs "\\)\\>"
                    "[ \t]*\\(\\sw+\\)?")
          (list 1 'font-lock-keyword-face)
          (list ,(+ cg-type-specs-depth 2) 'font-lock-type-face nil t)
          (list 'font-lock-match-c-style-declaration-item-and-skip-to-next
                  nil
                  ;; Finish with point after the variable name if
                  ;; there is one.
                  `(if (match-end 2)
                       (goto-char (match-end 2)))
                  ;; Fontify as a variable or function name.
                  '(1 (if (match-beginning 2)
                          font-lock-function-name-face
                        font-lock-variable-name-face) nil t))))
    ;;
    ;; Fontify structures, or typedef names, plus their items.
    '("\\(}\\)[ \t*]*\\sw"
      (font-lock-match-c-style-declaration-item-and-skip-to-next
       (goto-char (match-end 1)) nil
       (1 font-lock-type-face)))
    ;;
    ;; Fontify anything at beginning of line as a declaration or definition.
    '("^\\(\\sw+\\)\\>\\([ \t*]+\\sw+\\>\\)*"
      (1 font-lock-type-face)
      (font-lock-match-c-style-declaration-item-and-skip-to-next
       (goto-char (or (match-beginning 2) (match-end 1))) nil
       (1 (if (match-beginning 2)
              font-lock-function-name-face
            font-lock-variable-name-face))))
    )))
 )


(defvar cg-font-lock-keywords cg-font-lock-keywords-3
  "Default expressions to highlight in C mode.
See also `cg-font-lock-extra-types'.")

(defvar cg-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Populate the syntax TABLE
    ;;(modify-syntax-entry ?_  "_"     table)
    (modify-syntax-entry ?_  "w"     table)
    (modify-syntax-entry ?\\ "\\"    table)
    (modify-syntax-entry ?+  "."     table)
    (modify-syntax-entry ?-  "."     table)
    (modify-syntax-entry ?=  "."     table)
    (modify-syntax-entry ?%  "."     table)
    (modify-syntax-entry ?<  "."     table)
    (modify-syntax-entry ?>  "."     table)
    (modify-syntax-entry ?&  "."     table)
    (modify-syntax-entry ?|  "."     table)
    (modify-syntax-entry ?\' "\""    table)
    ;; Set up block and line oriented comments.  The new C standard
    ;; mandates both comment styles even in C, so since all languages
    ;; now require dual comments, we make this the default.
    (cond
     ;; Emacs 22 and higher do nothing
     ((>= emacs-major-version 22))
     ;; XEmacs 19 & 20
     ((memq '8-bit c-emacs-features)
      (modify-syntax-entry ?/  ". 1456" table)
      (modify-syntax-entry ?*  ". 23"   table))
     ;; Emacs 19 & 20
     ((memq '1-bit c-emacs-features)
      (modify-syntax-entry ?/  ". 124b" table)
      (modify-syntax-entry ?*  ". 23"   table))
     ;; incompatible
     (t (error "Cg Mode is incompatible with this version of Emacs"))
     )
    (modify-syntax-entry ?\n "> b"  table)
    ;; Give CR the same syntax as newline, for selective-display
    (modify-syntax-entry ?\^m "> b" table)
    table)
  "Cg Syntax table")

(define-derived-mode cg-mode c-mode "Cg"
  "Major mode for editing Cg files."
  (set-syntax-table cg-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(cg-font-lock-keywords))

  )

(defvar cg-mode-hook nil
  "*Hook run when entering cg mode.")

(provide 'cg-mode)

;;; teco-mode.el --- Emacs mode for TECO macro -*- lexical-binding: t; -*-
;; Copyright (C) 2024 LdBeth
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; If not, see <https://www.gnu.org/licenses/>.
;;
;; This teco mode mainly targets TECO-64 which has slightly different
;; implementation on how to read in commands compared to traditional
;; tecoes.
(defconst teco-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?! "! 12b" table)

    (modify-syntax-entry ?\" "." table)
    (modify-syntax-entry ?\' "." table)
    (modify-syntax-entry ?\[ "." table) ; Q-reg push
    (modify-syntax-entry ?\] "." table) ; Q-reg pop
    (modify-syntax-entry ?\{ "." table) ; not teco command
    (modify-syntax-entry ?\} "." table)
    (modify-syntax-entry ?*  "." table)
    (modify-syntax-entry ?/  "." table)
    (modify-syntax-entry ?&  "."  table)
    (modify-syntax-entry ?#  "."  table)
    (modify-syntax-entry ?+  "."  table)
    (modify-syntax-entry ?-  "."  table)
    (modify-syntax-entry ?*  "."  table)
    (modify-syntax-entry ?.  "."  table)
    (modify-syntax-entry ?:  "."  table)
    (modify-syntax-entry ?@  "."  table)
    (modify-syntax-entry ?=  "."  table)
    (modify-syntax-entry ?<  "."  table)
    (modify-syntax-entry ?>  "."  table)
    (modify-syntax-entry ?|  "."  table)
    (modify-syntax-entry ?\n "> b" table)
    table))

(defvar teco-font-lock-keywords
  '(("[=<>]=\\|<>\\|//\\|<<\\|>>\\|\\^_\\|\\\\/" (0 'font-lock-operator-face))
    ("[#&*+/!~:@-]" (0 'font-lock-operator-face))
    ("F?['<>|]" (0 font-lock-keyword-face))
    ("\\^[][_\\@A-Za-z]" (0 font-lock-constant-face))
    ("\\(?:^U\\|[]UXQGM*%\C-u[]\\)\\(\\.?[a-zA-Z0-9]\\)"
     (1 font-lock-function-name-face))
    ("\"[ACDEFGLNRSTUVW<>=]" (0 font-lock-keyword-face))
    ("F[BCDKNRS_]" (0 font-lock-builtin-face))
    ("E[ABCFGIJKLMNPQRWXYZ%_]" (0 font-lock-builtin-face))
    ("E[1-4DEHOSTUV]" (0 'font-lock-variable-name-face))
    ("F?[HZ]\\|[B.]\\|F0" (0 'font-lock-variable-name-face))))

(eval-when-compile
  (defconst teco-atsign-1-arg-regexp
    "@[:,0-9]*\\(?:E[%BGILNQRW_]\\|F[BDKR]\\|\\^\\(?:A\\|U\\.?[a-zA-Z0-9]\\)\\|[\1\25=INOS_]\\)\\([^\C-@]\\)[^\C-@]*?\\(\\1\\)")
  (defconst teco-atsign-2-arg-regexp
    "@[:,0-9]*F[CNS_]\\([^\C-@]\\)[^\C-@]*?\\1[^\C-@]*?\\(\\1\\)"))

(defalias 'teco-mode-syntax-propertize
  (syntax-propertize-rules
   (teco-atsign-1-arg-regexp (1 "|") (2 "|"))
   (teco-atsign-2-arg-regexp (1 "|") (2 "|"))
   ("@[:,0-9]*\\(!\\)\\([^\C-@]\\)[^\C-@]*?\\(\\1\\)" (1 ".") (2 "!") (3 "!"))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.te[sc]\\'"  . teco-mode))

;;;###autoload
(define-derived-mode teco-mode prog-mode "TECO"
  :syntax-table teco-mode-syntax-table
  (setq font-lock-defaults (list 'teco-font-lock-keywords nil t))
  (setq-local comment-start "! "
              comment-end " !"
              syntax-propertize-function #'teco-mode-syntax-propertize))

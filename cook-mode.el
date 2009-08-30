;;; cook-mode.el
;;; Copyright (C) 2005, Scott Frazer <frazer.scott@gmail.com>

;; cook-mode.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc., 59
;; Temple Place, Suite 330, Boston, MA 02111-1307 USA

;; This is version 1.1 as of 13 Feb 2008

;;; Commentary:

;; This is a major mode for writing cook (a make replacement) files.  See:
;; http://www.canb.auug.org.au/~millerp/cook/cook.html
;; This mode is just the essentials right now, syntax highlighting and
;; indentation.  I'll add more as needed.

(require 'custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom

(defgroup cook-mode nil
  "*cook mode."
  :group 'programming)

(defcustom cook-indent-offset 4
  "*Indentation offset for `cook-mode'"
  :group 'cook-mode
  :type 'integer)

(defcustom cook-mode-hook nil
  "*List of functions to call on entry to `cook-mode'."
  :group 'cook-mode
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table

(defvar cook-mode-syntax-table
  (let ((st (make-syntax-table)))
    (if (featurep 'xemacs)
        (progn
          (modify-syntax-entry ?/ ". 14" st)
          (modify-syntax-entry ?* ". 23" st))
      (modify-syntax-entry ?/ ". 14n" st)
      (modify-syntax-entry ?* ". 23n" st))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?- "w" st)
    (modify-syntax-entry ?@ "w" st)
    (modify-syntax-entry ?% "w" st)
    st)
  "Syntax table for `cook-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation

(defun cook-indent-line ()
  "Indent current line for `cook-mode'."
  (interactive)
  (let ((indent-col))
    (save-excursion
      (beginning-of-line)
      (condition-case nil
          (progn
            (backward-up-list 1)
            (if (looking-at "{")
                (progn
                  (back-to-indentation)
                  (setq indent-col (+ (current-column) cook-indent-offset)))
              (re-search-forward "\\s-" (point-at-eol) 'go)
              (setq indent-col (current-column))))
        (error (setq indent-col 0))))
    (save-excursion
      (back-to-indentation)
      (when (and (looking-at "}") (>= indent-col cook-indent-offset))
        (setq indent-col (- indent-col cook-indent-offset))))
    (indent-line-to indent-col)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font-lock

(unless (boundp 'font-lock-builtin-face)
  (defvar font-lock-builtin-face 'font-lock-builtin-face
    "Face name to use for builtins.")
  (defface font-lock-builtin-face
    '((((type tty) (class color)) (:foreground "blue" :weight light))
      (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
      (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
      (((class color) (background light)) (:foreground "Orchid"))
      (((class color) (background dark)) (:foreground "LightSteelBlue"))
      (t (:bold t)))
    "Font Lock mode face used to highlight builtins."
    :group 'font-lock-highlighting-faces))

; Preprocessor
; (regexp-opt '("#include" "#include-cooked" "#include-cooked-nowarn" "#if" "#elif" "#else" "#endif" "#ifdef" "#ifndef" "#pragma") t)

; Variables
; (concat "\\<" (regexp-opt '("arg" "command-line-goals" "__FILE__" "__FUNCTION__" "graph_leaf_file" "graph_exterior_file" "graph_interior_file" "graph_leaf_pattern" "graph_exterior_pattern" "graph_interior_pattern" "__LINE__" "need" "parallel_hosts" "parallel_jobs" "parallel_rsh" "search_list" "self" "target" "targets" "thread-id" "younger" "version") t) "\\>")
; (concat (regexp-opt '("@1" "@2" "@3" "@4" "@5" "@6" "@7" "@8" "@9" "%0" "%1" "%2" "%3" "%4" "%5" "%6" "%7" "%8" "%9") t))

; Builtins
; (concat "\\[\\s-*" (regexp-opt '("addprefix" "addsuffix" "and" "basename" "cando" "catenate" "collect_lines" "collect" "cook" "count" "defined" "dirname" "dir" "dos-path" "downcase" "entryname" "execute" "exists" "exists-symlink" "expr" "filter_out" "filter" "find_command" "findstring" "firstword" "fromto" "getenv" "glob" "head" "home" "if" "in" "interior_files" "join" "leaf_files" "match" "matches" "match_mask" "mtime" "mtime-seconds" "notdir" "not" "operating_system" "options" "or" "os" "pathname" "patsubst" "prepost" "print" "quote" "read_lines" "readlink" "read" "relative_dirname" "reldir" "resolve" "shell" "sort_newest" "sort" "split" "stringset" "stripdot" "strip" "substr" "subst" "suffix" "tail" "un-dos-path" "unsplit" "upcase" "uptodate" "wildcard" "word" "wordlist" "words" "write") t) "\\s-+")

; Keywords
; (concat "\\<" (regexp-opt '("data" "dataend" "else" "host-binding" "loopstop" "single-thread" "fail" "if" "return" "then" "function" "loop" "set" "unsetenv") t) "\\>")

(defvar cook-mode-font-lock-keywords
  '(
    ("\\(#\\(?:e\\(?:l\\(?:if\\|se\\)\\|ndif\\)\\|i\\(?:f\\(?:n?def\\)?\\|nclude\\(?:-cooked\\(?:-nowarn\\)?\\)?\\)\\|pragma\\)\\)"
     . font-lock-reference-face)
    ("\\<\\(__\\(?:\\(?:F\\(?:ILE\\|UNCTION\\)\\|LINE\\)__\\)\\|arg\\|command-line-goals\\|graph_\\(?:exterior_\\(?:file\\|pattern\\)\\|interior_\\(?:file\\|pattern\\)\\|leaf_\\(?:file\\|pattern\\)\\)\\|need\\|parallel_\\(?:hosts\\|jobs\\|rsh\\)\\|se\\(?:arch_list\\|lf\\)\\|t\\(?:argets?\\|hread-id\\)\\|version\\|younger\\)\\>"
     . font-lock-variable-name-face)
    ("\\(%[0-9]\\|@[1-9]\\)"
     . font-lock-variable-name-face)
    ("\\[\\s-*\\(a\\(?:dd\\(?:\\(?:pre\\|suf\\)fix\\)\\|nd\\)\\|basename\\|c\\(?:a\\(?:ndo\\|tenate\\)\\|o\\(?:llect\\(?:_lines\\)?\\|ok\\|unt\\)\\)\\|d\\(?:efined\\|ir\\(?:name\\)?\\|o\\(?:s-path\\|wncase\\)\\)\\|e\\(?:ntryname\\|x\\(?:ecute\\|ists\\(?:-symlink\\)?\\|pr\\)\\)\\|f\\(?:i\\(?:lter\\(?:_out\\)?\\|nd\\(?:_command\\|string\\)\\|rstword\\)\\|romto\\)\\|g\\(?:etenv\\|lob\\)\\|h\\(?:ead\\|ome\\)\\|i\\(?:nterior_files\\|[fn]\\)\\|join\\|leaf_files\\|m\\(?:atch\\(?:_mask\\|es\\)?\\|time\\(?:-seconds\\)?\\)\\|not\\(?:dir\\)?\\|o\\(?:p\\(?:erating_system\\|tions\\)\\|[rs]\\)\\|p\\(?:at\\(?:hname\\|subst\\)\\|r\\(?:\\(?:epos\\|in\\)t\\)\\)\\|quote\\|re\\(?:ad\\(?:_lines\\|link\\)?\\|l\\(?:ative_dirname\\|dir\\)\\|solve\\)\\|s\\(?:hell\\|ort\\(?:_newest\\)?\\|plit\\|tri\\(?:ngset\\|p\\(?:dot\\)?\\)\\|u\\(?:bstr?\\|ffix\\)\\)\\|tail\\|u\\(?:n\\(?:-dos-path\\|split\\)\\|p\\(?:\\(?:cas\\|todat\\)e\\)\\)\\|w\\(?:ildcard\\|ord\\(?:list\\|s\\)?\\|rite\\)\\)\\s-+"
     (1 font-lock-builtin-face))
    ("\\<function\\s-+\\(\\sw+\\)"
     (1 font-lock-function-name-face))
    ("^\\s-*[][a-zA-Z0-9_.%]+:+\\(\\s-*.+:\\)?"
     . font-lock-type-face)
    ("\\<\\(data\\(?:end\\)?\\|else\\|f\\(?:ail\\|unction\\)\\|host-binding\\|if\\|loop\\(?:stop\\)?\\|return\\|s\\(?:et\\|ingle-thread\\)\\|then\\|unsetenv\\)\\>"
     . font-lock-keyword-face)
    )
  "Keyword highlighting specification for `cook-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap

(defvar cook-mode-map nil "`cook-mode' keymap.")
(if (not cook-mode-map)
    (let ((map (make-keymap)))
      (setq cook-mode-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode startup

(defun cook-mode ()
  "cook-mode is a major mode for browsing cook files.\n\n
\\{cook-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table cook-mode-syntax-table)
  (set (make-local-variable 'indent-line-function) 'cook-indent-line)
  (set (make-local-variable 'font-lock-defaults)
       '(cook-mode-font-lock-keywords))
  (set (make-local-variable 'comment-start) "/* ")
  (set (make-local-variable 'comment-end) " */")
  (use-local-map cook-mode-map)
  (setq major-mode 'cook-mode)
  (setq mode-name "cook")
  (run-hooks 'cook-mode-hook))

(setq auto-mode-alist (append (list
                               (cons "\\.cook$" 'cook-mode)
                               (cons "\\.cook[.]?rc" 'cook-mode)
                               (cons "[Cc]ook[.]?file" 'cook-mode))
                              auto-mode-alist))

(provide 'cook-mode)

;;; cook-mode.el ends here.

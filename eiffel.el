;;; eiffel.el --- major mode for editing Eiffel files.

;; Copyright (C) 1989, 1990, 93, 94, 95, 96, 99, 2000, 01, 02, 03
;;                         Tower Technology Corporation,
;;                         Free Software Foundation,
;;                         Bob Weiner,
;;                         C. Adrian

;; Authors: 1989-1990 Stephen Omohundro, ISE and Bob Weiner
;;          1993-1996 Tower Technology Corporation
;;          1999-2003 Martin Schwenke <martin@meltin.net>
;; Maintainer: martin@meltin.net
;; Keywords: eiffel languages oop
;; Requires: font-lock, compile, easymenu, imenu

;; This file is derived from eiffel4.el from Tower Technology Corporation.
;;
;; Known bugs:
;;
;; * eif-short buffer doesn't get font locked under GNU Emacs 19.34.
;;
;; * eif-debug can hang under (at least) XEmacs 21.4.[89] in the wait
;;   loop if there is input pending (that is, if the user hits return
;;   an extra time).  Not yet tested under XEmacs 21.5.
;;
;; This file is distributed under the same terms as GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;  NEW VERSIONS
;;    The latest version of this mode is always available via:
;;      http://meltin.net/hacks/emacs/

;;  INSTALLATION
;;    To install, simply copy this file into a directory in your
;;    load-path and add the following two commands in your .emacs file:
;;
;;    (add-to-list 'auto-mode-alist '("\\.e\\'" . eiffel-mode))
;;    (autoload 'eiffel-mode "eiffel" "Major mode for Eiffel programs" t)
;;

;;; History:
;;

;; Add history stuff here!!!
;; src: https://web.archive.org/web/20060621120537/http://meltin.net/hacks/emacs/src/eiffel.el [2026-03-14]

;;; Code:

(require 'font-lock)
(require 'compile)
(require 'easymenu)
(require 'imenu)

(defconst eiffel-version-string
  "$Id: eiffel.el,v 2.67 2003/06/14 10:41:01 martins Exp $"
  "Version string to make reporting bugs more meaningful.
Note that if this file becomes part of GNU Emacs then the file might
be changed by the Emacs maintainers without this version number
changing.  This means that if you are reporting a bug for a version
that was shipped with Emacs, you should report the Emacs version!")

(defgroup eiffel nil
  "Eiffel mode for Emacs"
  :group 'oop)

(defgroup eiffel-indent nil
  "Indentation variables in Eiffel mode"
  :prefix "eif-"
  :group 'eiffel)

(defgroup eiffel-compile nil
  "Compilation support variables in Eiffel mode"
  :prefix "eif-"
  :group 'eiffel)

(defun eif-customize ()
  "Run \\[customize-group] for the `eiffel' group."
  (interactive)
  (customize-group 'eiffel))

;; Indentation amount variables.
;;
;; The default values correspond to style used in ``Eiffel: The
;; Language''.

(defcustom eif-indent-increment 3
  "*Default indentation interval (in spaces)."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-class-level-kw-indent 0
  "*Indentation for Class level keywords.
Specified as number of `eif-indent-increments'.  See the variable
`eif-class-level-keywords-regexp'."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-extra-class-level-kw-indent 0
  "*Number of extra spaces to add to `eif-class-level-kw-indent'.
This results in the actual indentation of a class level keyword.  Can
be negative."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-class-level-comment-indent 0
  "*Indentation of comments at the beginning of a class.
Specified as number of `eif-indent-increments'."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-extra-class-level-comment-indent 0
  "*Number of spaces to add to `eif-class-level-comment-indent'.
This results in the actual indentation of a class level comment.  Can
be negative."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-inherit-level-kw-indent 2
  "*Indentation of keywords falling under the Inherit clause.
Specified as number of `eif-indent-increments'.  See the variable
`eif-inherit-level-keywords'."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-extra-inherit-level-kw-indent 0
  "*Number of spaces to add to `eif-inherit-level-kw-indent'.
This results in the actual indentation of an inherit level keyword.
Can be negative."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-feature-level-indent 1
  "*Indentation amount of features.
Specified as number of `eif-indent-increments'."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-extra-feature-level-indent 0
  "*Number of spaces to add to `eif-feature-level-indent'.
This results in the indentation of a feature.  Can be negative."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-feature-level-kw-indent 2
  "*Indentation of keywords belonging to individual features.
Specified as number of `eif-indent-increments'.  See the variable
`eif-feature-level-keywords-regexp'."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-extra-feature-level-kw-indent 0
  "*Number of spaces to add to `eif-feature-level-kw-indent'.
This results in the actual indentation of a feature level keyword.
Can be negative."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-feature-level-comment-indent 3
  "*Indentation of comments at the beginning of a feature.
Specified as number of `eif-indent-increments'."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-extra-feature-level-comment-indent 0
  "*Number of spaces to add to `eif-feature-level-comment-indent'.
This results in the actual indentation of a feature level comment.
Can be negative."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-body-comment-indent 0
  "*Indentation of comments in the body of a routine.
Specified as number of `eif-indent-increments')"
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-extra-body-comment-indent 0
  "*Number of spaces to add to `eif-body-comment-indent'.
This results in the actual indentation of a routine body comment.  Can
be negative."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-check-keyword-indent 0
  "*Extra indentation for the check clause as described in ETL.
Specified as number of `eif-indent-increments'.  Default is 0, which
is different than in ETL's 1."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-extra-check-keyword-indent 0
  "*Number of spaces to add to `eif-check-keyword-indent'.
This results in the actual indentation of a check keyword.  Can be
negative."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-rescue-keyword-indent -1
  "*Extra indentation for the rescue clause as described in ETL.
Specified as number of `eif-indent-increments'.  Default is -1."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-extra-rescue-keyword-indent 0
  "*Number of spaces to add to `eif-rescue-keyword-indent'.
This results in the actual indentation of a rescue keyword.  Can be
negative."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-then-indent 0
  "*Indentation for a `then' appearing on a line by itself.
This is as opposed to a `then' on the same line as an `if'.  Specified
as number of `eif-indent-increments'."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-extra-then-indent 1
  "*Number of spaces to add to `eif-then-indent'.
This results in the actual indentation of a `then' appearing on a line
by itself.  Can be negative."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-continuation-indent 1
  "*Extra indentation for a continued statement line.
Specified as number of `eif-indent-increments'."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-extra-continuation-indent 0
  "*Number of spaces to add to `eif-continuation-indent'.
This results in the actual indentation of a continued statement
line.  Can be negative."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-string-continuation-indent 0
  "*Extra indentation for a continued string.
Specified as number of `eif-indent-increments'."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-extra-string-continuation-indent -1
  "*Number of spaces to add to `eif-string-continuation-indent'.
This results in the actual indentation of a continued string.  Can be
negative."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-indent-string-continuations-relatively-flag t
  "*Non-nil means string continuations are indented relative to 1st character.
That is, `eif-string-continuation-indent' and
`eif-extra-string-continuation-indent' are added to position of first
character of string.  If nil, string continuations are indented
relative to indent of previous line."
  :type 'boolean
  :group 'eiffel-indent)

(defcustom eif-set-tab-width-flag t
  "*Non-nil means `tab-width' is set to `eif-indent-increment' in `eiffel-mode'."
  :type 'boolean
  :group 'eiffel-indent)

(defcustom eif-preprocessor-indent 0
  "*Indentation for lines GOBO preprocessor directives.
Specified as number of `eif-indent-increments' from left margin."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-fill-max-save 4096
  "*Maximum size of a paragraph to save before filling.
Normally \\[eif-fill-paragraph] will mark a buffer as modified even if
the fill operation does not make any changes.  If the paragraph being
filled is smaller than the value of this variable then the contents of
the paragraph will be saved for comparison with the paragraph after
the fill operation.  If they are the same, the buffer modification
state is restored.  Set this to 0 to disable this feature, or a very
big number to enable it for all paragraphs."
  :type 'integer
  :group 'eiffel-indent)

(defcustom eif-use-gnu-eiffel t
  "*If t include support for compilation using GNU SmartEiffel."
  :type 'boolean
  :group 'eiffel-compile)

(defcustom eif-compile-command
  (if (file-executable-p "/usr/bin/se-compile")
      "se-compile"
    "compile")
  "*Program to use for compiling Eiffel programs.
The default is \"compile\", unless \"/usr/bin/se-compile\" exists, as
in Debian GNU/Linux, when the default value is \"se-compile\"."
  :type 'string
  :group 'eiffel-compile)

(defcustom eif-short-command "short"
  "*Program to use for producing short form of Eiffel classes."
  :type 'string
  :group 'eiffel-compile)

(defcustom eif-compile-options ""
  "*Options to use for compiling Eiffel programs."
  :type 'string
  :group 'eiffel-compile)

;;
;; No user-customizable definitions below this point.
;;

;;
;; Indentation macros.
;;

(defmacro eif-class-level-kw-indent-m ()
  "Indentation amount for Class level keywords (in number of spaces)."
  '(+ (* eif-class-level-kw-indent eif-indent-increment)
      eif-extra-class-level-kw-indent))

(defmacro eif-class-level-comment-indent-m ()
  "Indentation amount for Class level comments (in number of spaces)."
  '(+ (* eif-class-level-comment-indent eif-indent-increment)
      eif-extra-class-level-comment-indent))

(defmacro eif-inherit-level-kw-indent-m ()
  "Indentation amount for Inherit level keywords (in number of spaces)."
  '(+ (* eif-inherit-level-kw-indent eif-indent-increment)
      eif-extra-inherit-level-kw-indent))

(defmacro eif-feature-level-indent-m ()
  "Indentation amount for features (in number of spaces)."
  '(+ (* eif-feature-level-indent eif-indent-increment)
      eif-extra-feature-level-indent))

(defmacro eif-feature-level-kw-indent-m ()
  "Indentation amount for Feature level keywords (in number of spaces)."
  '(+ (* eif-feature-level-kw-indent eif-indent-increment)
      eif-extra-feature-level-kw-indent))

(defmacro eif-body-comment-indent-m ()
  "Indentation amount for comments in routine bodies (in number of spaces)."
  '(+ (* eif-body-comment-indent eif-indent-increment)
      eif-extra-body-comment-indent))

(defmacro eif-feature-level-comment-indent-m ()
  "Indentation amount for Feature level comments (in number of spaces)."
  '(+ (* eif-feature-level-comment-indent eif-indent-increment)
      eif-extra-feature-level-comment-indent))

(defmacro eif-check-keyword-indent-m ()
  "Indentation amount for Check keyword (in number of spaces)."
  '(+ (* eif-check-keyword-indent eif-indent-increment)
      eif-extra-check-keyword-indent))

(defmacro eif-rescue-keyword-indent-m ()
  "Indentation amount for Rescue keyword (in number of spaces)."
  '(+ (* eif-rescue-keyword-indent eif-indent-increment)
      eif-extra-rescue-keyword-indent))

(defmacro eif-then-indent-m ()
  "Indentation amount for `then' appearing on a line by itself (in number of spaces)."
  '(+ (* eif-then-indent eif-indent-increment)
      eif-extra-then-indent))

(defmacro eif-continuation-indent-m ()
  "Indentation amount for a statement continuation line (in number of spaces)."
  '(+ (* eif-continuation-indent eif-indent-increment)
      eif-extra-continuation-indent))

(defmacro eif-string-continuation-indent-m ()
  "Indentation amount for a statement continuation line (in number of spaces)."
  '(+ (* eif-string-continuation-indent eif-indent-increment)
      eif-extra-string-continuation-indent))

(defmacro eif-preprocessor-indent-m ()
  "Indentation amount for a preprocessor statement (in number of spaces)."
  '(* eif-preprocessor-indent eif-indent-increment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Keyword Regular Expression Constants.               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst eif-non-id-char-regexp "\\S_" ;; "[^a-z0-9_]"
  "The characters that are not part of identifiers.")

(defun eif-post-anchor (regexp)
  "Anchor given REGEXP with end-word delimiter and `eif-non-id-char-regexp'."
  (concat "\\(" regexp "\\)\\>" eif-non-id-char-regexp))

(defun eif-word-anchor (regexp)
  "Anchor given REGEXP with word delimiters and `eif-non-id-char-regexp'."
  (concat "\\<\\(" regexp "\\)\\>" eif-non-id-char-regexp))

(defun eif-anchor (regexp)
  "Anchor given REGEXP front and back to match line break or non-symbol char."
  (concat "\\(^\\|\\S_\\<\\)\\(" regexp "\\)\\($\\|\\>\\S_\\)"))

;; Note invariant is handled as a special case since it is both a
;; class-level and a from-level keyword
;; Note obsolete is handled as a special case since it is both a
;; class-level and a feature-level keyword
(defconst eif-class-level-keywords
  "indexing\\|class\\|deferred[ \t]+class\\|expanded[ \t]+class\\|reference[ \t]+class\\|separate[ \t]+class\\|inherit\\|creation\\|feature"
  "Keywords introducing class-level clauses.
Note that `invariant' and `obsolete' are not included here since can
function as more than one type of keyword.")

(defconst eif-class-level-keywords-regexp
  (eif-word-anchor eif-class-level-keywords)
  "Regexp of keywords introducing class level clauses, with some context.
See `eif-class-level-keywords'.")

(defconst eif-inherit-level-keywords
  "rename\\|redefine\\|undefine\\|select\\|export"
  "Those keywords which introduce subclauses of the inherit clause.")

(defconst eif-feature-level-keywords
  "require\\|local\\|deferred\\|separate\\|do\\|once\\|ensure\\|alias\\|external"
  "Those keywords which are internal to features (in particular, routines).")

(defconst eif-feature-level-keywords-regexp
  (eif-word-anchor eif-feature-level-keywords)
  "Regexp of keywords internal to features (usually routines).
See `eif-feature-level-keywords'.")

(defconst eif-end-keyword "end" "The `end' keyword.")

(defconst eif-end-on-current-line ".*[ \t]end[ \t]*;?[ \t]*\\(--.*\\)?$"
  "Regular expression to identify lines ending with the `end' keyword.")

(defconst eif-control-flow-keywords
  "if\\|inspect\\|from\\|debug"
  "Keywords which introduce control-flow constructs.")

(defconst eif-control-flow-matching-keywords
  (concat "deferred\\|do\\|once" "\\|" eif-control-flow-keywords)
  "Keywords that may cause the indentation of an `eif-control-flow-keyword'.
If these occur prior to an `eif-control-flow-keyword' then the
`eif-control-flow-keyword' is indented.  Note that technically, `end'
is part of this list but it is handled separately in the function
\[eif-matching-kw\].")

(defconst eif-control-flow-matching-keywords-regexp
  (eif-word-anchor eif-control-flow-matching-keywords)
  "Regexp of keywords maybe causing indentation of `eif-control-flow-keyword'.
See `eif-control-flow-keywords'.")

(defconst eif-check-keyword "check"
  "The `check' keyword.")

(defconst eif-check-keywords-regexp
  (eif-word-anchor eif-check-keyword)
  "The `check' keyword (with trailing context).")

;; FIXME: Doesn't work if once keyword is followed by a string on next
;; line, but didn't get broken by this attempt at factoring.
(defconst eif-check-matching-keywords-regexp
  eif-control-flow-matching-keywords-regexp
  "Keywords that may cause the indentation of an `eif-check-keyword'.
If these occur prior to an `eif-check-keyword' then the
`eif-check-keyword' is indented.  Note that technically, `end' is part
of this list but it is handled separately in the function
\[eif-matching-kw\].  See also `eif-control-flow-matching-keywords-regexp'.")

;; FIXME: This could be fixed or removed.
(defconst eif-end-keyword-regexp "\\(^\\|[^a-z0-9_]\\)end\\($\\|[^a-z0-9_]\\)"
  "The `end' keyword with context.")

(defconst eif-end-matching-keywords
  (concat "check\\|class\\|feature\\|rename\\|redefine\\|undefine" "\\|"
	  "select\\|export\\|separate\\|external\\|alias" "\\|"
	  eif-control-flow-matching-keywords)
  "Those keywords whose clause is terminated by an `end' keyword.")

(defconst eif-end-matching-keywords-regexp
  (eif-word-anchor eif-end-matching-keywords)
  "Regexp of keywords whose clause is terminated by an `end' keyword.
See `eif-end-matching-keywords'.")

(defconst eif-rescue-keyword "rescue"  "The `rescue' keyword.")

(defconst eif-obsolete-keyword "obsolete"  "The `obsolete' keyword.")

(defconst eif-rescue-keywords-regexp
  (eif-word-anchor eif-rescue-keyword)
  "The `rescue' keyword (with trailing context).")

(defconst eif-rescue-matching-keywords-regexp
  (eif-word-anchor "deferred\\|do\\|once")
  "Keywords that may cause the indentation of an `eif-rescue-keyword'.
If these occur prior to an `eif-rescue-keyword' then the
`eif-rescue-keyword' is indented.  Note that technically, `end' is
part of this list but it is handled separately in the function
\[eif-matching-kw\].  See also `eif-control-flow-matching-keywords-regexp'.")

(defconst eif-from-level-keywords
  "until\\|variant\\|loop"
  "Keywords occuring inside of a from clause.")

(defconst eif-from-level-keywords-regexp
  (eif-word-anchor eif-from-level-keywords)
  "Regexp of keywords occuring inside of a from clause.
See `eif-from-level-keywords'.")

(defconst eif-from-keyword  "from" "The keyword `from'.")

(defconst eif-if-or-inspect-level-keywords
  "elseif\\|else\\|when"
  "Keywords occuring inside of an if or inspect clause.")

(defconst eif-if-or-inspect-level-keywords-regexp
  (eif-word-anchor eif-if-or-inspect-level-keywords)
  "Regexp of keywords occuring inside of an if or inspect clause.
See eif-if-or-inspect-level-keywords.")

(defconst eif-if-or-inspect-keyword-regexp
  (eif-word-anchor "if\\|inspect")
  "Regexp matching the `if' or `inspect' keywords.")

(defconst eif-then-keyword ".*[ \t)]then[ \t]*$"
  "The keyword `then' with possible leading text.")

(defconst eif-solitary-then-keyword "then" "The keyword `then'.")

(defconst eif-then-matching-keywords "\\(if\\|elseif\\|when\\)"
  "Keywords that may alter the indentation of an `eif-then-keyword'.
If one of these occur prior to an `eif-then-keyword' then this sets
the indentation of the `eif-then-keyword'.  Note that technically,
`end' is part of this list but it is handled separately in the
function \[eif-matching-kw\].  See also
`eif-control-flow-matching-keywords-regexp'.")

(defconst eif-invariant-keyword "invariant" "The `invariant' keyword.")

(defconst eif-invariant-matching-keywords
  "from\\|feature"
  "Keywords that may cause the indentation of an `eif-invarient-keyword'.
If one of these occurs prior to an `eif-invariant-keyword' then the
`eif-invariant-keyword' is indented.  Note that technically, `end' is
part of this list but it is handled separately in the function
\[eif-matching-kw\].  See also `eif-control-flow-matching-keywords-regexp'.")

(defconst eif-obsolete-matching-keywords
  "\\(is\\|class\\)"
  "Keywords that may cause the indentation of an `eif-obsolete-keyword'.
If one of these occurs prior to an `eif-obsolete-keyword' then the
`eif-obsolete-keyword' is indented.")

(defconst eif-create-keyword
  "create"
  "Eiffel class keyword.  Can be used at class or minor level.")

(defconst eif-create-keyword-regexp
  (eif-post-anchor "create")
  "Regexp matching `create' keyword, with trailing context.")

(defconst eif-indentation-keywords
  (concat "indexing\\|rescue\\|inherit\\|creation" "\\|"
	  "invariant\\|require\\|local\\|ensure\\|obsolete" "\\|"
	  eif-from-level-keywords "\\|"
	  eif-if-or-inspect-level-keywords "\\|"
	  eif-end-matching-keywords)
  "Keywords that match any eiffel keyword triggering indentation.")

(defconst eif-indentation-keywords-regexp
  (eif-word-anchor eif-indentation-keywords)
  "Regexp of keywords that match any eiffel keyword triggering indentation.
See `eif-indentation-keywords'.")

(defconst eif-once-non-indent-regexp
  "\\s-*once\\(\\s-\\|\n\\)+\""
  "Regexp of Eiffel once keyword in context not affecting indentation.")

(defconst eif-feature-indentation-keywords-regexp
  (eif-word-anchor "creation\\|feature")
  "Keywords which denote the presence of features following them.")

(defconst eif-is-keyword-regexp "\\(.*[ \t)]\\)?is[ \t]*\\(--.*\\)?$"
  "The `is' keyword (with some context).")

(defconst eif-multiline-routine-is-keyword-regexp
  ".*([^)]*)\\([ \t\n]*\\|[ \t\n]*:[][ \t\nA-Za-x0-9_,]*\\)is[ \t]*\\(--.*\\)?$"
  "The `is' keyword (with some context).")

(defconst eif-operator-keywords
  "and\\|or\\|implies"
  "Eiffel operator keywords.")

(defconst eif-operator-regexp
  (concat "[ \t]*\\([@*/+]\\|-[^-]\\|\\<\\("
	  eif-operator-keywords
	  "\\)[ \t(]\\)")
  "Eiffel operators - used to identify continuation lines.
See `eif-operator-keywords'.")

(defconst eif-operator-eol-regexp
  (concat ".*\\([@*/+-]\\|\\<\\(" eif-operator-keywords
	  "\\)\\|:=\\)[ \t]*\\(--.*\\)?$")
  "Eiffel operators - used to identify continuation lines.")

(defconst eif-misc-keywords
  (concat "agent\\|all\\|as\\|frozen\\|infix\\|like" "\\|"
	  "old\\|precursor\\|prefix\\|strip\\|unique\\|xor" "\\|"
	  "expanded\\|reference")
  "Eiffel miscellaneous keywords.")

(defconst eif-preprocessor-keywords
  "#\\(define\\|undefine\\|ifdef\\|else\\|endif\\|ifndef\\|include\\)"
  "Eiffel GOBO preprocessor keywords.")

(defconst eif-preprocessor-keywords-regexp
  (eif-post-anchor eif-preprocessor-keywords)
  "Eiffel GOBO preprocessor keywords, with context.
See `eif-preprocessor-keywords'.")

(defconst eif-smarteiffel-guru-keywords
  (concat "c_inline_c\\|c_inline_h\\|to_pointer" "\\|"
	  "is_expanded_type\\|is_basic_expanded_type" "\\|"
	  "object_size\\|object_id_memory" "\\|"
	  "se_guru01\\|se_guru02\\|se_guru03")
  "Eiffel keywords used by gurus with the SmartEiffel compiler.")

(defconst eif-major-variable-keywords
  (concat "[Vv]oid\\|[Rr]esult\\|[Cc]urrent\\|[Tt]rue\\|[Ff]alse" "\\|"
	  "[Pp]recursor\\|io\\|std_input\\|std_output\\|std_error")
  "Eiffel keywords representing major variables.")

(defconst eif-standard-class-keywords
  (concat "ANY\\|BIT\\|BOOLEAN\\|CHARACTER\\|DOUBLE\\|GENERAL" "\\|"
	  "INTEGER\\|NONE\\|POINTER\\|REAL\\|STRING")
  "Eiffel keywords representing standard classes.")

(defconst eif-all-keywords
  (concat eif-indentation-keywords    "\\|"
	  eif-solitary-then-keyword   "\\|"
	  eif-create-keyword          "\\|"
	  eif-end-keyword)
  "Regexp matching (nearly) any eiffel keyword in a line.
Does not include `is'.")

(defconst eif-all-keywords-regexp
  (concat "\\("
	  (eif-word-anchor eif-all-keywords) "\\|"
	  eif-preprocessor-keywords-regexp   "\\)")
  "Anchored regexp matching (nearly) any eiffel keyword in a line.
Does not include `is'.  See `eif-all-keywords'.")

(defconst eiffel-comment-start-skip
  "--+|?[ \t]*"
  "Regexp matching the beginning of an Eiffel comment.")

(defconst eif-non-source-line
  (concat "[ \t]*\\(\\(" "--" "\\|"
	  eif-preprocessor-keywords-regexp "\\).*\\)?$")
  "RE matching line with only whitespace and comment or preprocessor keyword.")

(defconst eif-variable-or-const-regexp "[^()\n]*:[^=].*"
  "RE to match a variable or constant declaration.")

;; Factor out some important important regexps for use in
;; eif-{beginning,end}-of-feature.

(defconst eif-routine-begin-regexp
  "\\([a-z_][a-zA-Z_0-9]*\\)\\s-*\\(([^)]*)\\)?\\s-*\\(:\\s-*[A-Z][A-Za-z0-9_]*\\)?\\s-*\\<is\\>\\s-*\\(--.*\\)?$"
  "Regexp matching the beginning of an Eiffel routine declaration.")

(defconst eif-attribute-regexp
  (concat "[a-z_][^-:\n]*:\\s-*"
	  "\\(like\\s-*[a-zA-Z][a-z_0-9]*\\|"
	  "\\(\\(expanded\\|reference\\)\\s-*\\)?[A-Z][A-Z_0-9]*"
	  "\\(\\s-*\\[[^-\n]*\\]\\)?\\)"
	  "\\s-*\\($\\|[;)].*\\|--.*\\)")
  "Regexp matching an Eiffel attribute, parameter or local variable.")

(defconst eif-constant-regexp
  "[a-z_][^-:\n]*:[^-\n]*\\<is\\>\\s-*[^ \t\n]"
  "Regexp matching an Eiffel constant declaration.")

(defconst eif-probably-feature-regexp
  (concat "\\(" eif-routine-begin-regexp
	  "\\|" eif-attribute-regexp
	  "\\|" eif-constant-regexp "\\)")
  "Regexp probably matching an Eiffel feature.
This will also match local variable and parameter declarations.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar eif-matching-indent -1
  "Indentation of the keyword found on the last call to \[eif-matching-kw\].
-1 if no match was found.")

(defvar eif-matching-kw-for-end nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Font-lock support.
;;
;; Most of this font-lock code was originally contributed by: Karl
;; Landstr�m <kala9469@student.su.se>.  Much of it has now been
;; factored out above.
;;

(defconst eiffel-font-lock-keywords-1
  `(;; hidden comments
    ("--|.*" 0 font-lock-keyword-face t)
    ;; routines
    (,(concat "^[ \t]*" eif-routine-begin-regexp) 1 font-lock-function-name-face ))
  "Regular expressions to use with font-lock mode.")

(defconst eiffel-font-lock-keywords-2
  (append
   eiffel-font-lock-keywords-1
   `(;; Assertions.
     ;; FIXME: Cyril thinks these should just be part of the keywords below.
     (,(eif-anchor "check\\|ensure then\\|ensure\\|invariant\\|require else\\|require\\|variant") 2 font-lock-reference-face nil)

     ;; Preprocessor keywords.  Note that, by luck more than planning,
     ;; these aren't font-locked when they're not indented, since the
     ;; '#' isn't a word boundary (which is added by eif-anchor).
     (,(eif-post-anchor eif-preprocessor-keywords) 2 font-lock-builtin-face nil)

     ;; Keywords.  The first few can appear in conjunction with other
     ;; keywords, and the anchored regexp doesn't cater for overlaps,
     ;; thus there are several entries here.
     (,(eif-anchor "class\\|is\\|not")        2 font-lock-keyword-face nil)
     (,(eif-anchor eif-operator-keywords)     2 font-lock-keyword-face nil)
     (,(eif-anchor eif-misc-keywords)         2 font-lock-keyword-face nil)
     (,(eif-anchor eif-all-keywords)          2 font-lock-keyword-face nil)

     ;; Quoted expr's in comments.
     ("`[^`'\n]*'" 0 font-lock-string-face t)

     ;; Classes.
     (,(eif-anchor eif-standard-class-keywords) 2 font-lock-type-face)))
   "Regular expressions to use with font-lock mode and level 2 fontification.")

(defconst eiffel-font-lock-keywords-3
  (append
   eiffel-font-lock-keywords-2
   `(;; attributes/parameters/local variables
     (,(concat "^[ \t]*" eif-attribute-regexp) (0 nil)
      ("\\s-*\\(\\<[a-z][a-zA-Z_0-9]*\\)\\s-*\\(,\\|:[^;\n]*\\|).*\\)"
       (re-search-backward "\\((\\|^\\)" nil t)
       (end-of-line)
       (1 font-lock-variable-name-face)))
     ;; constants
     (,(concat "^[ \t]*" eif-constant-regexp) (0 nil)
      ("\\s-*\\(\\<[A-Za-z][a-zA-Z_0-9]*\\)\\s-*\\(,\\|:.*\\)"
       (beginning-of-line) (end-of-line)
       (1 font-lock-constant-face)))))
  "Regular expressions to use with font-lock mode and level 3 fontification.")

(defconst eiffel-font-lock-keywords-4
  ;; SmartEiffel guru keywords and major variables.
  (append
   eiffel-font-lock-keywords-3
   `((,(eif-anchor eif-smarteiffel-guru-keywords) 2 font-lock-warning-face)
     (,(eif-anchor eif-major-variable-keywords) 2 font-lock-constant-face))))

(defvar eiffel-font-lock-keywords eiffel-font-lock-keywords-1
  "Default expressions to highlight in Eiffel mode.
See also `c-font-lock-extra-types'.")

(defconst eiffel-font-lock-defaults
  '((eiffel-font-lock-keywords
     eiffel-font-lock-keywords-1
     eiffel-font-lock-keywords-2
     eiffel-font-lock-keywords-3
     eiffel-font-lock-keywords-4)
    nil nil nil nil))

(and (boundp 'font-lock-defaults-alist)
     (add-to-list 'font-lock-defaults-alist
		  (cons 'eiffel-mode
			eiffel-font-lock-defaults)))

;; font-lock faces used by GNU Emacs and XEmacs are inconsistent.
(if (and (not (boundp 'font-lock-constant-face))
	 (fboundp 'copy-face))
    (copy-face 'font-lock-variable-name-face 'font-lock-constant-face))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Compilation support for GNU SmartEiffel.
;;

(defvar eif-compile-dir nil
  "Current directory where Eiffel compilations are taking place.
Possibly used for error location.")

(defvar eif-root-class nil
  "Current Eiffel root class being compiled/debugged.")

(defvar eif-compile-target nil
  "Current Eiffel compilation target.")

(defvar eif-debug-target nil
  "Current Eiffel debug target.")

(defvar eif-root-proc "make"
  "Current Eiffel root procedure.")

(defvar eif-run-command nil
  "Current command to run after Eiffel compile.")

(defvar eif-debug-command nil
  "Current debug command to run after Eiffel debug compile.")

(defun eif-compilation-mode-hook ()
  "Hook function to set local value for `compilation-error-screen-columns'.
This should be nil for SmartEiffel compiles, because column positions are
returned as character positions rather than screen columns."
  ;; In Emacs > 20.7 compilation-error-screen-columns is buffer local.
  (or (assq 'compilation-error-screen-columns (buffer-local-variables))
      (make-local-variable 'compilation-error-screen-columns))
  (setq compilation-error-screen-columns nil))

(defun eif-compile ()
  "Compile an Eiffel root class."
  (interactive)
  (eif-compile-prompt)
  (eif-compile-internal))

(defun eif-set-compile-options ()
  "Set Eiffel compiler options."
  (interactive)
  (setq eif-compile-options
	(read-string "Eiffel compiler options: " eif-compile-options)))

;; Taken from Emacs 20.3 subr.el (just in case we're running under Emacs 19).
(defun eif-split-string (string &optional separators)
  "Split STRING into substrings separated by SEPARATORS.
Each match for SEPARATORS is a splitting point.  The substrings
between the splitting points are made into a list which is returned.
If SEPARATORS is absent, it defaults to \"[ \\f\\t\\n\\r\\v]+\".

If there is match for SEPARATORS at the beginning of STRING, we do not
include a null substring for that.  Likewise, if there is a match
at the end of STRING, we do not include a null substring for that."
  (let ((rexp (or separators "[ \f\t\n\r\v]+"))
	(start 0)
	notfirst
	(list nil))
    (while (and (string-match rexp string
			      (if (and notfirst
				       (= start (match-beginning 0))
				       (< start (length string)))
				  (1+ start) start))
		(< (match-beginning 0) (length string)))
      (setq notfirst t)
      (or (eq (match-beginning 0) 0)
	  (and (eq (match-beginning 0) (match-end 0))
	       (eq (match-beginning 0) start))
	  (setq list
		(cons (substring string start (match-beginning 0))
		      list)))
      (setq start (match-end 0)))
    (or (eq start (length string))
	(setq list
	      (cons (substring string start)
		    list)))
    (nreverse list)))

(defun eif-run ()
  "Run a compiled Eiffel program."
  (interactive)
  (setq eif-run-command
	(read-string "Command to run: "
		     (or eif-run-command
			 eif-compile-target
			 (file-name-sans-extension
			  (if (eq system-type 'windows-nt)
			      buffer-file-name
			    (file-name-nondirectory (buffer-file-name)))))))
  (eif-run-internal))

(defun eif-debug ()
  "Run the SmartEiffel debugger."
  (interactive)

  (eif-compile-prompt)
  
  (setq eif-debug-target
	(file-name-sans-extension
	 (read-string "Debug target name: "
		      (or eif-debug-target
			  (concat eif-compile-target "_debug")))))
    
  (let* ((eif-compile-options (concat "-sedb " eif-compile-options))
	 (eif-compile-target eif-debug-target)
	 (buff (eif-compile-internal))
	 (proc (get-buffer-process buff)))

    ;; This works under GNU Emacs, but hangs under at least some
    ;; versions of XEmacs if there is input pending.
    (while (eq (process-status proc) 'run)
      (sit-for 1))

    (if (= (process-exit-status proc) 0)
	(progn
	  (setq eif-debug-command
		(read-string "Debugger command to run: "
			     (or eif-debug-command
				 eif-debug-target
				 (file-name-sans-extension
				  (if (eq system-type 'windows-nt)
				      buffer-file-name
				    (file-name-nondirectory
				     (buffer-file-name)))))))
	  (let ((eif-run-command eif-debug-command))
	    (eif-run-internal))))))

(defun eif-compile-prompt ()
  "Prompt for information required to compile an Eiffel root class."

  ;; Do the save first, since the user might still have their hand on
  ;; the mouse.
  (save-some-buffers (not compilation-ask-about-save) nil)

  (setq eif-compile-dir (file-name-directory (buffer-file-name)))
  (setq eif-root-class
	(file-name-sans-extension
	 (read-string "Name of root class: "
		      (or eif-compile-target
			  (file-name-sans-extension
			   (file-name-nondirectory (buffer-file-name)))))))
  (setq eif-compile-target eif-root-class)
  (setq eif-root-proc
	(read-string "Name of root procedure: "
		     eif-root-proc)))

(defun eif-compile-internal ()
  "Compile an Eiffel root class.  Internal version.
Returns the same thing as \\[compile-internal] - the compilation buffer."

  (let ((cmd (concat eif-compile-command
		     " "    eif-compile-options
		     " -o " eif-compile-target
		     (if (eq system-type 'windows-nt) ".exe")
		     " "    eif-root-class
		     " "    eif-root-proc))
	(compilation-mode-hook (cons 'eif-compilation-mode-hook
				     compilation-mode-hook)))
    (compile-internal cmd "No more errors")))

(defun eif-run-internal ()
  "Run a compiled Eiffel program.  Internal version."

  (let* ((tmp-buf (current-buffer))
	 (words   (eif-split-string eif-run-command))
	 (cmd     (expand-file-name (car words))))

    (apply 'make-comint cmd cmd nil (cdr words))
    (switch-to-buffer tmp-buf)
    (switch-to-buffer-other-window (concat "*" cmd "*"))))

;; This has been loosened up to spot parts of messages that contain
;; references to multiple locations.  Thanks to Andreas
;; <nozone@sbox.tu-graz.ac.at>.  Also, the column number is a character
;; count rather than a screen column, so we need to make sure that
;; compilation-error-screen-columns is nil.  Note that in XEmacs this
;; variable doesn't exist, so we end up in the wrong column.  Hey, at
;; least we're on the correct line!
(add-to-list 'compilation-error-regexp-alist
	     '("^Line \\([0-9]+\\) column \\([0-9]+\\) in [^ ]+ (\\([^)]+\\.[Ee]\\))" 3 1 2))

(defun eif-short ()
  "Display the short form of an Eiffel class."
  (interactive)
  (let* ((class (read-string
		 "Class or file: "
		 (if (buffer-file-name)
		     (file-name-nondirectory (buffer-file-name)))))
	 (buf (get-buffer-create (concat "*Eiffel - short " class "*"))))

    (shell-command (concat eif-short-command " " class) buf)
    (save-excursion
      (set-buffer buf)
      (let ((font-lock-defaults eiffel-font-lock-defaults))
	(font-lock-fontify-buffer))
      (toggle-read-only 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Utility Functions.                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eif-feature-quote ()
  "Put a `' around the current feature name."
  (interactive)
  (save-excursion
    ;; Only try to go back to the beginning of the feature if we're
    ;; not already there.
    (if (/= (point)
	    (save-excursion
	      (forward-sexp)
	      (backward-sexp)
	      (point)))
	(backward-sexp))
    (insert "`")
    (forward-sexp)
    (insert "'"))
  (if (looking-at "'")
      (forward-char 1)))

(defun eif-peeking-backwards-at (regexp)
  "Return non-nil is previous character exists and is matched by REGEXP.
The match is actually an unbounded match starting at the previous character."
  (save-excursion
    (save-match-data
      (and (not (bobp))
	   (or (backward-char) t)
	   (looking-at regexp)))))

(defsubst eif-in-comment-p ()
  "Return t if point is in a comment."
  (interactive)
  (save-excursion
    (nth 4 (parse-partial-sexp
 	    (save-excursion (beginning-of-line) (point))
 	    (point)))))

(defun eif-in-comment-or-quoted-string-p ()
  "Return t if point is in a comment or quoted string."
  (or (eif-in-comment-p)
      (eif-in-quoted-string-p)))
  
(defun eif-not-in-comment-or-quoted-string-p ()
  "Return t if point is not in a comment or quoted string."
  (not (eif-in-comment-or-quoted-string-p)))
  
(defun eif-near-comment-p ()
  "Return t if point is close enough to a comment for filling purposes."
  (or (eif-in-comment-p)
      (and (or (looking-at comment-start-skip)
	       (eif-peeking-backwards-at comment-start-skip))
	   (not (eif-in-quoted-string-p)))
      (looking-at (concat "[ \t]*" comment-start-skip))))

(defun eif-re-search-forward (regexp &optional limit noerror)
  "Search forward from point for REGEXP not in comment or string.
`case-fold-search' is set to nil when searching.  For details on other
arguments see \\[re-search-forward]."

  (interactive "sRE search: ")
  (let ((start (point))
	found case-fold-search)
    (while (and (setq found (re-search-forward regexp limit noerror))
		(eif-in-comment-or-quoted-string-p)))
    (if (and found
	    (eif-not-in-comment-or-quoted-string-p))
	found
      (if (eq noerror t)
	  (goto-char start))
      nil)))

(defun eif-re-search-backward (regexp &optional limit noerror)
  "Search backward from point for REGEXP not in comment or string.
`case-fold-search' is set to nil when searching.  For details on other
arguments see \\[re-search-forward]."
  (interactive "sRE search: ")
  (let ((start (point))
	found case-fold-search)
    (while (and (setq found (re-search-backward regexp limit noerror))
		(eif-in-comment-or-quoted-string-p)))
    (if (and found
	    (eif-not-in-comment-or-quoted-string-p))
	found
      (if (eq noerror t)
	  (goto-char start))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Indentation Functions.                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eif-skip-leading-whitespace ()
  "Move to the first non-whitespace character on the current line."
      (end-of-line)
      (let ((line-end (point)))
	(beginning-of-line)
	(skip-syntax-forward "-" line-end)))

(defun eif-calc-indent ()
  "Calculate the indentation of the current line of eiffel code.
This function handles the case where there is a keyword that affects
indentation at the beginning of the current line.  For lines that
don't start with a relevant keyword, the calculation is handed off to
\\[eif-calc-non-keyword-indent]."
  (let ((indent   0)
	kw-match)

    (save-excursion
      (eif-skip-leading-whitespace)
      
      ;; Look for a keyword on the current line.
      (if (looking-at eif-all-keywords-regexp)

	  (cond ((looking-at eif-create-keyword-regexp)
		 ;; Class-level or minor occurence?
		 (if (save-excursion (eif-find-beginning-of-feature))
		     ;; Minor.
		     (setq indent (eif-calc-indent-non-keyword))
		   ;; Class-level.
		   (setq indent (eif-class-level-kw-indent-m))))
		;; There's possibly a better way of coding this exception.
		((looking-at eif-once-non-indent-regexp)
		 (setq indent (eif-calc-indent-non-keyword)))
		((looking-at eif-class-level-keywords-regexp)
		 ;; File level keywords (indent defaults to 0)
		 (setq indent (eif-class-level-kw-indent-m)))
		((looking-at eif-inherit-level-keywords)
		 ;; Inherit level keywords (indent defaults to
		 ;; 2*eif-indent-increment)
		 (setq indent (eif-inherit-level-kw-indent-m)))
		((looking-at eif-feature-level-keywords-regexp)
		 ;; Feature level keywords (indent defaults to
		 ;; (eif-feature-level-indent-m) + eif-indent-increment)
		 (setq indent (eif-feature-level-kw-indent-m)))
		((looking-at eif-end-keyword)
		 ;; End keyword (indent to level of matching keyword)
		 (if (string-match "end"
				   (eif-matching-kw
				    eif-end-matching-keywords-regexp))
		     ;; Then
		     (if (= eif-matching-indent
			    (eif-feature-level-kw-indent-m))
			 ;; Then
			 (setq indent (eif-class-level-kw-indent-m))
		       ;; Else
		       (setq indent
			     (- eif-matching-indent eif-indent-increment)))
		   ;; Else
		   (setq indent eif-matching-indent))
		 ;; FIXME: This is broken!!!
		 (if (<= indent (eif-feature-level-indent-m))
		     (save-excursion
		       (end-of-line)
		       (while (and (< (point) (point-max))
				   (or (forward-char 1) t)
				   (looking-at eif-non-source-line))
			 (end-of-line))
		       (if (not (looking-at eif-non-source-line))
			   (setq indent (eif-inherit-level-kw-indent-m))
			 (setq indent (eif-class-level-kw-indent-m))))))
		((looking-at eif-control-flow-keywords)
		 ;; Control flow keywords
		 ;;  Indent to same level as a preceding "end" or
		 ;;  if no preceding "end" is found, indent to the level
		 ;;  of the preceding "do" plus the value of
		 ;;  eif-indent-increment
		 (setq kw-match
		       (eif-matching-kw
			eif-control-flow-matching-keywords-regexp))
		 (cond ((string-match "end" kw-match)
			(setq indent eif-matching-indent))
		       (t
			(setq indent
			      (+ eif-matching-indent eif-indent-increment)))))
		((looking-at eif-check-keywords-regexp)
		 ;; Check keyword
		 ;;  Indent to level of preceding "end"+eif-indent-increment or
		 ;;  if no preceding "end" is found, indent to the level of
		 ;;  the preceding eif-check-matching-keywords-regexp plus the
		 ;;  value (eif-indent-increment + eif-check-keyword-indent).

		 (setq kw-match (eif-matching-kw
				 eif-check-matching-keywords-regexp))
		 (cond ((string-match "end" kw-match)
			(setq indent (+ eif-matching-indent
					(eif-check-keyword-indent-m))))
		       (t
			(setq indent
			      (+ eif-matching-indent
				 (+ eif-indent-increment
				    (eif-check-keyword-indent-m)))))))
		((looking-at eif-rescue-keywords-regexp)
		 ;; Rescue keyword
		 ;;  Indent to level of preceding "end"+eif-indent-increment or
		 ;;  if no preceding "end" is found, indent to the level of
		 ;;  the preceding eif-rescue-matching-keywords-regexp plus the
		 ;;  value (eif-indent-increment + eif-rescue-keyword-indent).
		 (setq kw-match (eif-matching-kw
				 eif-rescue-matching-keywords-regexp))
		 (cond ((string-match "end" kw-match)
			(setq indent (+ eif-matching-indent
					(eif-rescue-keyword-indent-m))))
		       (t
			(setq indent eif-matching-indent))))
		((looking-at eif-from-level-keywords-regexp)
		 ;; From level keywords (indent to level of matching "From")
		 (if (string-match "end" (eif-matching-kw eif-from-keyword))
		     ;; Closest matching KW is `end'.
		     (setq indent (- eif-matching-indent eif-indent-increment))
		   ;; Closest matching KW is one of `eif-from-keyword'.
		   (setq indent eif-matching-indent)))
		((looking-at eif-if-or-inspect-level-keywords-regexp)
		 ;; If level keywords (indent to level of matching
		 ;; "If" or "Inspect")
		 (if (string-match "end"
				   (eif-matching-kw
				    eif-if-or-inspect-keyword-regexp))
		     ;; Closest matching KW is `end'.
		     (setq indent (- eif-matching-indent eif-indent-increment))
		   ;; Closest matching KW is one of `eif-if-or-inspect-keyword-regexp'.
		   (setq indent eif-matching-indent)))
		((looking-at eif-solitary-then-keyword)
		 ;; Handles case where "then" appears on a line by itself
		 ;;   (Indented to level of the matching if, elseif or when)
		 (eif-matching-kw eif-then-matching-keywords)
		 (setq indent (+ eif-matching-indent (eif-then-indent-m))))
		((looking-at eif-invariant-keyword)
		 ;; Invariant keyword
		 ;;   (Indented to level of the matching from or feature)
		 (if (string-match "from"
				   (eif-matching-kw eif-invariant-matching-keywords))
		     ;; Then - loop invariant
		     (setq indent eif-matching-indent)
		   ;; Else - class invariant
		   (setq indent (eif-class-level-kw-indent-m))))
		((looking-at eif-obsolete-keyword)
		 ;; Obsolete keyword
		 ;;   (Indented to the level of the matching from or feature)
		 (if (string-match "is"
				   (eif-matching-kw eif-obsolete-matching-keywords))
		     ;; Then - feature obsolete
		     (setq indent (eif-feature-level-kw-indent-m))
		   ;; Else - class obsolete
		   (setq indent (eif-class-level-kw-indent-m))))
		((looking-at eif-preprocessor-keywords-regexp)
		 (setq indent (eif-preprocessor-indent-m))))
	;; No keyword.  Hand off...
	(setq indent (eif-calc-indent-non-keyword))))
    indent))

(defun eif-calc-indent-non-keyword ()
  "Calculate indentation of current Eiffel code line, without leading keyword.
This function generally assumes that the preceding line of code is
indented properly, and usually bases the indentation of the current
line on that preceding line."
  (let ((indent   0)
	originally-looking-at-comment originally-looking-at-lone-string
	continuation id-colon)

    (save-excursion
      (eif-skip-leading-whitespace)

      ;; Is the line we are trying to indent a comment line?
      (setq originally-looking-at-comment (looking-at comment-start-skip))

      ;; Is the line we are trying to indent a lone string?
      (setq originally-looking-at-lone-string (looking-at "\"[^\"]*\"[ \t]*$"))

      ;; Are we in a multi-line parenthesis expression?
      (if (or (and (> (eif-in-paren-expression) 0)
		   (> (setq indent (eif-indent-multi-line)) -1))
	      (setq indent (eif-manifest-array-indent)))

	  ;; Multi-line parenthesis expression.
	  ;; Move string continuation lines as per configuration.
	  (if (looking-at "%")
	      (setq indent (+ indent (eif-string-continuation-indent-m))))

	;; Else Find the first preceding line with non-comment source on it
	;; that is not a continuation line of a multi-line parenthesized
	;; expression (and isn't a preprocessor line :-).

	;; Record whether this line begins with an operator.  We assume
	;; that the line is a continuation line if it begins with an operator
	(beginning-of-line)
	(setq continuation (looking-at eif-operator-regexp))

	;; Record whether the line being indented begins with an "<id> :"
	;; This is used in indenting assertion tag expressions.
	(setq id-colon (looking-at "[ \t]*[a-zA-Z0-9_]+[ \t]*:"))
	
	(forward-line -1)
	(beginning-of-line)
	(while (and (looking-at eif-non-source-line)
		    (not (= (point) 1)))
	  (forward-line -1)
	  (beginning-of-line))
	(if (eif-line-contains-close-paren)
	    (backward-sexp))
	(eif-skip-leading-whitespace)

	(cond ((and (= (point) 1)
		    originally-looking-at-comment
		    (setq indent (eif-class-level-comment-indent-m))))
	      ;; 'eif-is-keyword-regexp' case must precede
	      ;; '(not eif-all-keywords-regexp)' case since "is" is not
	      ;; part of 'eif-all-keywords-regexp'
	      ((or (looking-at eif-is-keyword-regexp)
		   (looking-at eif-multiline-routine-is-keyword-regexp)
		   (looking-at eif-obsolete-keyword))
	       (if originally-looking-at-comment
		   ;; Then  the line we are trying to indent is a comment
		   (setq indent (eif-feature-level-comment-indent-m))
		 ;; Else  the line being indented is not a comment
		 (setq indent (eif-feature-level-kw-indent-m))))
	      ;; Feature indentation keyword or class-level `create'.
	      ((or (looking-at eif-feature-indentation-keywords-regexp)
		   (and (looking-at eif-create-keyword-regexp)
			(not (save-excursion
			       (eif-find-beginning-of-feature)))))
	       (setq indent (eif-feature-level-indent-m)))
	      ((and (looking-at eif-indentation-keywords-regexp)
		    (not (looking-at eif-once-non-indent-regexp)))
	       (if (looking-at eif-end-on-current-line)
		   (setq indent (eif-current-line-indent))
		 (setq indent
		       (+ (eif-current-line-indent) eif-indent-increment))))
	      ((looking-at eif-solitary-then-keyword)
	       (setq indent (- (+ (eif-current-line-indent) eif-indent-increment)
			       (eif-then-indent-m))))
	      ((looking-at eif-then-keyword)
	       (setq indent (eif-current-line-indent)))
	      ((looking-at (concat eif-end-keyword eif-non-id-char-regexp))
	       (if (= (setq indent (eif-current-line-indent))
		      (eif-feature-level-kw-indent-m))
		   (setq indent (eif-feature-level-indent-m))
		 (eif-matching-line)
		 (if (string-match eif-check-keyword eif-matching-kw-for-end)
		     (setq indent (- indent (eif-check-keyword-indent-m))))))
	      ((looking-at eif-variable-or-const-regexp)
	       ;;Either a variable declaration or a pre or post condition tag
	       (if originally-looking-at-comment
		   ;; Then  the line we are trying to indent is a comment
		   (if (= (setq indent (eif-current-line-indent))
			  (eif-feature-level-indent-m))
		       ;; Then - a feature level comment
		       (setq indent (eif-feature-level-comment-indent-m))
		     ;; Else - some other kind of comment
		     (setq indent (+ indent (eif-body-comment-indent-m))))
		 ;; Else  the line being indented is not a comment
		 (if (setq indent (eif-indent-assertion-continuation id-colon))
		     indent
		   ;; One of the ways of getting here is when we're
		   ;; in a split line in an indexing clause.
		   ;; Strings on their own need to be given some
		   ;; extra indent.
		   (if originally-looking-at-lone-string
		       (if (looking-at "[ \t]*\"[^\"]*\"[ \t]*$")
			   (setq indent (eif-current-line-indent))
			 (setq indent (+ (eif-current-line-indent)
					 eif-indent-increment)))
		     (setq indent (eif-current-line-indent))))))
	      ((setq indent (eif-manifest-array-start))
	       indent)
	      ;; OK, this is a sanity check, but it kills a minor
	      ;; instance of `create', so we need to code the corner
	      ;; case.  As for minor instance of `once'.
	      ((or (not (looking-at eif-all-keywords-regexp))
		   (looking-at eif-create-keyword-regexp)
		   (looking-at eif-once-non-indent-regexp))
	       (if originally-looking-at-comment
		   ;; Then  the line we are trying to indent is a comment
		   (cond ((eif-continuation-line)
			  (setq indent
				(+ (- (eif-current-line-indent)
				      eif-indent-increment)
				   (eif-body-comment-indent-m))))
			 ;; preceding line is at eif-feature-level-indent -
			 ;; assume that the preceding line is a parent
			 ;; class in an inherit clause
			 ((= (eif-current-line-indent)
			     (eif-feature-level-indent-m))
			  (setq indent
				(+ (eif-inherit-level-kw-indent-m)
				   (eif-body-comment-indent-m))))
			 (t
			  (setq indent
				(+ (eif-current-line-indent)
				   (eif-body-comment-indent-m)))))
		 ;; Else line being indented is not a comment
		 
		 ;; The line the point is on is the one above the line being
		 ;; indented
		 (beginning-of-line)
		 (if (or continuation (looking-at eif-operator-eol-regexp))
		     ;; Then the line being indented is a continuation line
		     (if  (eif-continuation-line)
			 ;; The line preceding the line being indented is
			 ;; also a continuation line.  Indent to the current
			 ;; line indentation.
			 (setq indent (eif-current-line-indent))
		       ;; Else The line preceding the line being indented is
		       ;; not a continuation line.  Indent an extra
		       ;; eif-continuation-indent
		       (setq indent (+ (eif-current-line-indent)
				       (eif-continuation-indent-m))))
		   ;; Else the line being indented is not a continuation line.
		   (if (eif-continuation-line)
		       (if id-colon
			   ;; Then the line preceding the one being indented
			   ;; is an assertion continuation.  Indent the current
			   ;; line to the same level as the preceding assertion
			   ;; tag.
			   (setq indent (eif-indent-assertion-tag))
			 ;; Then the line preceding the one being indented is
			 ;; a continuation line.  Un-indent by an
			 ;; eif-continuation-indent.
			 (setq indent (- (eif-current-line-indent)
					 eif-indent-increment)))
		     ;; Else the line preceding the line being indented is
		     ;; also not a continuation line.
		     
		     (if (and (looking-at "[ \t]*\"[^\"]*\"[ \t]*$")
			      (not originally-looking-at-lone-string))
			 (setq indent (- (eif-current-line-indent)
					 eif-indent-increment))
		       ;; Else use the current indent.
		       (setq indent (eif-current-line-indent))))))))))
    indent))

(defun eif-continuation-line ()
  "Return non-nil if the current line is a continuation line."
  (or (looking-at eif-operator-regexp)
      (save-excursion
	(forward-line -1)
	(beginning-of-line)
	(looking-at eif-operator-eol-regexp))))

(defun eif-indent-assertion-continuation (id-colon)
  "Generally, are we in line that is a continuation of an assertion?
More precisely, are we inside a pre or a post condition clause on a
line that is a continuation of a multi-line assertion beginning with a
tag?  If so, return the indentation of the continuation line.  The
argument ID-COLON is t if the line we are indenting begins with
\"<id> :\", and nil otherwise."
  (let ((limit (point)))
    (if (save-excursion
	  (if (re-search-backward
	       (concat eif-feature-level-keywords-regexp "\\|"
		       eif-end-keyword-regexp) nil t)
	      (if (looking-at "ensure\\|require")
		  (setq limit (point)))))
	(save-excursion
	  (end-of-line)
	  (if (and (not id-colon) (re-search-backward ": *" limit t))
	      (progn
		(goto-char (match-end 0))
		(current-column)))))))

(defun eif-indent-assertion-tag ()
  "Return indentation for part of a multi-line assertion.
That is, the current line is assumed to be a continuation of a
multi-line assertion, and we return the required indentation."
  (save-excursion
    (if (re-search-backward "ensure\\|require\\|variant\\|invariant" nil t)
	(+ (eif-current-line-indent) eif-indent-increment)
      ;; This option should not occur
      (error "Could not find assertion tag"))))

(defun eif-matching-kw (matching-keyword-regexp)
  "Search backwards and return a keyword in MATCHING-KEYWORD-REGEXP.
Also set the value of variable `eif-matching-indent' to the
indentation of the keyword found.  If an `end' keyword occurs prior to
finding one of the keywords in MATCHING-KEYWORD-REGEXP and it
terminates a check clause, set the value of variable
`eif-matching-indent' to the indentation of the `end' minus the value
of `eif-check-keyword-indent'."
  (let ((search-regexp (concat "[^a-z0-9A-Z_.]"
			       eif-end-keyword
			       "[^a-z0-9A-Z_.]\\|[^a-z0-9A-Z_.]"
			       matching-keyword-regexp))
	(keyword ""))
    (save-excursion
      ;; Search backward for a matching keyword.
      ;; Note that eif-once-non-indent-regexp indicates we haven't
      ;; found a match so should keep going.
      (while (and (eif-re-search-backward search-regexp 1 t)
		  (looking-at eif-once-non-indent-regexp)
		  (not (= (point) 1))))
      (if (looking-at search-regexp)
	  ;; Then - a keyword was found
	  (progn
	    (setq keyword
		  (buffer-substring (match-beginning 0) (match-end 0)))
	    (if (and (looking-at eif-end-keyword-regexp)
		     (eif-matching-line)
		     (string-match eif-check-keyword eif-matching-kw-for-end))
		;; Then
		(setq eif-matching-indent (- (eif-current-line-indent)
					     (eif-check-keyword-indent-m)))
	      ;; Else
	      (setq eif-matching-indent (eif-current-line-indent))))
	;; Else no keyword was found.  I think this is an error
	(setq eif-matching-indent 0)
	(message "No matching indent keyword was found"))
      keyword)))

(defun eif-line-contains-close-paren ()
  "Return t if the current line contains a close paren, nil otherwise.
If a close paren is found, the point is placed immediately after the
last close paren on the line.  If no paren is found, the point is
placed at the beginning of the line."
  (let ((search-min 0))
    (beginning-of-line)
    (setq search-min (point))
    (end-of-line)
    (if (search-backward ")" search-min t)
	;; Then
	(progn
	  (forward-char 1)
	  t)
      ;; Else
      (beginning-of-line)
      nil)))

;; Not Currently Used
;;(defun eif-quoted-string-on-line-p ()
;;  "t if an Eiffel quoted string begins, ends, or is continued
;;   on current line."
;;  (save-excursion
;;    (beginning-of-line)
;;    ;; Line must either start with optional whitespace immediately followed
;;    ;; by a '%' or include a '\"'.  It must either end with a '%' character
;;    ;; or must include a second '\"' character.
;;    (looking-at "^\\([ \t]*%\\|[^\"\n]*\"\\)[^\"\n]*\\(%$\\|\"\\)")))

(defconst eif-opening-regexp
  "\\<\\(external\\|check\\|deferred\\|do\\|once\\|from\\|if\\|inspect\\|debug\\)\\>"
  "Keywords that open eiffel nesting constructs.")
;; OK, this is a horrible hack in all of this to handle "once" as a
;; special case because it has been overloaded.  The search for the
;; opening keyword on the current line is quite reasonably limited to
;; the current line.  Therefore, the standard hacky way that we avoid
;; matching once strings, by making sure they're followed by
;; whitespace and a non-double-quote, doesn't work here.
(defconst eif-non-opening-regexp
  "\\<once\\s-+\""
  "Pattern matching exclusions from `eif-opening-regexp'.")
(defconst eif-closing-regexp "\\<end\\>"
  "Keywords that close eiffel nesting constructs.")
(defconst eif-do-regexp "\\<\\(do\\|once\\|external\\)\\>"
  "Keyword that opens eiffel routine body.")
(defconst eif-opening-or-closing-regexp
  (concat "\\(" eif-opening-regexp "\\|" eif-closing-regexp "\\)")
  "Keywords that open or close eiffel nesting constructs.")

;;
;; Code to allow indenting whole eiffel blocks
;;

(defun eif-matching-line (&optional return-line-break direction)
  "Return the position of the keyword matching the one on the current line.
For example, a line containing the keyword `do' is matched by a line
containing the keyword `end' and a line containing `end' may be
matched by a number of opening keywords.  If the optional parameter
RETURN-LINE-BREAK is non-nil, the character position returned is the
beginning (or end) of the line containing the matching keyword instead
of the position of the keyword itself.  If the second optional
parameter, DIRECTION, is non-nil, the current line is not searched for
a keyword.  Instead, if the value of direction is 'forward, the
function acts as if an `eif-opening-regexp' is on the current line.
If the value of direction is 'backward, the function acts as if a
`eif-closing-regexp' is on the current line.  The effect of using the
direction parameter is to locate either the opening or closing keyword
of the syntactic construct containing the point."
  (let ((nesting-level 0)
	(search-end 0)
	matching-point opening-keyword match-start match-end
	success start-point)
    (unwind-protect
	(save-excursion
	  (modify-syntax-entry ?_  "w  ")
	  (setq eif-matching-kw-for-end "");; public variable set by this function
	  (setq start-point (point))
	  (end-of-line)
	  (setq search-end (point))
	  (beginning-of-line)
	  ;; Set starting state: If direction was specified use it.
	  ;; If direction is nil, search for a keyword on the current line
	  ;; If the keyword is in eif-opening-regexp, set the search
	  ;; direction to 'forward, if the keyword on the current line is `end'
	  ;; set the search direction to 'backward.
	  (cond ((eq direction 'forward)
		 (end-of-line)       ;; So we wont see keywords on this line.
		 (setq nesting-level 1))
		((eq direction 'backward)
		 (beginning-of-line) ;; So we wont see keywords on this line.
		 (setq nesting-level -1))
		((and (re-search-forward eif-opening-regexp search-end t)
		      (eif-not-in-comment-or-quoted-string-p))
		 (setq match-start (match-beginning 0))
		 (setq match-end (match-end 0))
		 (goto-char match-start)
		 (if (and (not (looking-at eif-non-opening-regexp))
			  (eif-not-in-comment-or-quoted-string-p))
		     (setq nesting-level 1))
		 (setq opening-keyword
		       (cons (buffer-substring match-start match-end)
			     opening-keyword))
		 (goto-char match-end))
		((and (progn (beginning-of-line) t)
		      (re-search-forward eif-closing-regexp search-end t)
		      (eif-not-in-comment-or-quoted-string-p))
		 (goto-char (match-beginning 0))
		 (if (eif-not-in-comment-or-quoted-string-p)
		     (setq nesting-level -1))))
	  ;; Perform the search
	  (while (not (= nesting-level 0))
	    (if (> nesting-level 0)
		;; Then search forward for the next keyword not in a comment
		(while (and (re-search-forward eif-opening-or-closing-regexp nil 1)
			    (goto-char (setq match-start (match-beginning 0)))
			    (setq match-end   (match-end 0))
			    (setq success t)
			    (or (looking-at eif-non-opening-regexp)
				(eif-in-comment-or-quoted-string-p)))
		  (goto-char match-end)
		  (setq success nil))
	      ;; Else search backward for the next keyword not in a comment
	      (while (and (re-search-backward eif-opening-or-closing-regexp nil 1)
			  (goto-char (setq match-start (match-beginning 0)))
			  (setq success t)
			  (or (looking-at eif-non-opening-regexp)
			      (eif-in-comment-or-quoted-string-p)))
		(setq success nil)))
	    (cond ((and (not (looking-at eif-non-opening-regexp))
			(looking-at eif-opening-regexp)
			success)
		   ;; Found an opening keyword
		   (if (> nesting-level 0)
		       ;; Then
		       (if (looking-at eif-do-regexp)
			   ;; Then
			   (setq nesting-level -1)
			 ;; Else
			 (setq opening-keyword
			       (cons (buffer-substring match-start
						       (match-end 0))
				     opening-keyword))
			 (goto-char (match-end 0)))
		     ;; Else
		     (if (= nesting-level -1)
			 ;; Then
			 (progn
			   (setq eif-matching-kw-for-end
				 (buffer-substring match-start (match-end 0)))
			   (if (looking-at "[ \t\n]+")
			       (goto-char (match-end 0))))
		       ;; Else
		       (if (looking-at eif-do-regexp)
			   ;; Then
			   (progn
			     (goto-char (eif-matching-line nil 'forward))
			     (setq nesting-level -1))))
		     (setq opening-keyword (cdr opening-keyword))
		     (if return-line-break
			 (beginning-of-line)))
		   (setq nesting-level (1+ nesting-level)))
		  ((and (looking-at eif-closing-regexp) success)
		   ;; Found an opening keyword
		   (if (> nesting-level 0)
		       ;; Then
		       (progn
			 (setq opening-keyword (cdr opening-keyword))
			 (if return-line-break
			     (end-of-line))
			 (goto-char (match-end 0)))
		     ;; Else
		     (setq opening-keyword
			   (cons (buffer-substring (match-beginning 0)
						   (match-end 0))
				 opening-keyword)))
		   (setq nesting-level (1- nesting-level)))
		  (t (message (concat "Could not find match"
				      (if (car opening-keyword)
					  (concat " for: "
						  (car opening-keyword)))))
		     (goto-char start-point)
		     (setq nesting-level 0))))
	  (setq matching-point (point)))
      (modify-syntax-entry ?_  "_  "))
    (set-mark matching-point)))

;; ENHANCEME: Make this function correctly indent more than just routine
;;            bodies and their sub-constructs.  At the least it should
;;            handle whole routines also.
(defun eif-indent-construct ()
  "Indent an entire eiffel syntactic construct.
It is assumed that the point is within a nesting construct ('do',
`once', 'check', 'if', 'from', or 'inspect').  The whole construct is
indented up to the matching end.  If the point is not within such a
construct, then only that line is indented"
  (interactive)
  (let ((end-point 0))
    (save-excursion
      (end-of-line)
      (if (not (= (point) (point-max))) (forward-char 1))
      (goto-char (eif-matching-line t 'backward))
      (setq end-point (eif-matching-line t 'forward))
      (while (< (point) end-point)
	(eif-indent-line)
	(forward-line 1)
	(beginning-of-line)))))

(defun eif-indent-region (&optional start end)
  "Indent the lines in the current region.
The region may be specified using optional arguments START and END."
  (interactive)
  (let ((start-point (or start (region-beginning)))
	(end-point   (copy-marker (or end (region-end)))))
    (save-excursion
      (goto-char start-point)
      (cond ((eq major-mode 'eiffel-mode)
	     (while (< (point) end-point)
	       (if (not (looking-at "[ \t]*$"))
		   (eif-indent-line))
	       (forward-line 1)
	       (beginning-of-line)))
	    (t (error "Buffer must be in eiffel mode"))))))

;;(defun eif-goto-matching-line (&optional direction)
;;  "Place the cursor on the line which closes(opens) the current
;;opening(closing) syntactic construct.  For example if the point
;;is on `from', executing goto-matching-line places the point
;;on the matching `end' and vice-versa."
;;  (interactive)
;;  (if (not direction)
;;      (progn
;;	(cond ((save-excursion (beginning-of-line) (looking-at "[ \t]*end.*$"))
;;	       (goto-char (eif-matching-line nil 'backward)))
;;	      ((looking-at "(")
;;	       (forward-sexp))
;;	      ((save-excursion (backward-char 1) (looking-at ")"))
;;	       (backward-sexp))
;;	      (t
;;	       (goto-char (eif-matching-line nil 'forward)))))))

(defun eif-forward-sexp ()
  "Put cursor on line that closes the current opening syntactic construct.
For example, if the point is on `from' then the point is placed on the
matching `end'.  This also does matching of parens ala
\\[forward-sexp]."
  (interactive)
  (cond ((looking-at "[[(]")
	 (forward-sexp))
	(t
	 (goto-char (eif-matching-line nil 'forward)))))

(defun eif-backward-sexp ()
  "Put cursor on line that opens the current closing syntactic construct.
For example, if the point is on the terminating `end' of an `if'
statement, then the point is place on the opening `if'.  This also
does matching of parens ala \\[backward-sexp]'."
  (interactive)
  (cond ((eif-peeking-backwards-at "[])]")
	 (backward-sexp))
	(t
	 (goto-char (eif-matching-line nil 'backward)))))

(defun eif-local-indent (amount)
  "Set the value of `eif-indent-increment' to AMOUNT buffer-locally."
  (interactive "NNumber of spaces for eif-indent-increment: ")
  (make-local-variable 'eif-indent-increment)
  (setq eif-indent-increment amount))

;; ----------------------------------------------------------------------
;; This next portion of the file is derived from "eiffel.el"
;; Copyright (C) 1989, 1990 Free Software Foundation, Inc. and Bob Weiner
;; Available for use and distribution under the same terms as GNU Emacs.
;; ----------------------------------------------------------------------

(defvar eiffel-mode-map nil
  "Keymap for Eiffel mode.")

(if eiffel-mode-map
    nil
  (let ((map (make-sparse-keymap)))
    (define-key map [(control j)]       'newline-and-indent)
    (define-key map [(return)]          'reindent-then-newline-and-indent)
    (define-key map [(meta control q)]  'eif-indent-construct)
    (define-key map [(meta \')]         'eif-feature-quote)
    (define-key map [(meta q)]          'eif-fill-paragraph)
    (define-key map [(meta control a)]  'eif-beginning-of-feature)
    (define-key map [(meta control e)]  'eif-end-of-feature)
    (define-key map [(control x) ?n ?d] 'eif-narrow-to-feature)
    (setq eiffel-mode-map map)))

(defvar eiffel-mode-syntax-table nil
  "Syntax table in use in Eiffel-mode buffers.")

(if eiffel-mode-syntax-table
    nil
  (let ((table (make-syntax-table))
	(i 0))
    (while (< i ?0)
      (modify-syntax-entry i "_   " table)
      (setq i (1+ i)))
    (setq i (1+ ?9))
    (while (< i ?A)
      (modify-syntax-entry i "_   " table)
      (setq i (1+ i)))
    (setq i (1+ ?Z))
    (while (< i ?a)
      (modify-syntax-entry i "_   " table)
      (setq i (1+ i)))
    (setq i (1+ ?z))
    (while (< i 128)
      (modify-syntax-entry i "_   " table)
      (setq i (1+ i)))
    (modify-syntax-entry ?  "    " table)
    (modify-syntax-entry ?-  ". 12" table)
    (modify-syntax-entry ?_  "_  " table)
    (modify-syntax-entry ?\t "    " table)
    (modify-syntax-entry ?\n ">   " table)
    (modify-syntax-entry ?\f ">   " table)
    (modify-syntax-entry ?\" "\"    " table)
    (modify-syntax-entry ?\\ "." table)
    (modify-syntax-entry ?\( "()  " table)
    (modify-syntax-entry ?\) ")(  " table)
    (modify-syntax-entry ?\[ "(]  " table)
    (modify-syntax-entry ?\] ")[  " table)
    (modify-syntax-entry ?\{ "(}  " table)
    (modify-syntax-entry ?\} "){  " table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?` "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "\\" table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?\; "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?! "." table)
    (modify-syntax-entry ?. "." table)
    (setq eiffel-mode-syntax-table table)))

(defun eif-add-menu ()
  "Add the \"Eiffel\" menu to the menu bar."

  (easy-menu-define
   eiffel-mode-menu
   eiffel-mode-map
   "Menu for eiffel-mode."
   (append (list "Eiffel")
	   (if eif-use-gnu-eiffel
	       (list
		["Compile..."            eif-compile t]
		["Compiler Options..."   eif-set-compile-options t]
		["Next Compile Error..." next-error  t]
		["Run..."                eif-run     t]
		["Debug..."              eif-debug   t]
		["Short..."              eif-short   t]
		["----------" nil nil]))
	   (list
	    ["Indent Construct"    eif-indent-construct t]
	    ["----------" nil nil]
	    (list "Imenu"
		  ["By position"   eif-imenu-add-menubar-by-position t]
		  ["By name"       eif-imenu-add-menubar-by-name     t])
	    (list "Comments"
		  ["Feature Quote" eif-feature-quote  (eif-in-comment-p)]
		  ["Fill         " eif-fill-paragraph (eif-near-comment-p)])
	    ["----------" nil nil]
            ["Customize"           eif-customize     t])))
  (easy-menu-add eiffel-mode-menu))

;;;###autoload
(defun eiffel-mode ()
  "Major mode for editing Eiffel programs.
\\[indent-for-tab-command] indents the current Eiffel line correctly and
\\[reindent-then-newline-and-indent] causes the current and next line to be
properly indented.

Key definitions:
\\{eiffel-mode-map}

If variable `eif-use-gnu-eiffel' is non-nil (default t) then support
for using GNU SmartEiffel is enabled.  Run \\[eif-customize] to see
compilation and indentation variables that can be customized."

  (interactive)

  (kill-all-local-variables)

  (setq major-mode 'eiffel-mode)
  (setq mode-name "Eiffel")

  (if eif-use-gnu-eiffel
      (progn
	(define-key eiffel-mode-map "\C-c\C-c" 'eif-compile)
	(define-key eiffel-mode-map "\C-c\C-o" 'eif-set-compile-options)
	(define-key eiffel-mode-map "\C-c\C-r" 'eif-run)
	(define-key eiffel-mode-map "\C-c\C-d" 'eif-debug)
	(define-key eiffel-mode-map "\C-c\C-s" 'eif-short))
    (define-key eiffel-mode-map "\C-c\C-c" nil)
    (define-key eiffel-mode-map "\C-c\C-o" nil)
    (define-key eiffel-mode-map "\C-c\C-r" nil)
    (define-key eiffel-mode-map "\C-c\C-s" nil))

  (use-local-map eiffel-mode-map)
  (eif-add-menu)
  (set-syntax-table eiffel-mode-syntax-table)

  ;; Make local variables.
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'indent-region-function)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'imenu-create-index-function)
  ;; Now set their values.
  (setq paragraph-start              (concat "^$\\|" page-delimiter)
	paragraph-separate           paragraph-start
	paragraph-ignore-fill-prefix t
	require-final-newline        'ask
	parse-sexp-ignore-comments   t
	indent-line-function         'eif-indent-line
	indent-region-function       'eif-indent-region
	comment-start                "-- "
	comment-end                  ""
	comment-column               32
	comment-start-skip           eiffel-comment-start-skip
	font-lock-defaults           eiffel-font-lock-defaults)

  (if eif-set-tab-width-flag
      (setq tab-width eif-indent-increment))

  (setq auto-fill-function 'eif-auto-fill)
  (run-hooks 'eiffel-mode-hook))

(defconst eif-prefeature-regexp
  (concat "\\(" eif-non-source-line "\\|\n\\)*" "[ \t]*")
  "Regexp matching whitespace-equivalent content, possibly before a feature.")

(defun eif-find-end-of-feature ()
  "Find the `end' of the current feature definition.
Assumes point is at the beginning of the feature, not in a comment or
quoted string."
  (let (ret)
    (modify-syntax-entry ?_  "w  ")
    (cond ((looking-at (concat eif-prefeature-regexp
			       eif-routine-begin-regexp))
	   ;; At the start of a routine, find matching end.
	   (and (eif-re-search-forward eif-do-regexp nil t)
		(goto-char (match-beginning 0))
		(goto-char (setq ret (eif-matching-line)))))
	  ((looking-at (concat eif-prefeature-regexp
			       eif-probably-feature-regexp))
	   ;; Not a routine, find end of attribute or constant.
	   (goto-char (setq ret (match-end 0)))))
    (modify-syntax-entry ?_  "_  ")
    ret))    

;; OK, this works well, but it doesn't work for the following cases:
;; * In the middle of the feature regexp that need to be matched.
;;   However, it doesn't need to since eif-beginning-of-feature adds
;;   some smarts around it...
(defun eif-find-beginning-of-feature ()
  "Find the beginning of the most recent feature definition.
This will always move backward, if possible."
  (interactive)
  
  (let ((start (point))
	candidate routine-begin)
    (if (eif-re-search-backward (concat "\\s-" eif-probably-feature-regexp)
				nil t)
	(progn
	  (forward-char) ;; Skip the whitespace character matched above.
	  (if (not (or (looking-at (concat
				    "\\(" eif-attribute-regexp
				    "\\|" eif-constant-regexp "\\)"))))
	      ;; This is a routine.  Done.
	      (point)
	    ;; Variable/attribute or constant declaration matched.
	    ;; Now we go back and find the previous routine start, the
	    ;; following end, and see if the current position
	    ;; (candidate) is between.  If it is, then candidate is a
	    ;; variable or constant declaration within a routine, so
	    ;; we're interested in the routine start.  If it isn't,
	    ;; then it must be a class attribute or constant, so it is
	    ;; what we're looking for.
	    (setq candidate (point))
	    (goto-char start)
	    (if (eif-re-search-backward
		 (concat "\\s-" eif-routine-begin-regexp) nil t)
		(progn 
		  (forward-char)
		  (setq routine-begin (point))
		  (eif-find-end-of-feature)
		  (if (and (< routine-begin candidate)
			   (< candidate (point)))
		      (goto-char routine-begin)
		    (goto-char candidate)))
	      (goto-char candidate)))))))

(defun eif-beginning-of-feature (&optional arg)
  "Move backward to next feature beginning.
With ARG, do it that many times.  Negative arg -N
means move forward to Nth following beginning of feature.
Returns t unless search stops due to beginning or end of buffer."
  (interactive "p")

  (or arg
      (setq arg 1))

  (let ((start (point))
	(success t))
    (cond ((> arg 0)
	   ;; Going backward.

	   ;; We have to move forward to make sure we find any feature
	   ;; that we might be in the middle of the beginning of.  How
	   ;; far?  How about this far?
	   (eif-re-search-forward eif-probably-feature-regexp nil 'move)

	   ;; Change arg towards zero as we search, failing if we hit
	   ;; edge of buffer.
	   (while (and (> arg 0)
		       (or (eif-find-beginning-of-feature)
			   (setq success nil)))
	     ;; If we've gone backwards from the original start, then
	     ;; this counts.
	     (if (< (point) start)
		 (setq arg (1- arg))))
	   (or success
	       (goto-char (point-min))))

	  ((< arg 0)
	   ;; Going forward.
	   
	   ;; Similar to above, let's go back to the beginning of the
	   ;; current feature, and then skip over features and find
	   ;; the beginning of the next repeatedly.
	   (eif-find-beginning-of-feature)
	   
	   (while (and (< arg 0)
		       (or (not (eobp)) (setq success nil)))
	     (eif-find-end-of-feature)
	     (if (eif-re-search-forward eif-probably-feature-regexp
					nil 'move)
		 (progn
		   (goto-char (match-beginning 0))
		   (if (> (point) start)
		       (setq arg (1+ arg))))))))
    success))
	   
(defun eif-end-of-feature (&optional arg)
  "Move forward to end of feature.
With argument, do it that many times.  Negative argument means move
back ARG preceding ends of features."
  (interactive "p")

  ;; Default is to find the first feature's end.
  ;; Huh?  Even if they specify 0?  - martin@meltin.net
  ;; Hmmm, it is what end-of-defun does...
  (if (or (null arg)
	  (= arg 0))
      (setq arg 1))

  ;; This is a bad way of trying to get into position.  Happily, it
  ;; seems to work.  Hmmm, not sure if the comment skip is needed.
  (if (eif-in-comment-p)
      (end-of-line))
  (cond ((let ((curr (point)))
	   (save-excursion
	     (and (eif-beginning-of-feature)
		  (eif-find-end-of-feature)
		  (forward-line)
		  (or (< curr (point))
		      (and (< arg 0)
			   (= curr (point)))))))
	 ;; Within a feature.  Go to its beginning.
	 (eif-beginning-of-feature))
	((eif-peeking-backwards-at (concat "\\s-"
					   eif-probably-feature-regexp))
	 ;; Sitting at beginning of feature.  Don't move!
	 t)
	(t
	 ;; Not within a feature or at beginning, go to beginning of
	 ;; next feature.
	 (eif-beginning-of-feature -1)))

  ;; This part is correct.
  (if (eif-beginning-of-feature (+ (if (< arg 0) 0 1) (- arg)))
      (progn
	(eif-find-end-of-feature)
	(forward-line))))

(defun eif-narrow-to-feature ()
  "Make text outside current feature invisible.
The feature visible is the one that contains point or follows point."
  (interactive)
  (save-excursion
    (widen)
    (eif-end-of-feature)
    (let ((end (point)))
      (eif-beginning-of-feature)
      (narrow-to-region (point) end))))

(defun eif-current-line-indent ()
  "Return the indentation of the line containing the point."
  (save-excursion
    (eif-skip-leading-whitespace)
    (current-column)))

(defun eif-in-quoted-string-p (&optional non-strict-p)
  "Return t if point is in a quoted string.
Optional argument NON-STRICT-P if true causes the function to return
true even if the point is located in leading white space on a
continuation line.  Normally leading white space is not considered part
of the string."
  (let ((initial-regexp "^[ \t]*%\\|[^%]\"\\|%[ \t]*$")
	(search-limit (point))
	(count 0))
    (save-excursion
      ;; Line must either start with optional whitespace immediately followed
      ;; by a '%' or include a '\"' before the search-limit.
      (beginning-of-line)
      (while (re-search-forward initial-regexp search-limit t)
	(setq count (1+ count))
	(if (= count 1) (setq search-limit (1+ search-limit))))
      ;; If the number of quotes (including continuation line markers)
      ;; is odd, then we are inside of a string.  Also if non-strict-p
      ;; and we are in the leading white space of a continuation line,
      ;; then we are in a quote.
      (or (= (% count 2) 1)
	  (progn
	    (beginning-of-line)
	    (and non-strict-p
		 (looking-at "^[ \t]*%")))))))

;; ----------------------------------------------------------------------
;; End of portion derived from "eiffel.el"
;; ----------------------------------------------------------------------

(defun eif-comment-prefix ()
  "Return the prefix starting a comment that begins a line.
Comments that are not the only thing on a line return nil as their prefix."
  (save-excursion
    (end-of-line)
    (let ((limit (point)) len
	  (in-string (eif-in-quoted-string-p)))
      (beginning-of-line)
      (cond ((re-search-forward "^[ \t]*--|?[ \t]*" limit t)
	     (buffer-substring (match-beginning 0) (match-end 0)))
	    ;; Handle string-literal continuation lines
	    (in-string
	     (end-of-line)
	     (re-search-backward "^[ \t]*%\\|[^%]\"" nil t)
	     (re-search-forward "%\\|\"" nil t)
	     (setq len (1- (current-column)))
	     (concat (make-string len ? ) "%"))))))

(defun eif-auto-fill ()
  "Auto-fill an Eiffel comment."
  (let ((fill-prefix (eif-comment-prefix))
	(pm (point-marker)))
    (if (and (> (current-column) fill-column)
	     fill-prefix)
	(if (string-match "^[ \t]*%" fill-prefix)
	    (progn
	      (backward-char 1)
	      (re-search-backward "[^][a-zA-Z0-9]" nil t)
	      (forward-char 1)
	      (insert "%\n")
	      (insert fill-prefix)
	      (goto-char pm))
	  ;; (do-auto-fill)
	  (backward-char 1)
	  (re-search-backward "\\s-" nil t)
	  (forward-char 1)
	  (insert "\n")
	  (insert fill-prefix)
	  (goto-char pm)))))

(defun eif-fill-paragraph ()
  "Textually fills Eiffel comments ala \\[fill-paragraph]."
  (interactive)
  (save-excursion
    (let ((current-point (point))
	  (fill-prefix (eif-comment-prefix))
	  last-point para-begin para-end)
      (if fill-prefix
	  (progn
	    (setq last-point (point))
	    (forward-line -1)
	    (end-of-line)
	    (while (and (not (= (point) last-point))
			(eif-comment-prefix))
	      (setq last-point (point))
	      (forward-line -1)
	      (end-of-line))
	    (if (= (point) last-point)
		(setq para-begin (save-excursion (beginning-of-line) (point)))
	      (setq para-begin (1+ (point))))
	    (goto-char current-point)
	    (setq last-point (point))
	    (forward-line 1)
	    (end-of-line)
	    (while (and (not (= (point) last-point))
			(eif-comment-prefix))
	      (setq last-point (point))
	      (forward-line 1)
	      (end-of-line))
	    (if (= (point) last-point)
		(setq para-end (point))
	      (beginning-of-line)
	      (setq para-end (point)))
	    ;; Avert eyes now - gross hack follows...  how big can an
	    ;; Eiffel comment be anyway?  :-)
	    (let ((orig-region (and (<= (- para-end para-begin)
					eif-fill-max-save)
				    (buffer-substring para-begin para-end)))
		  (orig-state  (buffer-modified-p))
		  (ret  (fill-region para-begin para-end)))
	      (and orig-region
		   (<= para-end (point-max))
		   (string-equal
		    orig-region (buffer-substring para-begin para-end))
		   (set-buffer-modified-p orig-state))
	      ret))))))

(defun eif-indent-line (&optional whole-exp)
  "Indent the current line as Eiffel code.
With optional argument WHOLE-EXP, indent any additional lines of the
same clause rigidly along with this one (not implemented yet)."
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (let ((indent (eif-calc-indent)))
      (if (not (= indent (current-column)))
	  (progn
	    (delete-horizontal-space)
	    (indent-to indent)))))
  (skip-chars-forward " \t"))

(defun eif-move-to-prev-non-blank ()
  "Move point to previous line excluding blank lines.
Return t if successful, nil if not."
  (beginning-of-line)
  (re-search-backward "^[ \t]*[^ \t\n]" nil t))

(defvar eif-last-feature-level-indent -1)
(defvar eif-feature-level-indent-regexp nil)
(defun eif-in-paren-expression ()
  "Determine if we are inside of a parenthesized expression."
  (interactive)
  (let ((paren-count 0) (limit 0))
    (save-excursion
      (if (= eif-last-feature-level-indent (eif-feature-level-indent-m))
	  (setq limit
		(re-search-backward eif-feature-level-indent-regexp nil t))
	(setq eif-last-feature-level-indent (eif-feature-level-indent-m))
	(setq eif-feature-level-indent-regexp
	      (concat "^" (make-string eif-last-feature-level-indent ? )
		      "[^ \t\n]"))
	(setq limit
	      (or (re-search-backward eif-feature-level-indent-regexp nil t)
		  0))))
    (save-excursion
      (while (re-search-backward "[][()]" limit t)
	(if (looking-at "[[(]")
	    (setq paren-count (1+ paren-count))
	  (setq paren-count (1- paren-count)))))
    paren-count))

(defun eif-manifest-array-common ()
  "Common code for handling indentation/presence of Eiffel manifest arrays."
  (let ((paren-count 0))
    (if (= eif-last-feature-level-indent (eif-feature-level-indent-m))
	nil
      (setq eif-last-feature-level-indent (eif-feature-level-indent-m))
      (setq eif-feature-level-indent-regexp
	    (concat "^" (make-string eif-last-feature-level-indent ? )
		    "[^ \t\n]")))
    (while (and (<= paren-count 0) (re-search-backward "<<\\|>>" nil t))
      (if (not (eif-peeking-backwards-at "|\\|@"))
	  (if (looking-at "<<")
	      (setq paren-count (1+ paren-count))
	    (setq paren-count (1- paren-count)))))
    paren-count))

(defun eif-manifest-array-indent ()
  "Determine if we are inside of a manifest array."
  (interactive)
  (let (indent)
    (save-excursion
      (if (> (eif-manifest-array-common) 0)
	  (let ((eol (save-excursion (end-of-line) (point))))
	    (setq indent
		  (or (and (re-search-forward "[^< \t]" eol t)
			   (1- (current-column)))
		      (+ (current-column) 2))))))
    indent))

(defun eif-manifest-array-start ()
  "Determine the indentation of the statement containing a manifest array."
  (interactive)
  (let (indent)
    (save-excursion
      (if (> (eif-manifest-array-common) 0)
	  (let ((limit (progn (end-of-line) (point))))
	    (beginning-of-line)
	    (if (re-search-forward "^[ \t]*<<" limit t)
		(setq indent (- (current-column) 2 eif-indent-increment))
	      (re-search-forward "^[ \t]*" limit t)
	      (setq indent (current-column))))))
    indent))

;; ----------------------------------------------------------------------
;; The function below is derived from "eif-mult-fmt.el"
;; Copyright (C) 1985 Free Software Foundation, Inc.
;; Copyright (C) 1990 Bob Weiner, Motorola Inc.
;; Available for use and distribution under the same terms as GNU Emacs.
;; ----------------------------------------------------------------------

(defun eif-indent-multi-line (&optional parse-start)
  "Return indentation for line within parentheses or double quotes.
That is, we are in a multi-line parenthesised or double-quoted
expression, and want to know the suggested indentation for the current
line.  If we are not within such an expression then return -1.
Optional argument PARSE-START is buffer position at which to begin
parsing, default is to begin at the feature enclosing or preceding
point."
  (let ((eif-opoint (point))
	(indent-point (progn (beginning-of-line) (point)))
	(eif-ind-val -1)
	(eif-in-str nil)
	(eif-paren-depth 0)
	(retry t)
	state
	;; setting this to a number inhibits calling hook
	last-sexp containing-sexp)
    (if parse-start
	(goto-char parse-start)
      (eif-beginning-of-feature))
    ;; Find outermost containing sexp
    (while (< (point) indent-point)
      (setq state (parse-partial-sexp (point) indent-point 0)))
    ;; Find innermost containing sexp
    (while (and retry
		state
		(> (setq eif-paren-depth (elt state 0)) 0))
      (setq retry nil)
      (setq last-sexp (elt state 2))
      (setq containing-sexp (elt state 1))
      ;; Position following last unclosed open.
      (goto-char (1+ containing-sexp))
      ;; Is there a complete sexp since then?
      (if (and last-sexp (> last-sexp (point)))
	  ;; Yes, but is there a containing sexp after that?
	  (let ((peek (parse-partial-sexp last-sexp indent-point 0)))
	    (if (setq retry (car (cdr peek))) (setq state peek)))))
    (if retry
	nil
      ;; Innermost containing sexp found
      (goto-char (1+ containing-sexp))
      (if (not last-sexp)
	  ;; indent-point immediately follows open paren.
	  nil
	;; Find the start of first element of containing sexp.
	(parse-partial-sexp (point) last-sexp 0 t)
	(cond ((looking-at "\\s(")
	       ;; First element of containing sexp is a list.
	       ;; Indent under that list.
	       )
	      ((> (save-excursion (forward-line 1) (point))
		  last-sexp)
	       ;; This is the first line to start within the containing sexp.
	       (backward-prefix-chars))
	      (t
	       ;; Indent beneath first sexp on same line as last-sexp.
	       ;; Again, it's almost certainly a routine call.
	       (goto-char last-sexp)
	       (beginning-of-line)
	       (parse-partial-sexp (point) last-sexp 0 t)
	       (backward-prefix-chars))))
      (setq eif-ind-val (current-column)))
    ;; Point is at the point to indent under unless we are inside a string.
    (setq eif-in-str (elt state 3))
    (goto-char eif-opoint)
    (if (not eif-in-str)
	nil
      ;; Inside a string, indent 1 past string start
      (setq eif-paren-depth 1);; To account for being inside string
      (save-excursion
	(if (re-search-backward "\"" nil t)
	    (if eif-indent-string-continuations-relatively-flag
		(setq eif-ind-val (1+ (current-column)))
	      (setq eif-ind-val (eif-current-line-indent)))
	  (goto-char indent-point)
	  (if (looking-at "^[ \t]*[^ \t\n]")
	      (eif-move-to-prev-non-blank))
	  (skip-chars-forward " \t")
	  (setq eif-ind-val (current-column)))))
    (if (> eif-paren-depth 0) eif-ind-val -1)))

;; ----------------------------------------------------------------------
;; imenu support, great for browsing foreign code.
;; Originally contributed by Berend de Boer <berend@pobox.com>.
;; ----------------------------------------------------------------------

(defun eif-imenu-add-menubar-by-position ()
  "Add menu of features of a class, sorted in order of occurence."
  (interactive)
  (setq imenu-create-index-function  'eif-imenu-create-index-by-position)
  (imenu-add-to-menubar "Eiffel features")
  )

(defun eif-imenu-add-menubar-by-name ()
  "Add menu of features of a class, sorted by name."
  (interactive)
  (setq imenu-create-index-function  'eif-imenu-create-index-by-name)
  (imenu-add-to-menubar "Eiffel names"))

(defun eif-imenu-create-index-by-position ()
  "Generate index of features of a class, sorted in order of occurence."
  (eif-imenu-create-index 0))

(defun eif-imenu-create-index-by-name ()
  "Generate index of features of a class, sorted by name."
  (eif-imenu-create-index 1))

(defun eif-imenu-create-index (sort-method)
  "Generate an index of all features of a class.
Sort by position if sort-method is 0. Sort by name if sort-method is 1."

  (let (menu prevpos)

    (imenu-progress-message prevpos 0 t)

    ;; scan for features
    (goto-char (point-max))
    (while (eif-find-beginning-of-feature)
      (imenu-progress-message prevpos nil t)
      (if (looking-at "\\(\\sw\\|\\s_\\)+")
	  (add-to-list 'menu (cons (buffer-substring-no-properties
				    (match-beginning 0)
				    (match-end 0)) (point)))))

    (imenu-progress-message prevpos 100)

    ;; sort in increasing buffer position order or by name
    (if (= sort-method 0)
        (sort menu (function (lambda (a b) (< (cdr a) (cdr b)))))
      (sort menu (function (lambda (a b) (string< (car a) (car b))))))))

;; XEmacs addition
;;;###autoload(add-to-list 'auto-mode-alist '("\\.e\\'" . eiffel-mode))

(provide 'eiffel)
;;; eiffel.el ends here

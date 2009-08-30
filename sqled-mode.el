;;; sqled-mode.el --- Major Mode for editing sql, sqlplus and pl/sql code
;;;
;;; Requires custom, easy-menu
;;;

;; Copyright (C) 2003  Josie Stauffer
          
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
          
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
          
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA


;;; FILE SECTIONS
;;; ============
;;;
;;;      ** 1. USER OPTIONS
;;;      ** 2. SQLED-MODE
;;;      ** 3. AUTO CASING AND INDENTATION FUNCTIONS
;;;      ** 4. INTERACTIVE COMMENT FUNCTIONS
;;;      ** 5. BULK CASING AND INDENTATION FUNCTIONS
;;;      ** 6. FUNCTIONS FOR USE ON DESC-TABLE OUTPUT
;;;      ** 7. MISCELLANEOUS INTERACTIVE FUNCTIONS
;;;      ** 8. NON-INTERACTIVE UTILITY FUNCTIONS
;;;      ** 9. CONTEXT INFORMATION


;;; USAGE
;;; =====
;;;
;;; If the following lines are contained in your .emacs file:
;;;
;;;(load-file "..path.../sqled-mode.elc")
;;;
;;;(setq auto-mode-alist
;;;      (append '(("\\.sql\\'" . sqled-mode)) auto-mode-alist))
;;;
;;; then Emacs should enter sqled-mode when you load a file whose name
;;; ends in ".sql"
;;;
;;; When you have entered sqled-mode, you may get more info by pressing
;;; C-h m. You may also get online help describing various functions by:
;;; C-h d <Name of function you want described>


;;; KNOWN BUGS
;;; ==========
;;;
;;; Does not recognize multiline /* ..
;;;                                  .. */ as comments
;;; although /*........*/ in one line is recognized
;;;
;;; Does not recognize multiline strings
;;; A workaround for this is to write the string
;;;
;;; 'linestart
;;; linecontinuation'
;;;
;;; as
;;; 'linestart'||chr(10)||
;;; 'linecontinuation'
;;;

;;;;                ----------------------
;;;;                 ** 1. USER OPTIONS **
;;;;                ----------------------


;;; Commentary:
;; 


;;; History:
;; 

(require 'custom)
;;; Code:
(defgroup sqled nil
  "Major mode for editing SQL, SQLPLUS and PL/SQL code."
  :group 'languages)

;;;=============================================================
;;; A. Miscellaneous options
;;;=============================================================

(defcustom sqled-mode-hook nil
  "*List of functions to call when SQLED Mode is invoked.
This is a good place to add SQLED environment specific bindings."
  :type 'hook
  :group 'sqled)


(defcustom sqled-comment-prefix "-- "
  "*Prefix used by some `comment-fill` commands.
`comment-region' and `fill-comment-paragraph' insert this
string at the start of each line.
It is NOT the expression for recognizing the start of a comment
\( see `sqled-comment-start-re' )."
  :type 'string
  :group 'sqled)

(defcustom sqled-leftcomment-start-re "\\-\\-\\-\\|/\\*"
  "*Regular expression for the start of a left-aligned comment.
`comment-region' and `fill-comment-paragraph' insert this
string at the start of each line.
It is NOT the expression for recognizing the start of a comment
\( see `sqled-comment-start-re' )."
  :type 'string
  :group 'sqled)

(defcustom sqled-fill-comment-prefix "/* "
  "*Prefix used by some `comment-fill` commands.
`sqled-fill-comment-paragraph-prefix' and
`sqled-fill-comment-paragraph-postfix' insert this
string at the start of each line.
It is NOT the expression for recognizing the start of a comment
\( see `sqled-comment-start-re' )."
  :type 'string
  :group 'sqled)

(defcustom sqled-fill-comment-postfix " */"
  "*Postfix used by `sqled-fill-comment-paragraph-postfix'.
This string is inserted at the end of each line by
`sqled-fill-comment-paragraph-postfix'."
  :type 'string
  :group 'sqled)

(defcustom sqled-fill-comment-postfix-re "[ \t]*\\*/"
  "*Postfix recognized by some `fill-comment` commands.
A string matching this expression is deleted from the end
of each line when filling a comment paragraph using
`sqled-fill-comment-paragraph-prefix`."
  :type 'string
  :group 'sqled)

(defcustom sqled-comment-start-re
  "\\(^\\(rem \\|remark \\)\\|--\\|/\\*\\)"
  "*Regular expression for the start of a comment.
\(ie text to be ignored for casing/indentation purposes).
The comment is considered to end at the end of the line."
  :type 'string
  :group 'sqled)

(defcustom sqled-group-fcn-re
  "\\<\\(round\\|sum\\|max\\|min\\|count\\)\\>"
  "*Regular expression for grouping functions.
Used to generate the `GROUP BY' clause in a select statement."
  :type 'string
  :group 'sqled)
;;;=============================================================
;;; B. Variables defining casing options
;;;=============================================================

;;; Here we are using "SQL" to identify those commands that will be
;;; aligned with the end of the command word:
;;; 
;;;       SELECT tt
;;;         FROM yyy
;;;        WHERE ccc = ddd
;;;          AND dd = ee;
;;; 
;;; and "PL/SQL" to identify those commands that will be indented by a
;;; fixed amount like this:
;;; 
;;; IF x = y THEN
;;;    p := q;
;;; ELSE
;;;    c := d;
;;; END IF;
;;; 
;;; Each logical level is indented sqled-plsql-basic-offset spaces with respect to the
;;; previous one.
;;;
;;; "SQLPLUS" identifies commands that are not indented at all
;;;


(defcustom sqled-case-keyword-function 'upcase-word
  "*Function used to adjust the case of keywords.
It may be `downcase-word', `upcase-word' or `capitalize-word'."
  :type 'function
  :group 'sqled)

(defcustom sqled-base-case-function 'downcase-word
  "*Function used to adjust the case of unquoted, non-key words.
It may be `downcase-word', `upcase-word' or `capitalize-word'."
  :type 'function
  :group 'sqled)


(defcustom sqled-auto-case-flag t
  "*Non-nil means automatically changes case of keyword while typing.
Casing is done according to `sqled-case-keyword-function', so
long as a string matching `sqled-protect-line-re' is not found in
the line."
  :type 'boolean
  :group 'sqled )

(defcustom sqled-protect-line-re "\\-\\->>"
  "*Protect the line from auto-indentation.
If `sqled-auto-case-flag'  and/or `sqled-auto-indent-flag'
is non-nil, and a string matching this re is found in the line,
the line is not indented." )

;;;
;;; SQLPLUS keywords
;;;

(defcustom sqled-sqlplus-cmd-list
  '( "accept" "break" "btitle" "clear" "column" "col" "compute" "define"
     "exit" "pause" "prompt" "set" "spool" "spool off" "spool  off" "ttitle")
  "*List of SQLPLUS commands, recognized at the start of a line only.
The rest of the line is unchanged by casing and indentation functions.

NB: Changes will not take effect until sqled-mode.el is recompiled."
  :type '(repeat string)
  :group 'sqled )

(defvar sqled-sqlplus-cmd-re nil
  "*Regular expression for start of sqlplus commands.
Created at compilation by `regexp-opt'.")

;; NB: sqlplus keys are only recognized as commands if they start a line
;;     (no blanks before)
(if sqled-sqlplus-cmd-re
    ()
  (if (and (boundp 'emacs-major-version) (> emacs-major-version 20))
      (setq sqled-sqlplus-cmd-re
	    (eval-when-compile
	      (concat "^\\(?:!\\|@\\|\\<"
		      (regexp-opt sqled-sqlplus-cmd-list 't)) "\\>\\)"))
    (setq sqled-sqlplus-cmd-re
	  (eval-when-compile
	    (concat "^\\(!\\|@\\|\\<"
		    (regexp-opt sqled-sqlplus-cmd-list 't) "\\>\\)")))))

(defvar sqled-sqlplus-keyword-re nil
  "*Regular expression for a sqlplus keyword.
Created at compilation by `regexp-opt'.")

(if sqled-sqlplus-keyword-re
    ()
  (setq sqled-sqlplus-keyword-re
	(eval-when-compile
	  (concat "^\\<"
		  (regexp-opt sqled-sqlplus-cmd-list t)  "\\>" ))))


;;;
;;; SQL variables
;;;

;;; keywords
(defcustom sqled-sql-keyword-list
  '( "access" "add" "after" "all" "alter" "analyze" "and" "any" "as" "audit"
     "authorization" "before" "between" "block" "body" "by" "cascade" "change"
     "check" "cluster" "comment" "commit" "compress" "connect" "constraint"
     "constraints" "continue" "create" "current" "cursor" "declare" "default"
     "delete" "disable" "distinct" "drop" "each" "enable" "escape" "exclusive"
     "exec" "exists" "extent" "fetch" "file" "float" "foreign" "found" "from"
     "function" "go" "goto" "grant" "group" "having" "identified" "immediate"
     "in" "increment" "index" "indicator" "initial" "insert" "intersect" "is"
     "into" "key" "level" "like" "lock" "long" "maxextents" "minus" "mode"
     "modify" "module" "next" "noaudit" "nocompress" "not" "nowait" "numeric"
     "of" "off" "offline" "online" "only" "option" "or" "order" "package"
     "pctfree" "precision" "primary" "prior" "privileges" "procedure" "public"
     "raise" "raw" "real" "references" "rem" "remark" "rename" "resource"
     "return" "revoke" "rollback" "row" "rowlabel" "rownum" "rows" "savepoint"
     "schema" "section" "select" "sequence" "session" "set" "share" "size"
     "smallint" "some" "sqlerror" "start" "stop" "storage" "successful"
     "synonym" "system" "table" "tablespace" "to" "transaction" "trigger"
     "triggers" "truncate" "uid" "union" "unique" "update" "use" "user"
     "validate" "values" "view" "when" "whenever" "where" "with" "work")
  "*List of SQL keywords.
The case of these keywords is set by the function `sqled-case-keyword-function'.

This function is called  by any of the functions that adjust case.
If `sqled-auto-case-flag' is t it is invoked when any end-of-word
character is typed.

The face defaults to font-lock-keyword-face if font-lock mode is on.

NB: Changes will not take effect until sqled-mode.el is recompiled."
  :type '(repeat string)
  :group 'sqled )

(defvar sqled-sql-keyword-re nil
  "Regular expression for SQL keywords.
If `sqled-auto-case-flag' is t the case of these keywords is
automatically changed by the function `sqled-case-keyword-function'.
Created at compilation from `sqled-sql-keyword-list' by `regexp-opt'.")

(if sqled-sql-keyword-re
    ()
  (setq sqled-sql-keyword-re
	(eval-when-compile
	  (concat "\\<"
		  (regexp-opt sqled-sql-keyword-list t) "\\>" ))))

;;; built-in functions

(defcustom sqled-sql-function-list
  '("abs" "add_months" "ascii" "avg" "ceil" "chartorowid" "chr" "concat"
    "convert" "cos" "cosh" "count" "currval" "decode" "dump" "exp" "floor"
    "glb" "greatest" "greatest_lb" "hextoraw" "initcap" "instr" "instrb"
    "last_day" "least" "least_ub" "length" "lengthb" "ln" "log" "lower"
    "lpad" "ltrim" "lub" "max" "min" "mod" "months_between" "new_time"
    "next_day" "nextval" "nls_initcap" "nls_lower" "nls_upper" "nlssort"
    "nvl" "power" "rawtohex" "replace" "round" "rowidtochar" "rpad"
    "rtrim" "sign" "sin" "sinh" "soundex" "sqlcode" "sqlerrm" "sqrt"
    "stddev" "sum" "substr" "substrb" "tan" "tanh" "to_char"
    "to_date" "to_label" "to_multi_byte" "to_number" "to_single_byte"
    "translate" "trim" "trunc" "uid" "upper" "userenv" "variance" "vsize")
  "*List of sql built-in functions.
Used for font-lock mode, but not for casing/indentation.
NB: Changes will not take effect until sqled-mode.el is recompiled."
  :type '(repeat string)
  :group 'sqled )

(defvar sqled-sql-function-re nil
  "Regular expression for SQLPLUS keywords.

Created at compilation from `sqled-sql-function-list' by `regexp-opt'.")

(if sqled-sql-function-re
    ()
  (setq sqled-sql-function-re
	(eval-when-compile
	  (concat "\\<"
		  (regexp-opt sqled-sql-function-list  t) "\\>(" ))))


;;; data types
(defcustom sqled-sql-type-list
  '( "binary_integer" "blob" "boolean" "char" "character" "constant"
     "date" "decimal"  "int" "integer"  "number" "rowid" "%type"
     "varchar" "varchar2")
  "*List of SQL type-names.
If `sqled-auto-case-flag' is t the case of these keywords is
automatically changed by the function `sqled-case-keyword-function'.
NB: Changes will not take effect until sqled-mode.el is recompiled."
  :type '(repeat string)
  :group 'sqled )

(defvar  sqled-sql-type-re nil
  "Regular expression for SQL type-names.
If `sqled-auto-case-flag' is t the case of these keywords is
automatically changed by the function `sqled-case-keyword-function'.

In `font-lock-mode', defaults to font-lock-type-face.

Created at compilation by `regexp-opt'.")

(if sqled-sql-type-re
    ()
  (setq sqled-sql-type-re
	(eval-when-compile
	  (concat "\\<"
		  (regexp-opt sqled-sql-type-list t) "\\>" ))))

;;; commands

;;  SQL command keywords are not recognized for indentation purposes unless
;;    they occur as the first non-blank characters in a line.
;;
;;  "select" is an exception to this, and is recognized inside another
;;     statement and not only at the start of a line
;;
;;  "(" is also a command-start for indentation purposes

(if (and (boundp 'emacs-major-version) (> emacs-major-version 20))
    (defcustom sqled-sql-cmd-re
      "\\(^[ \t]*\\(?:alter +table\\|alter +session\\|analyze\\|audit\\|comment\\|commit\\|cursor\\|create +table\\|create +\\(unique \\)? *index\\|delete\\|drop\\|exec\\|fetch\\|grant\\|insert +into\\|lock\\|noaudit\\|numeric\\|pragma\\|public\\|rename\\|revoke\\|rollback\\|savepoint\\|truncate\\|update\\)\\>\\)\\|(\\|\\<select\\>"
      "*Regular expression for the start of an SQL command.
Controls SQL-style indentation when appearing at the start of a line."
      :type 'string
      :group 'sqled )

  (defcustom sqled-sql-cmd-re
    "\\(^[ \t]*\\(alter +table\\|alter +session\\|analyze\\|audit\\|comment\\|commit\\|cursor\\|create +table\\|create +\\(unique \\)? *index\\|delete\\|drop\\|exec\\|fetch\\|grant\\|insert +into\\|lock\\|noaudit\\|numeric\\|pragma\\|public\\|rename\\|revoke\\|rollback\\|savepoint\\|truncate\\|update\\)\\>\\)\\|(\\|\\<select\\>"
    "*Regular expression for the start of an SQL command.
Controls SQL-style indentation when matching the first word(s) of a line."
    :type 'string
    :group 'sqled ))

;;
;; Before sqled-sql-alist is used, all multiple
;; spaces in the command expression are reduced to single spaces.
;;
(if (and (boundp 'emacs-major-version) (> emacs-major-version 20))
    (defcustom sqled-sql-alist
      '(
	("select" . "\\<\\(?:and\\|connect\\|from\\|group\\|into\\|having\\|level\\|or\\|order\\|prior\\|select\\|start with\\|start  with\\|where\\)\\>" )
	("alter session" . "\\<set\\>")
	("fetch"         . "\\<into\\>")
	("update"        . "\\<\\(?:and\\|or\\|set\\|where\\)\\>")
	("delete"        . "\\<\\(?:and\\|or\\|where\\)\\>")
	("create table"  . "\\<select\\>")
	("create index"  . "\\<on\\>")
	("create unique index"  . "\\<on\\>")
	("insert into"   . "\\<\\(?:values|select\\)\\>")
	("("             . ")"))
      "AList of regular expressions for sub-parts of SQL commands.
The car is the main SQL command, and the cdr is the regular
expression for sub-parts of the command.

These parts are indented by sqled-indent-line so that they
right-align with the end of the main command."
      :type 'alist
      :group 'sqled )
  (defcustom sqled-sql-alist
    '(
      ("select" . "\\<\\(and\\|connect\\|from\\|group\\|into\\|having\\|level\\|or\\|order\\|prior\\|select\\|start with\\|start  with\\|where\\)\\>" )
      ("alter session" . "\\<set\\>")
      ("fetch"         . "\\<into\\>")
      ("update"        . "\\<\\(and\\|or\\|set\\|where\\)\\>")
      ("delete"        . "\\<\\(and\\|or\\|where\\)\\>")
      ("create table"  . "\\<select\\>")
      ("create index"  . "\\<on\\>")
      ("create unique index"  . "\\<on\\>")
      ("insert into"   . "\\<\\(values|select\\)\\>")
      ("("             . ")"))
    "AList of regular expressions for sub-parts of SQL commands.
The car is the main SQL command, and the cdr is the regular
expression for sub-parts of the command.

These parts are indented by sqled-indent-line so that they
right-align with the end of the main command."
    :type 'alist
    :group 'sqled ))
 
(defcustom sqled-command-end-re ";\\|^\\/\\s-*$"
  "*Regular expression for the end of an sql command."
  :type 'string
  :group 'sqled)


;;;
;;; PLSQL keywords
;;;

(defcustom sqled-plsql-keyword-list
  '( "begin" "close" "cursor" "declare" "else" "elsif" "end" "exception" "exit"
     "exit when" "for" "if" "into" "loop" "on" "open" "record" "replace"
     "return" "then" "type" "while" )
  "*List of PL/SQL keywords.
The case of these keywords is set by the function `sqled-case-keyword-function'.

This function is called  by any of the functions that adjust case.
If `sqled-auto-case-flag' is t it is invoked when any end-of-word
character is typed.

The face defaults to font-lock-keyword-face if font-lock mode is on.

NB: Changes will not take effect until sqled-mode.el is recompiled."
  :type '(repeat string)
  :group 'sqled )

(defvar sqled-plsql-keyword-re nil
  "Regular expression for PL/SQL keywords.

Created at compilation by `regexp-opt' from `sqled-plsql-keyword-list'." )

(if sqled-plsql-keyword-re
    ()
  (setq sqled-plsql-keyword-re
	(eval-when-compile
	  (concat "\\<"
		  (regexp-opt sqled-plsql-keyword-list t) "\\>"))))

;; 
;; Only PLSQL commands that are the first non-blank characters in the line
;; are recognized for indentation.
;;
(if (and (boundp 'emacs-major-version) (> emacs-major-version 20))
    (defcustom sqled-plsql-cmd-re
      "^[ \t]*\\<\\(?:if\\|loop\\|begin\\|declare\\|for\\|while\\|create\\(?:\\s-+or\\s-+replace\\)?\\(?:\\s-+\\)package\\|create\\(?:\\s-+or\\s-+replace\\)?\\(?:\\s-+\\)procedure\\|create\\(?:\\s-+or\\s-+replace\\)?\\s-+function\\|create\\(?:\\s-+or\\s-+replace\\)?\\s-+trigger\\)\\>"
      "*Regular expression for the start of a PL/SQL command."
      :type 'string
      :group 'sqled )
  (defcustom sqled-plsql-cmd-re
    "^[ \t]*\\<\\(if\\|loop\\|begin\\|declare\\|for\\|while\\|create\\(\\s-+or\\s-+replace\\)?\\(\\s-+\\)package\\|create\\(\\s-+or\\s-+replace\\)?\\(\\s-+\\)procedure\\|create\\(\\s-+or\\s-+replace\\)?\\s-+function\\|create\\(\\s-+or\\s-+replace\\)?\\s-+trigger\\)\\>"
    "*Regular expression for the start of a PL/SQL command.
Controls PL/SQL-style indentation when matching the first word(s) of a line."
    :type 'string
    :group 'sqled ))


;;
;; Note that before sqled-plsql-cmd-re-alist is used, all multiple
;; spaces in the command expression are reduced to single spaces.
;;
(if (and (boundp 'emacs-major-version) (> emacs-major-version 20))
    (defcustom sqled-plsql-alist
      '(
	("for" . "\\<\\(?:loop\\|end\\s-+loop\\)\\>")
	("loop" . "\\<\\(?:end\\s-+loop\\)\\>")
	("if" . "\\<\\(?:else\\|elsif\\|then\\|end\\s-+if\\)\\>")
	("begin" . "\\<exception\\|end\\>")
	("declare" . "\\<begin\\>")
	("create package" . "\\<as\\|end\\>")
	("create procedure" . "\\<\\(?:as\\|begin\\)\\>")
	("create function" . "\\<\\(?:is\\|as\\|return\\|begin\\)\\>")
	("create trigger" . "\\<\\(?:is\\|as\\|on\\|before\\|after\\|for\\|when\\|begin\\)\\>")
	("create or replace package" . "\\<as\\|end\\>")
	("create or replace procedure" . "\\<\\(?:as\\|begin\\)\\>")
	("create or replace function" . "\\<\\(?:is\\|as\\|return\\|begin\\)\\>")
	("create or replace trigger" . "\\<\\(?:is\\|as\\|on\\|before\\|after\\|for\\s-+each\\s-+row\\|declare\\|when\\|begin\\)\\>"))
      "*Regular expressions for sub-parts of PL/SQL commands.
The car is the main SQL command, and the cdr is the regular
expression for sub-parts of the command.
These parts are indented by sqled-indent-line to the same level
as the parent command.")

  (defcustom sqled-plsql-alist
    '(
      ("for" . "\\<\\(loop\\|end\\s-+loop\\)\\>")
      ("loop" . "\\<\\(end\\s-+loop\\)\\>")
      ("if" . "\\<\\(else\\|elsif\\|then\\|end\\s-+if\\)\\>")
      ("begin" . "\\<exception\\|end\\>")
      ("declare" . "\\<begin\\>")
      ("create package" . "\\<as\\|end\\>")
      ("create procedure" . "\\<\\(as\\|begin\\)\\>")
      ("create function" . "\\<\\(is\\|as\\|return\\|begin\\)\\>")
      ("create trigger" . "\\<\\(is\\|as\\|on\\|before\\|after\\|for\\|when\\|begin\\)\\>")
      ("create or replace package" . "\\<as\\|end\\>")
      ("create or replace procedure" . "\\<\\(as\\|begin\\)\\>")
      ("create or replace function" . "\\<\\(is\\|as\\|return\\|begin\\)\\>")
      ("create or replace trigger" . "\\<\\(is\\|as\\|on\\|before\\|after\\|for\\s-+each\\s-+row\\|declare\\|when\\|begin\\)\\>"))
    "*Regular expressions for sub-parts of PL/SQL commands.
The car is the main SQL command, and the cdr is the regular
expression for sub-parts of the command.
These parts are indented  by sqled-indent-line  to the same level
as the parent command."))

(if (and (boundp 'emacs-major-version) (> emacs-major-version 20))
    (defcustom sqled-plsql-cmd-part-re
      "\\<\\(?:create\\(?:\\s-+or\\s-+replace\\)?\\(?:\\s-+\\)package\\(?:\\s-+\\)body\\|else\\|elsif\\|exception\\|end\\|on\\|before\\|after\\|when\\|for\\s-+each\\s-+row\\)\\>"
      "*Regular expression for a sub-part of a PL/SQL command.
Using `package body' in the default value of this expression is a
hack to ensure that the `package body' line is NOT treated like the
`package' command, indenting everything under it.
Since `package body' usually only occurs at the start of a file,
indenting everything below it could create an excessive
amount of indentation."
      :type 'list
      :group 'sqled )
  (defcustom sqled-plsql-cmd-part-re
    "\\<\\(create\\(\\s-+or\\s-+replace\\)?\\(\\s-+\\)package\\(\\s-+\\)body\\|else\\|elsif\\|exception\\|end\\|on\\|before\\|after\\|when\\|for\\s-+each\\s-+row\\)\\>"
    "*Regular expression for a sub-part of a plsql command.
Using `package body' in the default value of this expression is a
hack to ensure that the `package body' line is NOT treated like the
`package' command, indenting everything under it.
`package body' usually only occurs at the start of a file, and
indenting everything below it could create an excessive
amount of indentation."
    :type 'list
    :group 'sqled ))

(defcustom sqled-plsql-cmd-end-re
  "\\<end\\>"
  "*Regular expression for recognizing end of PL/SQL commands."
  :type 'string
  :group 'sqled )


;;;=============================================================
;;; C. Variables defining indentation options
;;;=============================================================

(defcustom sqled-plsql-basic-offset 3
  "*Amount of indentation for PL/SQL statements."
  :type 'number
  :group 'sqled)

(defcustom sqled-auto-indent-flag t
  "*Non-nil means lines are indented as they are typed."
  :type 'boolean
  :group 'sqled )

(defcustom sqled-tab-binding 'indent-relative
  "*Function invoked by TAB when indentation is not required.
ie when sqled-auto-indent is null, or at any time if the cursor is after the
first word in the line."
  :type 'function
  :group 'sqled)


;;;=======================================================
;;; ---- end of user configurable variables
;;;=======================================================

(require 'easymenu)
   
(if (and (boundp 'emacs-major-version) (> emacs-major-version 20))
    (defvar sqled-keyword-re
      (concat  "\\(?:" sqled-sql-keyword-re   "\\|"
	       ;; sqled-sql-function-re  "\\|"
	       sqled-sql-type-re  "\\|"
	       sqled-plsql-keyword-re "\\|"
	       sqled-sqlplus-keyword-re "\\)")
      "regular expression for any sql, plsql or sqlplus keyword.
Derived from sqled-sql-type-re, sqled-plsql-keyword-re and
sqled-sqlplus-keyword-re. ")

  (defvar sqled-keyword-re
    (concat  "\\(" sqled-sql-keyword-re  "\\|"
	     ;; sqled-sql-function-re  "\\|"
	     sqled-sql-type-re  "\\|"
	     sqled-plsql-keyword-re  "\\|"
	     sqled-sqlplus-keyword-re "\\)")
    "regular expression for any sql, plsql or sqlplus keyword.
Derived from sqled-sql-type-re, sqled-plsql-keyword-re and
sqled-sqlplus-keyword-re."))

  

(defvar sqled-mode-syntax-table nil
  "*Syntax table to be used for editing SQL source code.")

(defun sqled-create-syntax-table ()
  "Create the syntax table for SQLED Mode."
  (setq sqled-mode-syntax-table (make-syntax-table))
  (set-syntax-table  sqled-mode-syntax-table)

  ;; define string brackets
  (modify-syntax-entry ?\' "\"" sqled-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" sqled-mode-syntax-table)

  (modify-syntax-entry ?%  "w" sqled-mode-syntax-table)
  (modify-syntax-entry ?\# "w" sqled-mode-syntax-table)
  (modify-syntax-entry ?:  "." sqled-mode-syntax-table)
  (modify-syntax-entry ?\; "." sqled-mode-syntax-table)
  (modify-syntax-entry ?&  "." sqled-mode-syntax-table)
  (modify-syntax-entry ?\| "." sqled-mode-syntax-table)
  (modify-syntax-entry ?+  "." sqled-mode-syntax-table)
  (modify-syntax-entry ?*  "." sqled-mode-syntax-table)
  (modify-syntax-entry ?/  "." sqled-mode-syntax-table)
  (modify-syntax-entry ?=  "." sqled-mode-syntax-table)
  (modify-syntax-entry ?<  "." sqled-mode-syntax-table)
  (modify-syntax-entry ?>  "." sqled-mode-syntax-table)
  (modify-syntax-entry ?$  "w" sqled-mode-syntax-table)
  (modify-syntax-entry ?\[ "." sqled-mode-syntax-table)
  (modify-syntax-entry ?\] "." sqled-mode-syntax-table)
  (modify-syntax-entry ?\{ "." sqled-mode-syntax-table)
  (modify-syntax-entry ?\} "." sqled-mode-syntax-table)
  (modify-syntax-entry ?.  "." sqled-mode-syntax-table)
  (modify-syntax-entry ?\\ "." sqled-mode-syntax-table)
 
  (modify-syntax-entry ?\_ "w" sqled-mode-syntax-table)

  ;; a single hyphen is punctuation, but a double hyphen starts a comment
  (modify-syntax-entry ?-  ". 12" sqled-mode-syntax-table)
  (modify-syntax-entry ?/  ". 14" sqled-mode-syntax-table)
  (modify-syntax-entry ?*  ". 23" sqled-mode-syntax-table)

  ;; and \f and \n end a comment
  (modify-syntax-entry ?\f  ">   " sqled-mode-syntax-table)
  (modify-syntax-entry ?\n  ">   " sqled-mode-syntax-table)

  ;; define parentheses to match
  (modify-syntax-entry ?\( "()" sqled-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" sqled-mode-syntax-table))

(defvar sqled-ret-binding nil
  "Variable to save key binding of RET when casing is activated.")

(defvar sqled-lfd-binding nil
  "Variable to save key binding of LFD when casing is activated.")

(defvar sqled-font-lock-keywords nil
  "Keywords for font-lock mode.")

(defvar  sqled-debug nil
  "Testing.")

;;;---------------------------------
;;; define keymap and menu for SQLED
;;;---------------------------------

(defvar sqled-mode-map
  (let ((sqled-mode-map (make-keymap)))
    (define-key sqled-mode-map "\177"     'backward-delete-char-untabify)
    (define-key sqled-mode-map "\C-c\C-a" 'sqled-reformat-region)
    (define-key sqled-mode-map "\C-c\C-b" 'sqled-indent-and-case-region)
    (define-key sqled-mode-map "\C-c\C-c" 'sqled-local-reformat-region)
    (define-key sqled-mode-map "\C-c\C-d" 'sqled-local-indent-and-case-region)
    (define-key sqled-mode-map "\C-c\C-e" 'sqled-local-indent-region)
    (define-key sqled-mode-map "\C-c\C-l" 'sqled-align-region-to-point)
    (define-key sqled-mode-map "\C-c\C-g" 'sqled-generate-group-by)
    (define-key sqled-mode-map "\C-c\C-w" 'sqled-where-list)
    (define-key sqled-mode-map "\C-c\C-v" 'sqled-variable-list)
    (define-key sqled-mode-map "\C-c:"    'sqled-comma-sep-list)
    (define-key sqled-mode-map "\C-c3"    'sqled-comma-sep-triple)
    (define-key sqled-mode-map "\C-c;"    'sqled-comma-sep-line)
    (define-key sqled-mode-map "\C-c\C-u" 'sqled-uncomment-region)
    (define-key sqled-mode-map "\C-c\C-f" 'sqled-fill-comment-paragraph-postfix)
    (define-key sqled-mode-map "\C-c\C-p" 'sqled-fill-comment-paragraph-prefix)
    (define-key sqled-mode-map "\t"       'sqled-indent-or-tab)
    (define-key sqled-mode-map "\C-c<"    'sqled-backtab)
    sqled-mode-map )
  "*Local keymap used for SQLED Mode.")

(easy-menu-define
  sqled-mode-menu sqled-mode-map
  "Menu for `sqled-mode'."
  '("SQLed"
    "--- Commands Affecting the Whole Buffer ---"
    ["auto case"                                   sqled-toggle-auto-case
     :style radio
     :selected  sqled-auto-case-flag ]
    ["auto indent"                                 sqled-toggle-auto-indent
     :style radio
     :selected   sqled-auto-indent-flag ]
    ["Indent and case buffer"                      sqled-reformat-buffer ]
    "--"
    "--- Commands Affecting the Region ---"
    ["Indent and case all words in region"         sqled-reformat-region   ]
    ["Indent and case keywords in region"          sqled-indent-and-case-region ]
    ["Indent region"                               indent-region ]
    ["Locally indent and case all words in region" sqled-local-reformat-region ]
    ["Locally indent and case keywords in region"  sqled-local-indent-and-case-region ]
    ["Locally indent region"                       sqled-local-indent-region ]
    ["Align region to its first column"            sqled-align-region-to-point]
    "--"
    "--- Generate code from the output of Oracle DESC ---"
    ["'where' clause for a table join"    sqled-where-list ]
    ["plsql list of variables"            sqled-variable-list ]
    ["comma-separated list 1 per line"    sqled-comma-sep-list ]
    ["comma-separated list 3 per line"    sqled-comma-sep-triple ]
    ["comma-separated list inline"        sqled-comma-sep-line ]
    "--"
    "--- Other Commands for Generating Code ---"
    ["Generate `Group By' clause (experimental)"  sqled-generate-group-by ]
    "--"
    "--- Commands for Formatting Comments ---"
    ["Comment region (--)"              comment-region ]
    ["Uncomment region (--)"            sqled-uncomment-region ]
    ["Fill comment paragraph (--)"       sqled-fill-comment-paragraph-prefix ]
    ["Fill comment paragraph (/*..*/)"   sqled-fill-comment-paragraph-postfix ]))


;;;;                  -------------------
;;;;                  ** 2. SQLED-MODE **
;;;;                  -------------------

;;;###autoload
(defun sqled-mode()
  "A major mode for editing (not executing) a mixture of SQLPLUS, SQL
and PL/SQL code.

With sqled-auto-indent-flag and sqled-auto-case-flag = t, text is
indented and keywords are (default upper-)cased as they are typed.
Lines containing a match for sqled-protect-line-re are not changed.

Functions are available for:

 1. Indenting and/or changing the case of regions or the whole buffer
    Indentation of a region may be done with respect to the entire buffer,
    or by treating the selected region in isolation.

 2. Commenting/uncommenting regions and filling comments

 3. manipulating the output of SQLPLUS `describe table' commands
    to generate:  lists of columns
                  'where' clauses
                  function argument lists
    These commands operate on a region consisting of lines of the form:

         COLUMN_NAME1    NOT NULL CHAR(5)
         COLUMN_NAME2             INTEGER
         COLUMN_NAME3             VARCHAR2(2000)
                                               ...... etc
         

 4. Generating the `Group By' clause of a select statement.  This is
    somewhat experimental, and the output should be checked.  It should still
    save a lot of typing when the select clause is long.

Keywords are of three types:
SQL commands are indented to the level of the column after the
end of the command:

   UPDATE mytable
      SET mycolumn = `xx'
    WHERE anothercolumn > 5;

   Most SQL command keywords are only recognized for indentation
   purposes if they occur as the first non-blank word in a line.
   'select' is the only exception to this. It is recognized inside
   another statement and not only at the start of a line.

   '(' is also an SQL command-start for indentation purposes.

PL/SQL commands are indented as is usual in C-style programming languages:

    BEGIN
       IF x > y THEN
          dothis
       ELSE
          dothat
       END IF;
    END;

    The depth of an indentation step  is controlled by the variable
    sqled-plsql-basic-offset (default 3).

SQLPLUS commands are recognized only if they start a line (*no* initial
spaces allowed), and all text from the end of the command word to the end
of the line is treated as comment - ie, is not auto-cased.

eg:

PROMPT Select the address
COLUMN address format A9


 KNOWN BUGS
 ==========

 Does not recognize multiline /* ..
                                  .. */ as comments
 although /*........*/ in one line is recognized.

 Multiline strings are not recognized.
 A workaround for this is to write the string:

 'linestart
 linecontinuation'

 as:
 
'linestart'||chr(10)||
 'linecontinuation'

"

  ;; Set up some special values for emacs variables
  (interactive)
  (kill-all-local-variables)

  (make-local-variable 'comment-start)
  (setq comment-start sqled-comment-prefix)

  ;; comment-end must be set because it may hold a wrong value if
  ;; this buffer had been in another mode before.
  (make-local-variable 'comment-end)
  (setq comment-end "")

  (make-local-variable 'comment-start-skip)
  ;; used by autofill, font-lock, etc
  (setq comment-start-skip
        "--+[ \t]*\\|\\/\\*[ \t]*\\|\\<rem[ \t]+\\|\\<prompt[ \t]+")

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'sqled-indent-line)

  (make-local-variable 'mark-even-if-inactive)
  ;; needed to avoid re-selecting for "desc table" functions
  (setq mark-even-if-inactive t)

  (set (make-local-variable 'comment-padding) 0)

  (make-local-variable 'parse-sexp-ignore-comments)
  ;; needed for paren balancing
  (setq parse-sexp-ignore-comments t)
  (if sqled-debug
(progn
  (sqled-create-syntax-table)
  (setq sqled-mode-hook nil )
  (setq sqled-comment-prefix "-- " )
  (setq sqled-leftcomment-start-re "\\-\\-\\-\\|/\\*" )
  (setq sqled-group-fcn-re "\\<\\(round\\|sum\\|max\\|min\\|count\\)\\>")
  (setq sqled-fill-comment-prefix "/* " )
  (setq sqled-fill-comment-postfix " */" )
  (setq sqled-fill-comment-postfix-re "[ \t]*\\*/" )
  (setq sqled-comment-start-re "\\(^\\(rem \\|remark \\)\\|--\\|/\\*\\)" )
  (setq sqled-case-keyword-function 'upcase-word )
  (setq sqled-base-case-function 'downcase-word )
  (setq sqled-auto-case-flag t )
  (setq sqled-protect-line-re "\\-\\->>" )
  (setq sqled-command-end-re ";\\|^\\/\\s-*$")
  (setq sqled-sqlplus-cmd-list
	'( "accept" "break" "btitle" "clear" "column" "col" "compute" "define" "exit"
	   "pause" "prompt" "set" "spool" "spool off" "spool  off" "ttitle")
	)


  (setq sqled-sqlplus-keyword-re
	(concat "\\<"
		(regexp-opt sqled-sqlplus-cmd-list t) "\\>"))

  (setq sqled-sqlplus-cmd-re
	(concat "^\\(?:!\\|@\\|\\<"
		(regexp-opt sqled-sqlplus-cmd-list t) "\\>\\)" ))

  (setq sqled-sql-keyword-list
	'("access" "add" "after" "all" "alter" "analyze" "and" "any" "as"
	  "audit" "authorization" "before" "between" "block" "body" "by"
	  "cascade" "change" "check" "cluster" "comment" "commit" "compress"
	  "connect" "constraint" "constraints" "continue" "create" "current"
	  "cursor" "declare" "default" "delete" "disable" "distinct" "drop"
	  "each" "enable" "escape" "exclusive" "exec" "exists" "extent" "fetch"
	  "file" "float" "foreign" "found" "from" "function" "go" "goto" "grant"
	  "group" "having" "identified" "immediate" "in" "increment" "index"
	  "indicator" "initial" "insert" "intersect" "into" "is" "key" "level"
	  "like" "lock" "long" "maxextents" "minus" "mode" "modify" "module"
	  "next" "noaudit" "nocompress" "not" "nowait" "numeric" "of" "off"
	  "offline" "online" "only" "option" "or" "order" "package" "pctfree"
	  "precision" "primary" "prior" "privileges" "procedure" "public" "raise"
          "raw" "real" "references" "rem" "remark" "rename" "resource" "return"
	  "revoke" "rollback" "row" "rowlabel" "rownum" "rows" "savepoint"
	  "schema" "section" "select" "sequence" "session" "set" "share" "size"
	  "smallint" "some" "sqlerror" "start" "stop" "storage" "successful"
	  "synonym" "system" "table" "tablespace" "to" "transaction" "trigger"
	  "triggers" "truncate" "uid" "union" "unique" "update" "use" "user"
	  "validate" "values" "view" "when" "whenever" "where" "with" "work") )

  (setq sqled-sql-keyword-re
	(concat "\\<"
		(regexp-opt sqled-sql-keyword-list t) "\\>" ))


  (setq sqled-sql-function-list
	'("abs" "add_months" "ascii" "avg" "ceil" "chartorowid" "chr" "concat"
	  "convert" "cos" "cosh" "count" "currval" "decode" "dump" "exp" "floor"
	  "glb" "greatest" "greatest_lb" "hextoraw" "initcap" "instr" "instrb"
	  "last_day" "least" "least_ub" "length" "lengthb" "ln" "log" "lower"
	  "lpad" "ltrim" "lub" "max" "min" "mod" "months_between" "new_time"
	  "next_day" "nextval" "nls_initcap" "nls_lower" "nls_upper" "nlssort"
	  "nvl" "power" "rawtohex" "replace" "round" "rowidtochar" "rpad"
	  "rtrim" "sign" "sin" "sinh" "soundex" "sqlcode" "sqlerrm" "sqrt"
	  "stddev" "sum" "substr" "substrb" "tan" "tanh" "to_char"
	  "to_date" "to_label" "to_multi_byte" "to_number" "to_single_byte"
	  "translate" "trim" "trunc" "uid" "upper" "userenv" "variance" "vsize"))
  
  (setq sqled-sql-function-re
	(concat "\\<"
		(regexp-opt sqled-sql-function-list  t) "\\>(" ))

  (setq sqled-sql-function-re
	(concat "\\<\\("
		(regexp-opt sqled-sql-function-list  t) "\\)(" ))

  (setq sqled-sql-type-list
	'( "binary_integer" "blob" "boolean" "char" "character" "constant"
	   "date" "decimal"  "int" "integer"  "number" "rowid" "%type"
	   "varchar" "varchar2"))


  (setq sqled-sql-type-re
	(concat "\\<"
		(regexp-opt sqled-sql-type-list  't) "\\>"))


  (setq sqled-sql-cmd-re
	"\\(^[ \t]*\\(?:alter +table\\|alter +session\\|analyze\\|audit\\|comment\\|commit\\|cursor\\|create +table\\|create +\\(unique \\)? *index\\|delete\\|drop\\|exec\\|fetch\\|grant\\|insert +into\\|lock\\|noaudit\\|numeric\\|pragma\\|public\\|rename\\|revoke\\|rollback\\|savepoint\\|truncate\\|update\\)\\>\\)\\|(\\|\\<select\\>"
	)

  (setq sqled-sql-alist
	'(
	  ("select" . "\\<\\(?:and\\|connect\\|from\\|group\\|into\\|having\\|level\\|or\\|order\\|prior\\|select\\|start with\\|start  with\\|where\\)\\>" )
	  ("alter session" . "\\<set\\>")
	  ("fetch"         . "\\<into\\>")
	  ("update"        . "\\<\\(?:and\\|or\\|set\\|where\\)\\>")
	  ("delete"        . "\\<\\(?:and\\|or\\|where\\)\\>")
	  ("create table"  . "\\<select\\>")
	  ("create index"  . "\\<on\\>")
	  ("create unique index"  . "\\<on\\>")
	  ("insert into"   . "\\<\\(?:values|select\\)\\>")
	  ("("             . ")"))
	)

  (setq sqled-plsql-keyword-list
	'( "begin" "close" "cursor" "declare" "else" "elsif" "end" "exception" "exit"
	   "exit when" "for" "if" "into" "loop" "on" "open" "record" "replace" "return"
	   "then" "type" "while" ) )

  (setq sqled-plsql-keyword-re
	(concat "\\<"
		(regexp-opt sqled-plsql-keyword-list t) "\\>"))


  (setq sqled-plsql-cmd-re
	"^[ \t]*\\<\\(?:if\\|loop\\|begin\\|declare\\|for\\|while\\|create\\(?:\\s-+or\\s-+replace\\)?\\(?:\\s-+\\)package\\|create\\(?:\\s-+or\\s-+replace\\)?\\(?:\\s-+\\)procedure\\|create\\(?:\\s-+or\\s-+replace\\)?\\s-+function\\|create\\(?:\\s-+or\\s-+replace\\)?\\s-+trigger\\)\\>"
	)
  (setq sqled-plsql-alist
	'(
	  ("for" . "\\<\\(?:loop\\|end\\s-+loop\\)\\>")
	  ("loop" . "\\<\\(?:end\\s-+loop\\)\\>")
	  ("if" . "\\<\\(?:else\\|elsif\\|then\\|end\\s-+if\\)\\>")
	  ("begin" . "\\<exception\\|end\\>")
	  ("declare" . "\\<begin\\>")
	  ("create package" . "\\<as\\|end\\>")
	  ("create procedure" . "\\<\\(?:as\\|begin\\)\\>")
	  ("create function" . "\\<\\(?:is\\|as\\|return\\|begin\\)\\>")
	  ("create trigger" . "\\<\\(?:is\\|as\\|on\\|before\\|after\\|for\\|when\\|begin\\)\\>")
	  ("create or replace package" . "\\<as\\|end\\>")
	  ("create or replace procedure" . "\\<\\(?:as\\|begin\\)\\>")
	  ("create or replace function" . "\\<\\(?:is\\|as\\|return\\|begin\\)\\>")
	  ("create or replace trigger" . "\\<\\(?:is\\|as\\|on\\|before\\|after\\|for\\s-+each\\s-+row\\|declare\\|when\\|begin\\)\\>")))

  (setq sqled-plsql-cmd-part-re
	"\\<\\(?:create\\(?:\\s-+or\\s-+replace\\)?\\(?:\\s-+\\)package\\(?:\\s-+\\)body\\|else\\|elsif\\|exception\\|end\\|on\\|before\\|after\\|when\\|for\\s-+each\\s-+row\\)\\>"
	)

  (setq sqled-keyword-re
	(concat  "\\(?:" sqled-sql-keyword-re  "\\|"
		 ;; sqled-sql-function-re  "\\|"
		 sqled-sql-type-re  "\\|"
		 sqled-plsql-keyword-re "\\|"
		 sqled-sqlplus-keyword-re "\\)"))

  (setq sqled-plsql-cmd-end-re "\\<end\\>")
  (setq sqled-plsql-basic-offset 3 )
  (setq sqled-auto-indent-flag t )
  (setq sqled-tab-binding 'indent-relative )
  ))

 

(make-local-variable 'font-lock-defaults)
(setq sqled-font-lock-keywords
      (list
       (list (concat sqled-comment-start-re ".*$") 0 'font-lock-comment-face t)
       (list (concat sqled-sqlplus-cmd-re ".*$") 0 'font-lock-reference-face t)
       (cons sqled-plsql-keyword-re  'font-lock-keyword-face )
       (cons sqled-sql-type-re  'font-lock-type-face )
       (cons sqled-sql-keyword-re  'font-lock-keyword-face )
       (list sqled-sql-function-re 1 'font-lock-function-name-face )
       ))

(setq font-lock-defaults
      '(sqled-font-lock-keywords  nil t  nil nil ))

(make-local-variable 'case-fold-search)

;; Oracle sql is case-insensitive
(setq case-fold-search t)

(make-local-variable 'fill-paragraph-function)
(setq fill-paragraph-function 'sqled-fill-comment-paragraph)

(setq major-mode 'sqled-mode)
(setq mode-name "SQLED")

(use-local-map sqled-mode-map)
(easy-menu-add sqled-mode-menu  sqled-mode-map)

(if sqled-mode-syntax-table
    (set-syntax-table sqled-mode-syntax-table)
  (sqled-create-syntax-table))

(run-hooks 'sqled-mode-hook)

(if (or sqled-auto-indent-flag sqled-auto-case-flag)
    (sqled-activate-keys)))			; END sqled-mode


;;;;         ----------------------------------------------
;;;;         ** 3. AUTO CASING AND INDENTATION FUNCTIONS **
;;;;         ----------------------------------------------

;;;------------------------
(defun sqled-activate-keys ()
  "Save original keybindings for ret/lfd.
When autocasing is activated, save the keybindings so
they can be called after the autocase function when
ret or lfd are typed."
;;;------------------------
  ;; the 'or ...' is there to be sure that the value will not
  ;; be changed again when SQL Mode is called more than once (MH)
  ;; 
  (or sqled-ret-binding
      (setq sqled-ret-binding (key-binding "\C-M")))
  (or sqled-lfd-binding
      (setq sqled-lfd-binding (key-binding "\C-j")))
  ;; call case/indent function after certain keys.
  (mapcar (function (lambda(key)
                      (define-key  sqled-mode-map
                        (char-to-string key)
			'sqled-case-indent-interactive)))
          '( ?& ?* ?( ?)  ?= ?+ ?[  ?]
                ?\\ ?| ?\; ?:  ?\" ?< ?,  ?\n 32 ?\r )))

;;;------------------------
(defun sqled-toggle-auto-case ()
  "Unset automatic keyword casing if it is set, otherwise set it."
;;;------------------------
  (interactive)
  (if sqled-auto-case-flag
      (setq sqled-auto-case-flag nil)
    (setq sqled-auto-case-flag t)))

;;;------------------------
(defun sqled-toggle-auto-indent ()
  "Unset automatic indentation if it is set, otherwise set it."
;;;------------------------
  (interactive)
  (if  sqled-auto-indent-flag
      (setq sqled-auto-indent-flag nil)
    (setq sqled-auto-indent-flag t)))


;;;------------------------
(defun sqled-indent-or-tab ()
  "Indent if in or before first word of line.
Otherwise call `sqled-tab-binding'.  This is the function invoked by TAB.

Do nothing if the line is protected by `sqled-protect-line-re'.
If point is before the first word in the line, call `sqled-indent-line'.
Otherwise call `sqled-tab-binding' (default indent-relative: move to under
the start of the next word in the previous line, or `tab-to-tab-stop' if
there is no such word).

See Emacs documentation for `indent-relative'."
;;;------------------------
  (interactive "*")
  (let ((rec (recent-keys) )
	(last-but-one (- (length (recent-keys)) 2)))
    (save-match-data
      (if (and sqled-auto-indent-flag
	       (not (looking-at (concat ".*" sqled-protect-line-re)))
	       (not (eq 'tab (aref rec last-but-one )))
	       (save-excursion (re-search-backward "^\\|\\>")
			       (looking-at "^")))
	  (sqled-indent-line)
	(funcall sqled-tab-binding)))))

;;;------------------------
(defun sqled-case-indent-interactive (arg)
  "Indent and case according to sqled mode.
Command invoked by typing an end-of-word character CHAR.

If cursor is immediately after a keyword, and `sqled-auto-case-flag' is non-nil,
adjust the case of the previous word by calling `sqled-case-last-word-if-key'.

If `sqled-auto-indent-flag' is non-nil:
   If the previous character was also CHAR, insert CHAR.
   If CHAR is a newline, indent the current line and insert CHAR with optional
        prefix argument ARG.
   Otherwise, insert CHAR with optional prefix argument ARG and then indent
        the current line."
;;;------------------------
  (interactive "*P")
  (let ((lastk last-command-char)
	(rec  (recent-keys))
	(last-but-one (- (length (recent-keys)) 2)))
    (if sqled-auto-case-flag (sqled-case-last-word-if-key lastk))
    (if sqled-auto-indent-flag
      (cond
       ;; if command is repeated, just insert it
       ((eq lastk (aref rec last-but-one)) (self-insert-command
					    (prefix-numeric-value arg)))
       ;; indent first, then insert for \n and \r
       ((eq lastk ?\n)   (sqled-indent-line) (funcall sqled-lfd-binding))
       ((eq lastk ?\r)   (sqled-indent-line) (funcall sqled-ret-binding))

       ;; for others, insert first, then indent
       ((progn (self-insert-command (prefix-numeric-value arg))
	       (sqled-indent-line))))
      ;; no auto-indent
      (cond
       ((eq lastk ?\n)  (funcall sqled-lfd-binding))
       ((eq lastk ?\r)  (funcall sqled-ret-binding))
       ((self-insert-command (prefix-numeric-value arg)))))))

;;;------------------------
(defun sqled-case-last-word-if-key ( &optional nextchar )
  "If after a keyword, adjust the case of the previous word.
Use the case function `sqled-case-keyword-function'.
If NEXTCHAR is not a word constituent, treat the current position as
end-of-word.
Called by `sqled-case-indent-interactive' after an end-of-word character is typed."
;;;------------------------
  (if (not (sqled-in-protected-region-p))
	(if (and (> (point) (+ 1 (point-min)))
		 (sqled-after-keyword nextchar))
	    (funcall sqled-case-keyword-function -1))))


;;;------------------------
(defun sqled-indent-line ()
  "Indent the current line as SQL or PL/SQL as appropriate.
If the line starts with a closing paren, run `sqled-align-parens'.
If line starts with '/' do nothing.
Called by tab via `sqled-indent-or-tab' if not at end of line.

This is the indent-line function for `sqled-mode'."
;;;------------------------
  (interactive "*P")
  (if (>  (count-lines (point) (point-min))  1 )
      (save-match-data
	(let ( (cmdlist) (cmd) (sql-cmd-align) (par) (protected)
	       (mat) (leftcomment))

	  ;; Decide if we are in an sql or plsql command
	  ;; or inside parens.

	  (save-excursion
	    (beginning-of-line)
	    (setq cmdlist (sqled-in-sql-command))
	    (setq cmd (car cmdlist))
	    (setq sql-cmd-align (cdr cmdlist))
	    ;; Comment starting --- or /* should left align
	    (setq leftcomment (looking-at (concat
					   "\\s-*\\("
					   sqled-leftcomment-start-re "\\)")))
	    ;; Skip sqlplus, "/" at the start of a line, or protected code
	    (setq protected (or (looking-at "^/")
				(looking-at (concat ".*" sqled-protect-line-re))
				( and (looking-at sqled-sqlplus-cmd-re)
				      (not (looking-at sqled-plsql-keyword-re))
				      (not cmd))))
	    ;; Align a ")" that is the first non-blank char in the line
	    ;; with the matching "("
	    (setq par (looking-at "\\s-*)")))
	  ;; Call the appropriate indentation function
	  (if (not  protected)
	      (if leftcomment
		  (let ((currcol (current-column))
			(currindent (current-indentation)))
		    (indent-line-to 0)
		    (move-to-column (- currcol currindent)))
		(if par
		    (sqled-align-parens)
		  (if cmd
		      (sqled-indent-as-sql (sqled-extract-cmd cmd) sql-cmd-align)
		    (sqled-indent-as-plsql)))))))))


;;;------------------------
(defun sqled-indent-as-sql (cmd sql-cmd-align)
  "Indent the line as part of the sql command CMD.
CMD is a SQL command keyword ending one column before the column SQL-CMD-ALIGN."
;;;------------------------
  (save-match-data
    (let  ((curr-indent) (align-col) (sub) (startpos (current-column))  )
      (save-excursion
        (back-to-indentation)
        (setq curr-indent (current-column))
        (setq sub (cdr-safe (assoc cmd sqled-sql-alist)))

        ;; If line starts with sub-part of sql cmd,
	;; align end of 1st word with end of cmd
        (if (and sub (looking-at sub ))
            (progn
              (if (equal (match-string 0) ")")
                  (forward-char)
                (forward-word 1))
              (setq align-col (+ (current-column) 1)))
          (setq align-col (current-column))))
      (let ((new-indent (+ curr-indent
			   (- sql-cmd-align align-col))))
        (if (< new-indent 0)
            (setq new-indent 0))
	(indent-line-to new-indent)
        (move-to-column (max 0 (+ startpos (- new-indent curr-indent))))))))
    


;;;------------------------
(defun sqled-indent-as-plsql ()
  "Indent the line with respect to the current PL/SQL command.
If not in a PL/SQL command, do not indent."
;;;------------------------
  (save-match-data
    (let ((level) (cmd) (sub) (startpos (current-column))
          (new-indent) (curr-indent (current-indentation)))
      (save-excursion
        (beginning-of-line)
        (if (sqled-search-back-ignore
             (concat sqled-plsql-cmd-end-re "\\|" sqled-plsql-cmd-re )
             (point-min))
            ;;
            ;; If we have a command start and have not passed its end
            ;; set indentation to that of the command. Check for "not cmd part"
            ;; to avoid "for each row", eg
            ;;
            (if (and (looking-at sqled-plsql-cmd-re)
                     (not (looking-at sqled-plsql-cmd-part-re)))
                (progn
		  ;; get rid of extra spaces
                  (setq cmd (sqled-extract-cmd (match-string 0)))
 
                  (skip-chars-forward " \t")
                  (setq level (+ (current-column) 3)))
              (setq level (current-indentation)))))
      ;;
      ;; Now check for "part" of cmd at start of line - dont indent that wrt cmd
      ;; Here we treat the case of being inside a command separately, so we can
      ;; have command "parts" that indent as part of one command but not as
      ;; part of another.
      ;;
      (if (null level)
          (setq new-indent 0)
        (save-excursion
          (back-to-indentation)
          (if cmd
              (progn
                (setq sub (cdr-safe (assoc cmd sqled-plsql-alist)))
                (if (and sub (looking-at sub))
                    (setq new-indent (- level sqled-plsql-basic-offset))
                  (setq new-indent level)))
            (if (and (looking-at sqled-plsql-cmd-part-re)
                     (>= level sqled-plsql-basic-offset) )
                ;; dont break if at start of line - PACKAGE ends with
                ;; "end" but we dont indent for it since there is usually only
                ;; one package per file.
                ( setq new-indent (- level sqled-plsql-basic-offset))
              (setq new-indent level)))))
      (indent-line-to new-indent)
      (move-to-column (max 0 (+ startpos (- new-indent curr-indent )))))))
  

;;;------------------------
(defun sqled-align-parens ()
  "Align `)' at start of line with matching '('."
;;;------------------------
  (interactive "*")
  (let ((doalign) (new-paren-pos) (old-paren-pos)
        (curr-column) (curr-indent) (new-indent))
    (save-match-data
      (save-excursion
        (beginning-of-line)
        (sqled-search-forward-ignore "\)" (save-excursion (end-of-line) (point)))
        (if (= (char-before (point)) ?\))
            (progn
              (setq doalign t)
              (setq old-paren-pos (1- (current-column) ))
              (save-excursion (backward-sexp)
                              (setq new-paren-pos (current-column)))))))
    (if doalign
        (progn
          (setq curr-column (current-column))
          (setq curr-indent (current-indentation))
          (setq new-indent (+ curr-indent (- new-paren-pos old-paren-pos)))
          (indent-line-to new-indent )
          (move-to-column (max 0 (+ curr-column (- new-indent curr-indent ))))))))




;;;;      ------------------------------------------------
;;;;           ** 4. INTERACTIVE COMMENT FUNCTIONS **
;;;;      ------------------------------------------------

;;;------------------------
(defun sqled-fill-comment-paragraph-prefix ()
  "Fill the current comment paragraph and indent it.
Point must be inside a comment.
Removes `sqled-fill-comment-postfix' (default '*/') from the ends of lines.
Resets match data."
;;;------------------------
  (interactive "*")
  (sqled-fill-comment-paragraph t))

;;;------------------------
(defun sqled-fill-comment-paragraph-postfix ()
  "Fill the current comment paragraph and indent it.
Point must be inside a comment.
Appends the string `sqled-fill-comment-prefix' (default '/*') to the start of
each line, and `sqled-fill-comment-postfix' (default '*/') to the end of
each line.
Resets match data."
;;;------------------------
  (interactive "*")
  (sqled-fill-comment-paragraph t t))


;;;------------------------
(defun sqled-fill-comment-paragraph (&optional justify postfix)
  "Fill the current comment paragraph.  Point must be inside a comment.

If JUSTIFY is non-nil, justify each line as well.
The possible values of JUSTIFY are  `full', `left', `right',
`center', or `none' (equivalent to nil).

If POSTFIX is non-nil, enclose the line in
`sqled-fill-comment-prefix' (default '/*') and
`sqled-fill-comment-postfix' (default '*/').

Otherwise, the line is started with `comment-start' (default '--'),
and any `sqled-fill-comment-postfix' characters are removed.
Resets match data."
;;;------------------------
  (interactive "*P")
  (let ((opos (point-marker))
        (begin nil)
        (end nil)
        (indent nil)
        (the-comment-start nil))

    (if postfix
	(setq the-comment-start sqled-fill-comment-prefix )
      (setq the-comment-start comment-start))
    ;; check if inside comment
    (if (not (sqled-in-comment-p))
        (error "Not inside comment"))

    ;;
    ;; find limits of paragraph
    ;;
    (message "Filling comment paragraph...")
    (save-excursion
      (beginning-of-line)
      ;; find end of paragraph
      (while  (looking-at (concat "[ \t]*\\("
                                  sqled-comment-start-re
                                  "\\).*$"))
        (forward-line 1))
      (backward-char)
      (setq end (point-marker))

      (goto-char opos)
      ;; find begin of paragraph
      (beginning-of-line)
      (while (and (looking-at (concat "[ \t]*\\("
                                      sqled-comment-start-re
                                      "\\).*$"))
                  ( > (point) (point-min)))
        (forward-line -1))

      (if (> (point) (point-min))
          (forward-line 1))

      ;; get indentation to calculate width for filling
      ;; first delete comment-start so line indents properly
      (if (looking-at (concat "[ \t]*" sqled-comment-start-re))
          (replace-match ""))
      (sqled-indent-line)

      (back-to-indentation)
      (setq indent (current-column))
      (setq begin (point-marker)) )
 
    ;; delete old postfix if necessary
    (save-excursion
      (goto-char begin)
 
      (while (re-search-forward (concat sqled-fill-comment-postfix-re
                                        "[ \t]*$")
                                end t)
        (replace-match "")))

    ;; delete leading whitespace and uncomment
    (save-excursion
      (goto-char begin)
      (beginning-of-line)
      (while (re-search-forward (concat "^[ \t]*\\("
                                        sqled-comment-start-re
                                        "\\|"
                                        sqled-fill-comment-prefix
                                        "\\)[ \t]*") end t)
        (replace-match "")))

    ;; calculate fill width
    (setq fill-column (- fill-column indent
                         (length the-comment-start)
                         (if postfix
                             (length sqled-fill-comment-postfix)
                           0)))
    ;; fill paragraph
    (fill-region begin end justify)
    (setq fill-column (+ fill-column
                         indent
                         (length the-comment-start)
                         (if postfix
                             (length sqled-fill-comment-postfix)
                           0)))

    ;; re-comment and re-indent region
    (save-excursion
      (goto-char begin)
      (indent-to indent)
      (insert the-comment-start)
      (while (re-search-forward "\n" end t)
        (replace-match (concat "\n" the-comment-start))
        (beginning-of-line)
        (indent-to indent)))

    ;; append postfix if wanted
    (if (and justify
             postfix
             sqled-fill-comment-postfix)
        (progn
          ;; append postfix
          (save-excursion
            (goto-char begin)
            (while (and (< (point) end)
                        (re-search-forward "$" end t))
              (replace-match
               (concat
                (make-string
                 (+ (- fill-column
                       (current-column)
                       (length sqled-fill-comment-postfix) )) ?\  )
                sqled-fill-comment-postfix ))
              (forward-line 1)))))
    (message "Filling comment paragraph...done")
    (goto-char opos))
  t)


;;;------------------------
(defun sqled-uncomment-region (beg end)
  "Delete `comment-prefix` from the start of each line in the region BEG, END.
Only works for comment-prefix (default '--'),
not for sqled-fill-comment-prefix/postfix (default /*..*/)."
;;;------------------------
  (interactive "*r")
  (comment-region beg end -1))


;;;;         ----------------------------------------------
;;;;         ** 5. BULK CASING AND INDENTATION FUNCTIONS **
;;;;         ----------------------------------------------

;;;------------------------
(defun sqled-base-case-region (from to)
  "Apply `sqled-base-case-function' to all non-quoted, non-key words in region.
Take all words in the region defined by FROM, TO that are not quoted,
commented or in the body of a sqlplus command, and process them  with
the command `sqled-base-case-function' (default `downcase-word' )."
;;;------------------------
  (interactive "*r")
  (save-excursion
    (goto-char from)
    ;;
    ;; loop: look for all word starts
    ;;
    (while (and (< (+ (point) 1) to))
      ;; do nothing if it is in a string or comment
      (if (and (not (sqled-in-protected-region-p ))
               (looking-at "\\<"))
          (funcall sqled-base-case-function 1)
        (forward-char 1)))))


;;;------------------------
(defun sqled-adjust-case-region (from to)
  "Adjusts the case of all words in the region defined by FROM, TO.
Resets match data."
;;;------------------------
  (interactive "*r")
  (save-excursion
    (goto-char to)
    ;;
    ;; loop: look for all identifiers and keywords
    ;;
    (while (and (> (point) from)
                (re-search-backward sqled-keyword-re from t))
      (progn
        (let (word (match-string 0))
          (or
           ;; do nothing if in a string or comment or sqlplus body
           (sqled-in-protected-region-p)
           (progn
             (funcall sqled-case-keyword-function 1)
             (forward-word -1))))))))

;;;------------------------
(defun sqled-indent-and-case-region (beg end)
  "Indent region  defined by BEG,END and adjust case of keywords only."
;;;------------------------
  (interactive "*r")
  (sqled-adjust-case-region beg end)
  (indent-region beg end nil))

;;;------------------------
(defun sqled-reformat-region (beg end)
  "Indent region.  Adjust case of keywords not quoted or commented.
Indent taking code before the region defined by BEG,END into account if
necessary.
Use `sqled-local-reformat-region` to treat the region in isolation."
;;;------------------------
  (interactive "*r")
  (message "Reformatting...")
  (sqled-base-case-region beg end)
  (sqled-adjust-case-region beg end)
  (indent-region beg end nil )
  (message "Reformatting...done"))

;;;------------------------
(defun sqled-local-reformat-region (beg end)
  "Indent region and adjust case of keywords.  Ignore rest of file.
Indent the region defined by defined by BEG, END as if the selected region
were the entire file, ignoring code before the region."
;;;------------------------
  (interactive "*r")
  (message "Reformatting...")
  (let (start)
    (if (> beg 1)
        ( setq start (- beg 1) )
      ( setq start beg ))
    (narrow-to-region start end))
  (sqled-base-case-region (point-min) (point-max))
  (sqled-adjust-case-region (point-min) (point-max))
  (indent-region (point-min) (point-max) nil )
  (widen)
  (message "Reformatting...done"))

;;;------------------------
(defun sqled-local-indent-and-case-region (beg end)
  "Indent region defined by BEG, END and adjust case of keywords only."
;;;------------------------
  (interactive "*r")
  (let (start)
    (if (> beg 1)
        ( setq start (- beg 1) )
      ( setq start beg ))
    (narrow-to-region start end))
  (sqled-adjust-case-region beg end)
  (indent-region beg end nil)
  (widen))

;;;------------------------
(defun sqled-local-indent-region (beg end)
  "Indent region defined by BEG, END ignoring code before region.
Indent as if the selected region were the entire file,
ignoring code before the region."
;;;------------------------
  (interactive "*r")
  (message "Reformatting...")
  (let (start)
    (if (> beg 1)
        ( setq start (- beg 1) )
      ( setq start beg ))
    (narrow-to-region start end))
  (indent-region (point-min) (point-max) nil )
  (widen)
  (message "Reformatting...done"))


;;;------------------------
(defun sqled-reformat-buffer ()
  "Indent buffer.  Adjust case of keywords not quoted or commented."
;;;------------------------
  (interactive "*")
  (message "Reformatting buffer...")
  (sqled-base-case-region (point-min) (point-max))
  (sqled-adjust-case-region (point-min) (point-max))
  (indent-region (point-min) (point-max) nil)
  (message "Reformatting buffer...done"))



;;;;      -----------------------------------------------------------
;;;;      ** 6. FUNCTIONS FOR USE ON SQLPLUS DESCRIBE-TABLE OUTPUT **
;;;;      -----------------------------------------------------------

;;;------------------------
(defun sqled-where-list(beg end)
  "Generate a join clause from the output of a sqlplus `DESCRIBE TABLE' command.
The region (BEG, END) is expected to consist of a string of the form
   COL1   TYPE1
   COL2   TYPE2
   COL3   TYPE3
\(part of the output from a sqlplus `describe table' command)
is inserted into the buffer and selected, this command replaces
the above text with:
          tabA.col1 = tabB.col1
      AND tabA.col2 = tabB.col2
      AND tabA.col3 = tabB.col3
where tabA and tabB are table aliases read from the minibuffer."
;;;------------------------
  (interactive "*r")
  (save-match-data
    (let ((left-table)
          (right-table)
          (wordlist ())
          (indent-column nil)
          (curr-word))
      (setq left-table (read-from-minibuffer "left table alias: "))
      (setq right-table (read-from-minibuffer "right table alias: "))
      (setq wordlist (sqled-first-words beg end))
      (if wordlist
          (progn
	    (setq curr-word (downcase (car wordlist)))
            (goto-char beg)
            (delete-region beg end)
	    (forward-word -1)
	    (if (looking-at "where")
		(forward-word 1)
	      (goto-char beg)
	      (insert "   "))
	    (setq indent-column (- (current-column) 3))
	    (insert (concat " " left-table "." curr-word " = "
			    right-table "." curr-word "\n"))
	    (setq wordlist (cdr wordlist))
	    (while wordlist
	      (setq curr-word (downcase (car wordlist)))
	      (insert (make-string indent-column  ?\ ) "AND "
		      (concat left-table "." curr-word " = "
			      right-table "." curr-word "\n"))
	      (setq wordlist (cdr wordlist))))))))


;;;------------------------
(defun sqled-variable-list (beg end)
  "Generate a pl/sql declaration list.
The region (BEG, END) is expected to consist of a string of the form
   COL1   TYPE1
   COL2   TYPE2
   COL3   TYPE3
\(part of the output from a sqlplus `describe table' command)
is inserted into the buffer and selected, this command replaces
the above text with:
     X_col1  .col1%type,
     X_col2  .col2%type,
     X_col3  .col3%type, ...
where X_ is a prefix read from the minibuffer.
Intended for generating PL/SQL variable declarations and
procedure argument lists."
;;;------------------------
  (interactive "*r")
  (let ((wordlist ())
        (curr-word)
        (indent-string)
        (prefix))
    (setq wordlist (sqled-first-words beg end))
    (setq prefix (read-from-minibuffer "prefix for variable name: "))
    (goto-char beg)
    (setq indent-string (make-string  (current-column) ?\ ))
    (delete-region beg end)
    (while wordlist
      (setq curr-word (downcase (car wordlist)))
      (insert prefix curr-word )
      (move-to-column  (+ (length indent-string) 20) t)
      (insert  "." curr-word "%type,\n" indent-string )
      (setq wordlist (cdr wordlist)))))


;;;------------------------
(defun sqled-comma-sep-line (beg end)
  "Generate a comma-separated column list, all in one line.
The region (BEG, END) is expected to consist of a string of the form
   COL1   TYPE1
   COL2   TYPE2
   COL3   TYPE3
\(part of the output from a sqlplus `describe table' command)
is inserted into the buffer and selected, this command replaces
the above text with:
     col1, col2, col3, ...
Intended for generating SELECT and INSERT clauses."
;;;------------------------
  (interactive "*r")
  (let ((wordlist ())
        (curr-word))
    (setq wordlist (sqled-first-words beg end))
    (goto-char beg)
    (delete-region beg end)
    (while wordlist
      (setq curr-word (downcase (car wordlist)))
      (insert curr-word ", " )
      (setq wordlist (cdr wordlist)))))

  
;;;------------------------
(defun sqled-comma-sep-list (beg end)
  "Generate a comma-separated column list, 1 per line.
The region (BEG, END) is expected to consist of a string of the form
   COL1   TYPE1
   COL2   TYPE2
   COL3   TYPE3
\(part of the output from a sqlplus `describe table' command)
is inserted into the buffer and selected, this command replaces
the above text with:
     col1,
     col2,
     col3, ...
Intended for generating SELECT and INSERT clauses."
;;;------------------------
  (interactive "*r")
  (let ((wordlist ())
        (curr-word)
        (indent-string))
    (setq wordlist (sqled-first-words beg end))
    (goto-char beg)
    (setq indent-string (make-string  (current-column) ?\ ))
    (delete-region beg end )
    (while wordlist
      (setq curr-word (downcase (car wordlist)))
      (insert curr-word ", \n" indent-string)
      (setq wordlist (cdr wordlist)))))


;;;------------------------
(defun sqled-comma-sep-triple (beg end)
  "Generate a comma-separated column list, 3 per line.
The region (BEG, END) is expected to consist of a string of the form
   COL1   TYPE1
   COL2   TYPE2
   COL3   TYPE3
   ......
\(part of the output from a sqlplus `describe table' command).
This command replaces the above text with:
     col1,         col2,          col3,
     col4,         col5,          col6,
     col7,         col8,  ...
Intended for generating long SELECT and INSERT clauses."
;;;------------------------
  (interactive "*r")
  (let ((wordlist ())
        (curr-word)
        (counter 1)
        (indent-string))
    (setq wordlist (sqled-first-words beg end))
    (goto-char beg)
    (setq indent-string (make-string  (current-column) ?\ ))
    (delete-region beg end )
    (while wordlist
      (setq curr-word (downcase (car wordlist)))
      (if (= 0 (mod counter 3))
          (insert (format "%-20s\n%s" (concat curr-word ",")  indent-string))
        (insert (format "%-20s" (concat curr-word ",")) ))
      (setq wordlist (cdr wordlist))
      (setq counter (+ counter 1)))))
     
;;;------------------------
(defun sqled-align-region-to-point (beg end)
  "Aligns the region defined by BEG, END so all lines start at its start column."
;;;------------------------
  (interactive "*r")
  (goto-char beg)
  (let ((lines 0)
        (newindent (current-column)))
    (while (< (point) end)
      (setq lines (+ lines 1))
      (forward-line))
    (goto-char beg)
    (let ((ln 1))
      (forward-line)
      (while (< ln lines)
        (setq ln (+ ln 1))
        (indent-line-to newindent)
        (forward-line)))))


;;;------------------------
(defun sqled-first-words (beg end)
  "Return a list of the first words of each line in the region.
Region is defined by BEG, END.
For use with the output of a DESCRIBE TABLE command."
;;;------------------------
  (save-match-data
    (let ((wordlist)
          (newword))
      (goto-char beg)
      (setq wordlist '())
      (while (< (point) end)
        (re-search-forward "\\<\\([^ \t\n]+\\)\\>" end t)
        (setq newword (match-string 1))
        (setq wordlist (append  wordlist (list newword)))
        (forward-line))
      wordlist )))

;;;;        --------------------------------------------
;;;;       ** 7. MISCELLANEOUS INTERACTIVE FUNCTIONS **
;;;;        --------------------------------------------

;;;------------------------
(defun sqled-test-expr ()
  "A debugging tool.
Check which sql regexps match the following word or words
immediately after point."
;;;------------------------
  (interactive)
  (let ((msg)
        (wd))
    (message "Current word is: < %s >" (current-word))
    (if (looking-at sqled-sql-keyword-re)
        (setq msg (concat msg "~ sqled-sql-keyword-re: " (match-string 0))))
    (if (looking-at sqled-comment-start-re)
        (setq msg (concat msg "~ sqled-comment-start-re: " (match-string 0))))
    (if (looking-at sqled-sql-cmd-re)
        (setq msg (concat msg "~ sqled-sql-cmd-re: " (match-string 0))))
    (if (looking-at sqled-command-end-re)
        (setq msg (concat msg "~ sqled-command-end-re: " (match-string 0))))
    (if (looking-at sqled-plsql-cmd-re)
        (setq msg (concat msg "~ sqled-plsql-cmd-re: " (match-string 0))))
    (if (looking-at sqled-plsql-cmd-part-re)
        (setq msg (concat msg "~ sqled-plsql-cmd-part-re: " (match-string 0))))
    (if (looking-at sqled-plsql-cmd-end-re)
        (setq msg (concat msg "~ sqled-plsql-cmd-end-re: " (match-string 0))))
    (if (looking-at sqled-plsql-keyword-re)
        (setq msg (concat msg "~ sqled-plsql-keyword-re: " (match-string 0))))
    (if (looking-at sqled-sqlplus-keyword-re)
        (setq msg (concat msg "~ sqled-sqlplus-keyword-re: " (match-string 0))))

    (if (sqled-in-string-p)
        (setq msg (concat msg "~ In-string ")))
    (if (sqled-in-blank-line-p)
        (setq msg (concat msg "~ In-blank-line ")))
    (if (sqled-in-comment-p) (setq msg (concat msg "~ In-comment ")))
    (if (sqled-in-sqlplus-body-p) (setq msg (concat msg "~ In-sqlplus-body ")))
    (if (sqled-in-sqlplus-p) (setq msg (concat msg "~ In-sqlplus ")))
    (if (sqled-in-string-comment-or-sqlplus-p)
        (setq msg (concat msg "~ In-str-or-comment ")))
    (if (sqled-in-protected-region-p)
        (setq msg (concat msg "~ In-protected-region ")))
    (let ((cmdlist (sqled-in-sql-command)))
      (message "cmdlist is %s" cmdlist )
      (if cmdlist
	  (setq msg (concat msg "~ In-sql-command " (car cmdlist) " indent to "
			    (number-to-string (cdr cmdlist) )))))
    (if (sqled-after-keyword)
        (setq msg (concat msg "~ after-keyword: ")))
    (message msg)
    (let (( pp (parse-partial-sexp (save-excursion  (beginning-of-line) (point)) (point))))
	  (message  "sexp: 0=%s 1=%s 2=%s 3=%s 4=%s 5=%s 6=%s 7=%s 8=%s 9=%s 10=%s #" (nth 0 pp) (nth 1 pp)   (nth 2 pp)  (nth 3 pp)  (nth 4 pp)  (nth 5 pp)  (nth 6 pp)  (nth 7 pp)  (nth 8 pp)  (nth 9 pp)  (nth 10 pp) ))
    ))

;;;------------------------
(defun sqled-backtab ()
  "Move point backwards to previous indentation level."
;;;------------------------
  (interactive "*")
  (untabify (point-min) (point))
  (let ((start-indent)
        (backup (current-column)))
    (setq start-indent (- (current-column) 1))
    (save-excursion
      (while (and (>= backup start-indent)
                  (> (point) 1))
        (forward-line -1)
        (if (not (sqled-in-blank-line-p))
            (setq backup (current-indentation)))))
    (indent-line-to backup)))

;;;------------------------
(defun sqled-generate-group-by ()
  "(Re-)generate the GROUP BY clause of an sql select statement.
Not perfect, but helpful if the select list is long.
Fails to delete the column alias if it is after `)'."
;;;------------------------
  (interactive "*")
  (setq case-fold-search t)
  (let ((command (sqled-in-sql-command)) (start (point)) (end) (grpstart) (grpend) )
       (if (and (equal (car command) "SELECT")
		(save-excursion (beginning-of-line)
				(looking-at " *group +by")))
	   (progn
	     (save-excursion
	       (sqled-search-back-ignore "SELECT")
	       (forward-word 1)
	       (setq start (point))
	       (sqled-search-forward-ignore "\\<from " (point-max))
	       (forward-word -1)
	       (setq end (point)))
	     (setq grpstart (make-marker))
	     (set-marker grpstart (point))
	     (insert (buffer-substring start end))
	     (setq grpend (make-marker))
	     (set-marker grpend (point))

	     (goto-char grpstart)
	     ;; First get rid of the grouping functions
	     (while (sqled-search-forward-ignore sqled-group-fcn-re grpend)
	       (replace-match "" nil nil )
	       (kill-sexp)
	       ;; remove the comma and any alias from the grouping function
	       (if (looking-at "[ \t]*\\(\\w\\|\\d\\)*,[ \t]*\n*")
		   (replace-match "" nil nil ))
	       (if (> (point) grpend )
		   (progn
		     (skip-chars-backward ",(\\s )*")
		     (delete-region (point) grpend))))
	     
	     ;; Then get rid of the column aliases
	     (goto-char grpstart)
 	     (while (re-search-forward "[^ \t\n,]+\\(\\s +[^ ()\t\n,]+\\s *\\)," grpend t)
	       (let ((pp (parse-partial-sexp grpstart (1- (point)))))
		 (if  (and (= (nth 0 pp) 0 )
			   (null (nth 4 pp))
			   (null (nth 3 pp)))
		     (replace-match "" nil nil nil 1))))

	     ;; Remove a final column alias if there is one
	     
	     (while (re-search-forward  "[^ \t\n,]+\\(\\s +[^ ()\t\n,]+\\s *\\)" grpend t)
	       (let ((pp (parse-partial-sexp grpstart (1- (point)))))
		 (if  (and (= (nth 0 pp) 0 )
			   (null (nth 4 pp))
			   (null (nth 3 pp)))
		     (replace-match "" nil nil nil 1))))
	     )
	 (message "not in group by clause of select statement"))))
				   

;;;;         ------------------------------------------
;;;;         ** 8. NON-INTERACTIVE UTILITY FUNCTIONS **
;;;;         ------------------------------------------

;;;------------------------
(defun sqled-extract-cmd (cmd)
  "Return CMD in lower case with whitespace compressed.
Initial tabs or spaces are stripped, and multiple internal
tabs or spaces replaced by single spaces."
;;;------------------------
  ;; Used to match up "create  or  replace", for instance, with the key
  ;; "create or replace" in sqled-plsql-alist.
  ;; 
  (save-match-data
    (let ((cmd-name))
      (setq cmd-name(downcase cmd))
      ;;
      ;; get rid of initial blanks and
      ;; replace other groups of blanks with one space
      ;;
      (if (string-match "^\\s-+" cmd-name)
          (setq cmd-name (replace-match "" 1 nil cmd-name)))
      (while (string-match "\\s-\\s-+" cmd-name)
        (setq cmd-name (replace-match " " 1 nil cmd-name)))
      cmd-name)))


;;;------------------------
(defun test-back ( )
  "Search back for regexp re, ignoring strings, comments, sqlplus commands and
anything inside paired parentheses.
If successful, return string matched, else return nil.
Resets match data."
;;;------------------------
(interactive "*")
(let (( re (read-from-minibuffer "enter re: ")) (bound (point-min)))
  ;;
  ;; return nil if in string or comment
  ;;
  (if (or (sqled-in-string-p) (sqled-in-comment-p))
      nil
      
    (let ((found nil))
      (while (and (not found) (> (point) bound)
                  (re-search-backward (concat ")\\|\\(" re "\\)") bound 1))
        ;;
        ;; skip back over any paired parens
        ;;
        (if (and (equal (match-string 0) ")")
		  (not (sqled-in-string-comment-or-sqlplus-p)))
	    (progn
              (forward-char)		; so we are after the )
              (backward-sexp))		; back to matching (
          ;;
          ;; Otherwise, check for string or comment
          ;;
          
	  (if (not (sqled-in-string-comment-or-sqlplus-p))
	      (setq found t))))

      (if (and (> (point) bound)
	       found
	       (not (sqled-in-string-comment-or-sqlplus-p)))
	  (match-string 0)
        nil))))
)

;;;------------------------
(defun sqled-search-back-ignore (re &optional bound )
  "Search back for regexp RE not in string, comments, sqlplus or ().
Search back for regexp RE, ignoring strings, comments,
sqlplus commands and anything inside paired parentheses.
Optional second argument BOUND limits the search, otherwise the limit
is the start of the buffer.
If successful, move to start of matched string, and return
the string matched, else return nil.
Resets match data."
;;;------------------------
  (if (null bound) (setq bound (point-min)))
  ;;
  ;; return nil if in string or comment
  ;;
  (if (or (sqled-in-string-p) (sqled-in-comment-p))
      nil
      
    (let ((found nil))
      (while (and (not found) (> (point) bound)
                  (re-search-backward (concat ")\\|\\(" re "\\)") bound 1))
        ;;
        ;; skip back over any paired parens
        ;;
        (if (and (equal (match-string 0) ")")
		  (not (sqled-in-string-comment-or-sqlplus-p)))
	    (progn
              (forward-char)		; so we are after the )
              (backward-sexp))		; back to matching (
          ;;
          ;; Otherwise, check for string or comment
          ;;
          
	  (if (not (sqled-in-string-comment-or-sqlplus-p))
	      (setq found t))))
      (if (and (> (point) bound)
	       found
	       (not (sqled-in-string-comment-or-sqlplus-p)))
	  (match-string 0)
        nil))))
          
;;;------------------------
(defun test-forward (&optional bound )
  "Search forward for regexp re, ignoring strings, comments,
sqlplus commands and anything inside paired parentheses.
Optional second argument BOUND limits the search, otherwise the limit
is the end of the buffer.
If successful, return the string matched, else return nil.
Resets match data."
;;;------------------------
(interactive "*")
(let ((re (read-from-minibuffer "enter re: ")))
       (if (null bound) (setq bound (point-max)))
  ;;
  ;; return nil if in string or comment
  ;;
  (if (sqled-in-string-comment-or-sqlplus-p)
      nil
    (let ((found nil))
      (while (and (not found) (< (point) bound))
        (re-search-forward (concat "(\\|" re) bound 1)
        ;;
        ;; skip forward over any paired parens
        ;;
        (if (and (equal (match-string 0) "(")
		  (not (sqled-in-string-comment-or-sqlplus-p)))
	    (progn (forward-char -1)
		   (forward-sexp))		; forward to matching )
          ;;
          ;; Otherwise, check for string or comment
          ;;
          (if (not (sqled-in-string-comment-or-sqlplus-p))
              (setq found t))))
      (if found
          (match-string 0)
        nil)))))
          
   
;;;------------------------
(defun sqled-search-forward-ignore (re &optional bound )
  "Search back for regexp RE not in string, comments, sqlplus or ().
Search forward for regexp RE, ignoring strings, comments,
sqlplus commands and anything inside paired parentheses.
Optional second argument BOUND limits the search, otherwise the limit
is the end of the buffer.
If successful, move to end of matched string and return the
string matched, else move to limit of search and return nil.
Resets match data."
;;;------------------------
  (if (null bound) (setq bound (point-max)))
  ;;
  ;; return nil if in string or comment
  ;;
  (if (sqled-in-string-comment-or-sqlplus-p)
      nil
    (let ((found nil))
      (while (and (not found) (< (point) bound)
        (re-search-forward (concat "(\\|" re) bound 1 ))
        ;;
        ;; skip forward over any paired parens
        ;;
        (if (and (equal (match-string 0) "(")
		  (not (sqled-in-string-comment-or-sqlplus-p)))
	    (progn (forward-char -1)
		   (forward-sexp))		; forward to matching )
          ;;
          ;; Otherwise, check for string or comment
          ;;
          (if (not (sqled-in-string-comment-or-sqlplus-p))
              (setq found t))))
      (if found
          (match-string 0)
        nil))))
          

;;;;              ----------------------------
;;;;              ** 9. CONTEXT INFORMATION **
;;;;              ----------------------------

;;;------------------------
(defun sqled-after-keyword(&optional nextchar)
  "If cursor is immediately after a keyword, return t, otherwise return nil.
If NEXTCHAR is not a word-constituent, treat the current position as
end-of-word.
Return nil when after non-alpha keywords such as (,!,@.
Do NOT check to see if the keyword is in a string or comment."
;;;------------------------
(if (and (> (point) (point-min))
	 (equal (char-syntax (char-before (point))) ?w ))
    (if (or (null nextchar) (equal (char-syntax nextchar) "?w"))
	(string-match sqled-keyword-re (current-word))
      (save-excursion
	(save-match-data
	  (let ((endcol (current-column)))
	    (re-search-backward "\\<\\|^" (point-min) )
	    (string-match sqled-keyword-re
			      (substring (current-word) 0 (- endcol (current-column)))
					   )))))
  nil))

;;;------------------------
(defun sqled-in-blank-line-p ()
 "Return t if cursor is in a line consisting only of whitespace."
;;;------------------------
  (save-excursion
    (beginning-of-line)
    (looking-at "\\s-*$")))

;;;------------------------
(defun sqled-in-string-p ()
  "Return t if point is inside a string.
The string must start and end on the current line."
;;;------------------------
  ;;(Taken from pascal-mode.el).
  (save-excursion
    (nth 3
         (parse-partial-sexp
          (save-excursion  (beginning-of-line) (point))
          (point)))))

;;;------------------------
(defun sqled-in-sqlplus-body-p()
  "Return t if in the body of a sqlplus command.
ie if the current line starts with a sqlplus command,
and point is not inside or immediately after the command."
;;;------------------------
  (save-match-data
    (save-excursion
      (re-search-backward "\\> \\|^" (point-min))
      (if (not (looking-at "^"))
          (progn
            (beginning-of-line)
            (looking-at sqled-sqlplus-cmd-re))))))

;;;------------------------
(defun sqled-in-sqlplus-p()
  "Return t if in a line starting with a sqlplus command."
;;;------------------------
  (save-match-data
    (save-excursion
      (beginning-of-line)
      (and (looking-at sqled-sqlplus-cmd-re)
	   (not (sqled-in-sql-command))))))


;;;------------------------
(defun sqled-in-comment-p ()
  "Return t if inside a comment.
A comment is defined as everything from the end of
`sqled-comment-start-re' to the end of the line,
where the match string is not quoted."
;;;------------------------

  (let ((startpt (point)) (linestart (save-excursion (beginning-of-line) (point))))
    (save-match-data
      (save-excursion
	(re-search-backward sqled-comment-start-re linestart 1)
        (and (looking-at sqled-comment-start-re)
             ;; we are not in the middle of the comment-start expression
             (search-forward (match-string 0) startpt t)
             (>= startpt (point))
             ;; the comment-start is not quoted
	     (not (sqled-in-string-p)))))))


;;;------------------------
(defun sqled-in-sql-command()
  "Return the command and its indent level if inside a sql command.
Search back (ignoring quotes and comments) for the last sql command
start or command end.

If the search finds the end of an sql command, return nil.

If it is a command that is found, return (CMD . COL), where
CMD is the sql command string found, and  COL is the column after the
last one of the string CMD.
It is the column to which the body of the command is indented."
;;;------------------------
  (save-match-data
    (save-excursion
      (sqled-search-back-ignore (concat sqled-sql-cmd-re "\\|" sqled-command-end-re )
                                (point-min) )
      (if (and (looking-at sqled-sql-cmd-re)
	       (not (sqled-in-sqlplus-body-p)))
	  (let ((cmd (match-string 0)))
	    (re-search-forward ".\\>\\|(" (point-max) 1)
	    (cons cmd (1+ (current-column))))
	nil))))


;;;------------------------
(defun sqled-in-string-comment-or-sqlplus-p()
  "Return t if inside a string, comment or sqlplus command."
;;;------------------------
  (or (sqled-in-comment-p)
      (sqled-in-sqlplus-p)
      (sqled-in-string-p)))

;;;------------------------
(defun sqled-in-protected-region-p()
  "Return t if inside a string, comment or sqlplus body."
;;;------------------------
  (or (sqled-in-comment-p)
      (sqled-in-sqlplus-body-p)
      (sqled-in-string-p)))


;;; provide ourself

(provide 'sqled-mode)

;;; sqled-mode.el ends here


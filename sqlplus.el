;;; sqlplus.el --- User friendly interface to SQL*Plus and support for PL/SQL compilation

;; Copyright (C) 2007, 2008 Peter Karpiuk, Scott Tiger S.A.

;; Author: Peter Karpiuk <piotr.karpiuk (at) gmail (dot) com>
;; Maintainer: Peter Karpiuk <piotr.karpiuk (at) gmail (dot) com>
;; Created: 25 Nov 2007
;; Version 0.9.1
;; Keywords: sql sqlplus oracle plsql

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;  Facilitates interaction with Oracle via SQL*Plus (GNU Emacs only).
;;  Moreover, this package complements plsql.el (Kahlil Hodgson) 
;;  upon convenient compilation of PL/SQL source files.
;;
;;  This package was inspired by sqlplus-mode.el (Rob Riepel, Peter
;;  D. Pezaris, Martin Schwenke), but offers more features:
;;    - tables are parsed, formatted and rendered with colors, like in
;;      many GUI programs; you can see raw SQL*Plus output also, 
;;      if you wish
;;    - table will be cutted if you try to fetch too many rows
;;      (SELECT * FROM MY_MILLION_ROWS_TABLE); current SQL*Plus command
;;      will be automatically interrupted under the hood in such cases
;;    - you can use many SQL*Plus processes simultaneously,
;;    - font locking (especially if you use Emacs>=22), with database
;;      object names highlighting,
;;    - history (log) of executed commands - see` sqlplus-history-dir`
;;      variable,
;;    - commands for fetching any database object definition
;;      (package, table/index/sequence script)
;;    - query result can be shown in HTML,
;;    - input buffer for each connection can be saved into file on
;;      disconnect and automatically restored on next connect (see
;;      'sqlplus-session-cache-dir' variable); if you place some
;;      SQL*Plus commands between '/* init */' and '/* end */'
;;      comments in saved input buffer, they will be automatically
;;      executed on every connect
;;    - if you use plsql.el for editing PL/SQL files, you can compile
;;      such sources everytime with C-cC-c; error messages will be
;;      parsed and displayed for easy source navigation
;;    - M-. or C-mouse-1 on database object name will go to definition
;;      in filesystem (use arrow button on toolbar to go back)
;;
;;  The following commands should be added to a global initialization
;;  file or to any user's .emacs file to conveniently use
;;  sqlplus-mode:
;;
;;    (require 'sqlplus)
;;    (add-to-list 'auto-mode-alist '("\\.sqp\\'" . sqlplus-mode))
;;
;;  If you want PL/SQL support also, try something like this:
;;
;;  (require 'plsql)
;;  (setq auto-mode-alist
;;    (append '(("\\.pls\\'" . plsql-mode) ("\\.pkg\\'" . plsql-mode)
;; 		("\\.pks\\'" . plsql-mode) ("\\.pkb\\'" . plsql-mode)
;; 		("\\.sql\\'" . plsql-mode) ("\\.PLS\\'" . plsql-mode) 
;; 		("\\.PKG\\'" . plsql-mode) ("\\.PKS\\'" . plsql-mode)
;; 		("\\.PKB\\'" . plsql-mode) ("\\.SQL\\'" . plsql-mode)
;; 		("\\.prc\\'" . plsql-mode) ("\\.fnc\\'" . plsql-mode)
;; 		("\\.trg\\'" . plsql-mode) ("\\.vw\\'" . plsql-mode)
;; 		("\\.PRC\\'" . plsql-mode) ("\\.FNC\\'" . plsql-mode)
;; 		("\\.TRG\\'" . plsql-mode) ("\\.VW\\'" . plsql-mode))
;; 	      auto-mode-alist ))
;;
;;  M-x sqlplus will start new SQL*Plus session.
;;
;;  C-RET   execute command under point
;;  S-C-RET execute command under point and show result table in HTML 
;;          buffer
;;  M-RET   explain execution plan for command under point
;;  M-. or C-mouse-1: find database object definition (table, view
;;          index, synonym, trigger, procedure, function, package)
;;          in filesystem
;;  C-cC-s  show database object definition (retrieved from database)
;;
;;  Use describe-mode while in sqlplus-mode for further instructions.
;;
;;  Many useful commands are defined in orcl-mode minor mode, which is
;;  common for input and otput SQL*Plus buffers, as well as PL/SQL
;;  buffers.
;;
;;  For twiddling, see 'sqlplus' customization group.
;;
;;  If you find this package useful, send me a postcard to address:
;;
;;    Peter Karpiuk
;;    Scott Tiger S.A.
;;    ul. Gawinskiego 8
;;    01-645 Warsaw
;;    Poland

;;; Known bugs:

;; 1. Result of SQL select command can be messed up if some columns
;;    has newline characters.  To avoid this, execute SQL*Plus command
;;      column <colname> truncated
;;    before such select 

;;; Code:

(require 'recentf)
(require 'font-lock)
(require 'cl)
(require 'sql)
(require 'tabify)
(require 'skeleton)

(defconst sqlplus-revision "$Revision: 1.7 $")

;;;  Variables -

(defgroup sqlplus nil
  "SQL*Plus"
  :group 'tools
  :version 21)

(defcustom plsql-auto-parse-errors-flag t
  "Non nil means parse PL/SQL compilation results and show them in the compilation buffer."
  :group 'sqlplus
  :type '(boolean))

(defcustom sqlplus-init-sequence-start-regexp "/\\* init \\*/"
  "SQL*Plus start of session init command sequence."
  :group 'sqlplus
  :type '(regexp))

(defcustom sqlplus-init-sequence-end-regexp "/\\* end \\*/"
  "SQL*Plus end of session init command sequence."
  :group 'sqlplus
  :type '(regexp))

(defcustom sqlplus-explain-plan-warning-regexps '("TABLE ACCESS FULL" "INDEX FULL SCAN")
  "SQL*Plus explain plan warning regexps"
  :group 'sqlplus
  :type '(repeat regexp))

(defcustom sqlplus-syntax-faces
  '((schema font-lock-type-face nil)
    (table font-lock-type-face ("dual"))
    (synonym font-lock-type-face nil)
    (view font-lock-type-face nil)
    (column font-lock-constant-face nil)
    (sequence font-lock-type-face nil)
    (package font-lock-type-face nil)
    (trigger font-lock-type-face nil)
    (index font-lock-type-face) nil)
  "Font lock configuration for database object names in current schema.
This is alist, and each element looks like (SYMBOL FACE LIST)
where SYMBOL is one of: schema, table, synonym, view, column,
sequence, package, trigger, index.  Database objects means only
objects from current schema, so if you want syntax highlighting
for other objects (eg. 'dual' table name), you can explicitly
enumerate them in LIST as strings."
  :group 'sqlplus
  :tag "Oracle SQL Syntax Faces"
  :type '(repeat (list symbol face (repeat string))))

(defcustom sqlplus-output-buffer-max-size (* 50 1000 1000)
  "Maximum size of SQL*Plus output buffer.
After exceeding oldest results are deleted."
  :group 'sqlplus
  :tag "SQL*Plus Output Buffer Max Size"
  :type '(integer))

(defcustom sqlplus-select-result-max-col-width nil
  "Maximum width of column in displayed database table, or nil if there is no limit.
If any cell value is longer, it will be cutted and terminated with ellipsis ('...')."
  :group 'sqlplus
  :tag "SQL*Plus Select Result Max Column Width"
  :type  '(choice integer (const nil)))

(defcustom sqlplus-format-output-tables-flag t
  "Non-nil means format result if it looks like database table."
  :group 'sqlplus
  :tag "SQL*Plus Format Output Table"
  :type '(boolean))

(defcustom sqlplus-kill-processes-without-query-on-exit-flag t
  "Non-nil means silently kill all SQL*Plus processes on Emacs exit."
  :group 'sqlplus
  :tag "SQL*Plus Kill Processes Without Query On Exit"
  :type '(boolean))

(defcustom sqlplus-multi-output-tables-default-flag t
  "Non-nil means render database table as set of adjacent tables so that they occupy all width of output window.
For screen space saving and user comfort."
  :group 'sqlplus
  :tag "SQL*Plus Multiple Tables In Output by Default"
  :type '(boolean))

(defcustom sqlplus-source-buffer-readonly-by-default-flag t
  "Non-nil means show database sources in read-only buffer."
  :group 'sqlplus
  :tag "SQL*Plus Source Buffer Read Only By Default"
  :type '(boolean))

(defcustom sqlplus-command "sqlplus"
  "SQL*Plus interpreter program."
  :group 'sqlplus
  :tag "SQL*Plus Command"
  :type '(string))

(defcustom sqlplus-history-dir nil
  "Directory of SQL*Plus command history (log) files, or nil (dont generate log files).
History file name has format '<connect-string>-history.txt'."
  :group 'sqlplus
  :tag "SQL*Plus History Dir"
  :type '(choice directory (const nil)))

(defvar sqlplus-session-file-extension "sqp")

(defcustom sqlplus-session-cache-dir nil
  "Directory of SQL*Plus input buffer files, or nil (dont save user session).
Session file name has format '<connect-string>.sqp'"
  :group 'sqlplus
  :tag "SQL*Plus History Dir"
  :type '(choice directory (const nil)))

(defcustom sqlplus-pagesize 200
  "Approximate number of records in query results.
If result has more rows, it will be cutted and terminated with '. . .' line."
  :group 'sqlplus
  :tag "SQL*Plus Max Rows Count"
  :type '(integer))

(defvar sqlplus-default-wrap "on")

(defcustom sqlplus-initial-strings
  (list "set sqlnumber off"
        "set tab off"
	"set linesize 4000"
        "set echo off"
        "set newpage 1"
        "set space 1"
        "set feedback 6"
        "set heading on"
        "set trimspool off"
        (format "set wrap %s" sqlplus-default-wrap)
        "set timing on"
	"set feedback on")
  "Initial commands to send to interpreter.
Customizing this variable is dangerous."
  :group 'sqlplus
  :tag "SQL*Plus Initial Strings"
  :type '(repeat string))

(defcustom sqlplus-table-col-separator " | "
  "Database table column separator (text-only terminals)."
  :group 'sqlplus
  :tag "SQL*Plus Table Col Separator"
  :type '(string))

(defcustom sqlplus-table-col-head-separator "-+-"
  "Database table header-column separator (text-only terminals)."
  :group 'sqlplus
  :tag "SQL*Plus Table Col Separator"
  :type '(string))

(defcustom sqlplus-html-output-file-name "$HOME/sqlplus_report.html"
  "Output file for HTML result."
  :group 'sqlplus
  :tag "SQL*Plus HTML Output File Name"
  :type '(file))

(defcustom sqlplus-html-output-encoding "iso-8859-1"
  "Encoding for SQL*Plus HTML output."
  :group 'sqlplus
  :tag "SQL*Plus HTML Output Encoding"
  :type '(string))

(defcustom sqlplus-html-output-sql t
  "Non-nil means put SQL*Plus command in head of HTML result."
  :group 'sqlplus
  :tag "SQL*Plus HTML Output Encoding"
  :type '(choice (const :tag "Elegant" 'elegant)
                 (const :tag "Simple" t)
                 (const :tag "No" nil)))

(defcustom sqlplus-html-output-header (concat (current-time-string) "<br><br>")
  "HTML header sexp (result must be string)."
  :group 'sqlplus
  :tag "SQL*Plus HTML Output Header"
  :type '(sexp))

(defcustom sqlplus-command-highlighting-percentage 7
  "SQL*Plus command highlighting percentage (0-100), only if sqlplus-command-highlighting-style is set."
  :group 'sqlplus
  :tag "SQL*Plus command highlighting percentage"
  :type '(integer))
  
(defcustom sqlplus-command-highlighting-style nil
  "How to highlight current command in sqlplus buffer."
  :group 'sqlplus
  :tag "SQL*Plud command highlighting style"
  :type '(choice (const :tag "Fringe" fringe)
		 (const :tag "Background" background)
		 (const :tag "Fringe and background" fringe-and-background)
		 (const :tag "None" nil)))

(defvar sqlplus-elegant-style window-system)

(defvar sqlplus-cs nil)

(defun sqlplus-shine-color (color percent)
  (when (equal color "unspecified-bg")
    (setq color (if (< percent 0) "white" "black")))
  (apply 'format "#%02x%02x%02x" 
         (mapcar (lambda (value)
                   (min 65535 (max 0 (* (+ (/ value 650) percent) 650))))
                 (color-values color))))

(defvar sqlplus-table-head-face 'sqlplus-table-head-face)
(defface sqlplus-table-head-face
  (list 
   (list '((class mono))
         '(:inherit default :weight bold :inverse-video t))
   (list '((background light))
         (append (list :inherit 'default :background (sqlplus-shine-color (face-background 'default) -70) :foreground (face-background 'default))
                 (when (and sqlplus-elegant-style (>= emacs-major-version 22)) '(:box (:style released-button)))))
   (list '((background dark))
         (append (list :inherit 'default :background (sqlplus-shine-color (face-background 'default) +70) :foreground (face-background 'default))
                 (when (and sqlplus-elegant-style (>= emacs-major-version 22)) '(:box (:style released-button)))))
   '(t (:inherit default)))
  "Face for table header"
  :group 'sqlplus)

(defvar sqlplus-table-even-rows-face 'sqlplus-table-even-rows-face)
(defface sqlplus-table-even-rows-face
  (list 
   (list '((class mono)) '())
   (list '((type tty)) '())
   (list '((background light))
         (append (list :inherit 'default :background (sqlplus-shine-color (face-background 'default) -20) :overline (face-background 'default))))
   (list '((background dark))
         (append (list :inherit 'default :background (sqlplus-shine-color (face-background 'default) +20) :overline (face-background 'default))))
   '(t ()))
  "Face for table even rows"
  :group 'sqlplus)

(defvar sqlplus-table-odd-rows-face 'sqlplus-table-odd-rows-face)
(defface sqlplus-table-odd-rows-face
  (list 
   (list '((class mono)) '(:inherit default))
   (list '((background light))
         (append (list :inherit 'default :background (sqlplus-shine-color (face-background 'default) -30) :overline (face-background 'default))))
   (list '((background dark))
         (append (list :inherit 'default :background (sqlplus-shine-color (face-background 'default) +30) :overline (face-background 'default))))
   '(t (:inherit default)))
  "Face for table even rows"
  :group 'sqlplus)

(defvar sqlplus-command-highlight-face 'sqlplus-command-highlight-face)
(defface sqlplus-command-highlight-face
  (list 
   '(((class mono)) ())
   '(((type tty)) ())
   (list '((background light))
         (append (list :background (sqlplus-shine-color (face-background 'default) (- sqlplus-command-highlighting-percentage)))))
   (list '((background dark))
         (append (list :background (sqlplus-shine-color (face-background 'default) sqlplus-command-highlighting-percentage))))
   '(t ()))
  "Face for highlighting command under point"
  :group 'sqlplus)

(defvar sqlplus-plsql-compilation-results-buffer-name "*PL/SQL Compilation*")

(defvar sqlplus-fan "|"
  "Local in input buffers")
(make-variable-buffer-local 'sqlplus-fan)

(defvar orcl-mode-map nil
  "Keymap used in Orcl mode.")

(define-minor-mode orcl-mode
  "Mode for executing SQL*Plus commands and scrolling results.

Mode Specific Bindings:

\\{orcl-mode-map}"
  nil                                   ; init value
  (" " (:eval sqlplus-fan) " " (:eval (connect-string-to-string))) ; mode indicator
  orcl-mode-map                                           ; keymap
  ;; body
  (setq sqlplus-fan "|")
  (unless (assq 'orcl-mode minor-mode-map-alist)
    (push (cons 'orcl-mode orcl-mode-map) minor-mode-map-alist)))

(defvar sqlplus-user-variables (make-hash-table :test 'equal))

(defvar sqlplus-user-variables-history nil)

(defvar sqlplus-get-source-history nil)

(defvar sqlplus-process-p nil
  "Non-nil (connect string) if current buffer is SQL*Plus process buffer.
Local in process buffer.")
(make-variable-buffer-local 'sqlplus-process-p)

(defvar sqlplus-command-seq 0
  "Sequence for command id within SQL*Plus connection.
Local in process buffer.")
(make-variable-buffer-local 'sqlplus-command-seq)

;;; :id - unique command identifier (from sequence, for session)
;;; :sql - content of command
;;; :dont-parse-result - process data online as it comes from sqlplus, with sqlplus-result-online or with :result-function function
;;; :result-function - function for processing sqlplus data; must have signature (context connect-string begin end interrupted);
;;;    if nil then it is sqlplus-result-online for :dont-parse-result set to non-nil and sqlplus-process-command-output for :dont-parse-result set to nil
;;; :current-command-input-buffer-name - buffer name from which command was initialized
(defvar sqlplus-command-contexts nil
  "Command options list, for current and enqueued commands, in chronological order.
Local in process buffer.")
(make-variable-buffer-local 'sqlplus-command-contexts)

(defvar sqlplus-connect-string nil
  "Local variable with connect-string for current buffer (input buffers, output buffer).")
(make-variable-buffer-local 'sqlplus-connect-string)

(defvar sqlplus-connect-strings-alist nil
  "Connect strings in format (CS . PASSWD), where PASSWD can be nil.")

(defvar sqlplus-connect-string-history nil)

(defvar sqlplus-prompt-prefix "SQL[")
(defvar sqlplus-prompt-suffix "]# ")

(defvar sqlplus-page-separator "@!%#!")

(defvar sqlplus-repfooter "##%@!")

(defvar sqlplus-mode-map nil
  "Keymap used in SQL*Plus mode.")

(defvar sqlplus-output-separator "@--"
  "String printed between sets of SQL*Plus command output.")

;;;  Markers -

(defvar sqlplus-buffer-mark (make-marker)
  "Marks the current SQL command in the SQL*Plus output buffer.
Local in output buffer.")
(make-variable-buffer-local 'sqlplus-buffer-mark)

(defvar sqlplus-region-beginning-pos nil
  "Marks the beginning of the region to sent to the SQL*Plus process.
Local in input buffer with sqlplus-mode.")
(make-variable-buffer-local 'sqlplus-region-beginning-pos)

(defvar sqlplus-region-end-pos nil
  "Marks the end of the region to sent to the SQL*Plus process.
Local in input buffer with sqlplus-mode.")
(make-variable-buffer-local 'sqlplus-region-end-pos)

(defvar sqlplus-connections-menu
  '("SQL*Plus"
    :filter sqlplus-connections-menu)
  "Menu for database connections")

(defconst sqlplus-kill-xpm "\
/* XPM */
static char * reload_page_xpm[] = {
\"24 24 100 2\",
\"  	c None\",
\". 	c #000000\",
\"+ 	c #2A5695\",
\"@ 	c #30609E\",
\"# 	c #3363A2\",
\"$ 	c #3969A6\",
\"% 	c #3D6BA6\",
\"& 	c #3C68A3\",
\"* 	c #35619C\",
\"= 	c #244F8D\",
\"- 	c #3364A3\",
\"; 	c #3162A1\",
\"> 	c #3867A4\",
\", 	c #3F6DA8\",
\"' 	c #4672AC\",
\") 	c #4B76AE\",
\"! 	c #4E78AF\",
\"~ 	c #537CB1\",
\"{ 	c #547DB0\",
\"] 	c #446BA1\",
\"^ 	c #2E5D9C\",
\"/ 	c #234F8C\",
\"( 	c #214C89\",
\"_ 	c #244E8C\",
\": 	c #3A649D\",
\"< 	c #517BB0\",
\"[ 	c #517BB1\",
\"} 	c #4874AD\",
\"| 	c #6086B7\",
\"1 	c #5F84B4\",
\"2 	c #4B71A6\",
\"3 	c #7B9BC4\",
\"4 	c #224C89\",
\"5 	c #3865A2\",
\"6 	c #406FAB\",
\"7 	c #436BA3\",
\"8 	c #648ABA\",
\"9 	c #4D78AF\",
\"0 	c #4B77AE\",
\"a 	c #6E91BE\",
\"b 	c #809EC6\",
\"c 	c #204A87\",
\"d 	c #4974AF\",
\"e 	c #2B5590\",
\"f 	c #6487B5\",
\"g 	c #678CBB\",
\"h 	c #3465A4\",
\"i 	c #84A1C8\",
\"j 	c #6D8FBA\",
\"k 	c #4F7AB0\",
\"l 	c #8BA7CB\",
\"m 	c #7E9DC5\",
\"n 	c #83A1C7\",
\"o 	c #91ACCE\",
\"p 	c #89A4C9\",
\"q 	c #8FA9CB\",
\"r 	c #85A2C7\",
\"s 	c #90ABCC\",
\"t 	c #3E6CA8\",
\"u 	c #87A3C8\",
\"v 	c #4B6DA1\",
\"w 	c #91ABCD\",
\"x 	c #3768A5\",
\"y 	c #8AA5C9\",
\"z 	c #2D5690\",
\"A 	c #204A86\",
\"B 	c #93ADCE\",
\"C 	c #7294BF\",
\"D 	c #6288B9\",
\"E 	c #86A3C8\",
\"F 	c #466EA3\",
\"G 	c #3864A1\",
\"H 	c #285390\",
\"I 	c #234E8C\",
\"J 	c #95AECF\",
\"K 	c #7493BC\",
\"L 	c #86A2C7\",
\"M 	c #7999C3\",
\"N 	c #5B82B5\",
\"O 	c #6C8EBB\",
\"P 	c #4B71A5\",
\"Q 	c #26508B\",
\"R 	c #2B5792\",
\"S 	c #305E9B\",
\"T 	c #31619F\",
\"U 	c #7895BD\",
\"V 	c #819DC3\",
\"W 	c #688DBB\",
\"X 	c #6288B8\",
\"Y 	c #5880B4\",
\"Z 	c #577FB3\",
\"` 	c #547DB2\",
\" .	c #416FAA\",
\"..	c #3564A2\",
\"+.	c #577AAB\",
\"@.	c #6286B6\",
\"#.	c #668BBA\",
\"$.	c #507AB0\",
\"%.	c #426EA8\",
\"&.	c #2F5B97\",
\"                                                \",
\"                                                \",
\"                                                \",
\"              . . . . . . .             .       \",
\"          . . + @ # $ % & * . .       . .       \",
\"        . = - ; @ > , ' ) ! ~ { . . . ] .       \",
\"        . ^ / ( _ . . . : < [ } | 1 2 3 .       \",
\"      . _ 4 5 6 .       . . 7 8 9 0 a b .       \",
\"      . c d . .             . e f g h i .       \",
\"      . . .   .               . j k h l .       \",
\"      .                     . f m n l o .       \",
\"                          . . . . . . . .       \",
\"      . . . . . . . .                           \",
\"      . p q q q r .                     .       \",
\"      . s , t u v .             .     . .       \",
\"      . w x | y z .             . . . A .       \",
\"      . B C 9 D E F . .       . G H I .         \",
\"      . J K L M N C O P . . . Q R S T .         \",
\"      . U . . . V W X | Y Z ` )  ....           \",
\"      . .       . . +.@.#.N $.%.&.. .           \",
\"      .             . . . . . . .               \",
\"                                                \",
\"                                                \",
\"                                                \"};
"
  "XPM format image used as Kill icon")

(defconst sqlplus-cancel-xpm "\
/* XPM */
static char * process_stop_xpm[] = {
\"24 24 197 2\",
\"  	c None\",
\". 	c #000000\",
\"+ 	c #C92B1E\",
\"@ 	c #DA432F\",
\"# 	c #E95941\",
\"$ 	c #F26B50\",
\"% 	c #ED6047\",
\"& 	c #DF4A35\",
\"* 	c #CE3324\",
\"= 	c #BF1D13\",
\"- 	c #EA5942\",
\"; 	c #EF563A\",
\"> 	c #F14D2C\",
\", 	c #F1431F\",
\"' 	c #F23A12\",
\") 	c #F2421C\",
\"! 	c #F24D2A\",
\"~ 	c #F15737\",
\"{ 	c #F0644A\",
\"] 	c #CF3121\",
\"^ 	c #D83828\",
\"/ 	c #ED5840\",
\"( 	c #EC3B1C\",
\"_ 	c #EE310B\",
\": 	c #F1350C\",
\"< 	c #F4380D\",
\"[ 	c #F53A0D\",
\"} 	c #F53B0D\",
\"| 	c #F4390D\",
\"1 	c #F2360C\",
\"2 	c #EF3A15\",
\"3 	c #F05A3D\",
\"4 	c #E44D37\",
\"5 	c #CD2B1E\",
\"6 	c #EA4D35\",
\"7 	c #E92D0C\",
\"8 	c #ED2F0B\",
\"9 	c #F0330C\",
\"0 	c #F3380D\",
\"a 	c #F63C0E\",
\"b 	c #F93F0F\",
\"c 	c #F9400F\",
\"d 	c #F73D0E\",
\"e 	c #F1340C\",
\"f 	c #EE300B\",
\"g 	c #EC482C\",
\"h 	c #E04532\",
\"i 	c #E84E3A\",
\"j 	c #E62A0E\",
\"k 	c #EA2B0A\",
\"l 	c #F83F0E\",
\"m 	c #FC4310\",
\"n 	c #FC4410\",
\"o 	c #F63B0E\",
\"p 	c #EB2C0A\",
\"q 	c #EB5139\",
\"r 	c #C8251A\",
\"s 	c #DD3D2E\",
\"t 	c #E5341D\",
\"u 	c #E62508\",
\"v 	c #F9BEB2\",
\"w 	c #FBCFC5\",
\"x 	c #F54C23\",
\"y 	c #F95125\",
\"z 	c #FDD4CB\",
\"A 	c #FABFB2\",
\"B 	c #E83013\",
\"C 	c #E84F3B\",
\"D 	c #E54737\",
\"E 	c #E22007\",
\"F 	c #E92A09\",
\"G 	c #FBD2CA\",
\"H 	c #FFFFFF\",
\"I 	c #FDDFD9\",
\"J 	c #F64E24\",
\"K 	c #FDE0D9\",
\"L 	c #E72609\",
\"M 	c #E7452F\",
\"N 	c #E33D2D\",
\"O 	c #E11E07\",
\"P 	c #E52308\",
\"Q 	c #E82809\",
\"R 	c #EC3F21\",
\"S 	c #FCDED8\",
\"T 	c #F55C37\",
\"U 	c #FCDFD8\",
\"V 	c #F04521\",
\"W 	c #EC2E0A\",
\"X 	c #E92909\",
\"Y 	c #E62408\",
\"Z 	c #E53823\",
\"` 	c #CE2B1F\",
\" .	c #C62018\",
\"..	c #E03120\",
\"+.	c #E01C06\",
\"@.	c #E32107\",
\"#.	c #ED4121\",
\"$.	c #FEF9F8\",
\"%.	c #E72709\",
\"&.	c #E42208\",
\"*.	c #E32D17\",
\"=.	c #D83729\",
\"-.	c #CB231B\",
\";.	c #DE2A1B\",
\">.	c #DE1A06\",
\",.	c #EE5135\",
\"'.	c #EF5335\",
\").	c #EC2D0A\",
\"!.	c #E82709\",
\"~.	c #E21F07\",
\"{.	c #E02511\",
\"].	c #DC392C\",
\"^.	c #BE1612\",
\"/.	c #DD2E21\",
\"(.	c #DC1705\",
\"_.	c #DF1B06\",
\":.	c #E42308\",
\"<.	c #E93A20\",
\"[.	c #FBDDD8\",
\"}.	c #EB3D20\",
\"|.	c #DF2A18\",
\"1.	c #D02A1F\",
\"2.	c #DC3328\",
\"3.	c #DA1404\",
\"4.	c #DD1805\",
\"5.	c #E3331E\",
\"6.	c #FADCD8\",
\"7.	c #FBDCD8\",
\"8.	c #EB4C34\",
\"9.	c #E6361F\",
\"0.	c #DD1905\",
\"a.	c #DF2F21\",
\"b.	c #C21A14\",
\"c.	c #DA3128\",
\"d.	c #D81408\",
\"e.	c #F7C9C4\",
\"f.	c #FADBD8\",
\"g.	c #E5341E\",
\"h.	c #E5351E\",
\"i.	c #F8CEC9\",
\"j.	c #DB1505\",
\"k.	c #DD3429\",
\"l.	c #C31613\",
\"m.	c #D9281F\",
\"n.	c #D71003\",
\"o.	c #D91304\",
\"p.	c #F3B5B0\",
\"q.	c #F7CDC9\",
\"r.	c #E12F1D\",
\"s.	c #DF1C06\",
\"t.	c #E2301D\",
\"u.	c #F4B6B0\",
\"v.	c #DC1605\",
\"w.	c #DB2317\",
\"x.	c #D2271F\",
\"y.	c #D1231D\",
\"z.	c #D61A10\",
\"A.	c #D60F03\",
\"B.	c #D81104\",
\"C.	c #DB1605\",
\"D.	c #D81204\",
\"E.	c #D81509\",
\"F.	c #DA2F26\",
\"G.	c #D52620\",
\"H.	c #D51A12\",
\"I.	c #D50D03\",
\"J.	c #D60E03\",
\"K.	c #D6170D\",
\"L.	c #D92B23\",
\"M.	c #BD100D\",
\"N.	c #AB0404\",
\"O.	c #CE1D19\",
\"P.	c #D6231C\",
\"Q.	c #D41008\",
\"R.	c #D40B02\",
\"S.	c #D40C02\",
\"T.	c #D50C03\",
\"U.	c #D40E05\",
\"V.	c #D62018\",
\"W.	c #D4251F\",
\"X.	c #B30A09\",
\"Y.	c #A20000\",
\"Z.	c #BC0F0E\",
\"`.	c #D2211E\",
\" +	c #D52520\",
\".+	c #D5201A\",
\"++	c #D41A14\",
\"@+	c #D51F19\",
\"#+	c #D62620\",
\"$+	c #D52420\",
\"%+	c #C51614\",
\"&+	c #A30101\",
\"*+	c #A30303\",
\"=+	c #AE0909\",
\"-+	c #BD0E0E\",
\";+	c #B30B0B\",
\">+	c #A30404\",
\"                                                \",
\"                . . . . . . .                   \",
\"            . . + @ # $ % & * . .               \",
\"          . = - ; > , ' ) ! ~ { ] .             \",
\"        . ^ / ( _ : < [ } | 1 2 3 4 .           \",
\"      . 5 6 7 8 9 0 a b c d | e f g h .         \",
\"      . i j k f : [ l m n c o 1 _ p q r .       \",
\"    . s t u k v w x l m n y z A _ p B C .       \",
\"    . D E u F G H I J b y K H w f k L M .       \",
\"    . N O P Q R S H I T K H U V W X Y Z ` .     \",
\"  .  ...+.@.u F #.S H $.H U V 8 k %.&.*.=..     \",
\"  . -.;.>.O &.L F ,.$.H $.'.).k !.P ~.{.]..     \",
\"  . ^./.(._.~.:.<.[.H $.H [.}.L P E +.|.1..     \",
\"    . 2.3.4._.5.6.H 7.8.7.H 6.9.~.+.0.a.b..     \",
\"    . c.d.3.(.e.H f.g.@.h.6.H i._.4.j.k..       \",
\"    . l.m.n.o.p.q.r._.s.s.t.e.u.v.3.w.x..       \",
\"      . y.z.A.B.o.j.C.(.(.v.j.3.D.E.F..         \",
\"        . G.H.I.J.n.B.B.B.B.n.A.K.L.M..         \",
\"        . N.O.P.Q.R.S.T.T.S.U.V.W.X..           \",
\"          . Y.Z.`. +.+++@+#+$+%+&+.             \",
\"            . . . *+=+-+;+>+Y.. .               \",
\"                  . . . . . .                   \",
\"                                                \",
\"                                                \"};
"
  "XPM format image used as Cancel icon")

(defconst sqlplus-rollback-xpm "\
/* XPM */
static char * rollback_xpm[] = {
\"24 24 228 2\",
\"  	c None\",
\". 	c #000000\",
\"+ 	c #F8F080\",
\"@ 	c #FEF57B\",
\"# 	c #FFF571\",
\"$ 	c #FFF164\",
\"% 	c #FFED58\",
\"& 	c #FFE748\",
\"* 	c #FEDE39\",
\"= 	c #F8F897\",
\"- 	c #FFFE96\",
\"; 	c #FFFA8A\",
\"> 	c #FFF67C\",
\", 	c #FFF16E\",
\"' 	c #FFEC62\",
\") 	c #FFE956\",
\"! 	c #FFE448\",
\"~ 	c #FFE03C\",
\"{ 	c #FFDD30\",
\"] 	c #FED821\",
\"^ 	c #F1CB15\",
\"/ 	c #FFFC92\",
\"( 	c #FFFC91\",
\"_ 	c #FFFC90\",
\": 	c #FFFB8D\",
\"< 	c #FFF67D\",
\"[ 	c #FFEB5E\",
\"} 	c #FFEA5B\",
\"| 	c #FFE958\",
\"1 	c #FFE855\",
\"2 	c #FFE752\",
\"3 	c #FDD41C\",
\"4 	c #FDD319\",
\"5 	c #FDD416\",
\"6 	c #FFFF9D\",
\"7 	c #FFFF99\",
\"8 	c #FFFD94\",
\"9 	c #FFFA89\",
\"0 	c #FFDC2F\",
\"a 	c #FED315\",
\"b 	c #FFD808\",
\"c 	c #FFFC9F\",
\"d 	c #FFFE99\",
\"e 	c #FFDF3B\",
\"f 	c #F7C909\",
\"g 	c #F8EA86\",
\"h 	c #FEFCB7\",
\"i 	c #FFFDA6\",
\"j 	c #FFFA91\",
\"k 	c #FFF681\",
\"l 	c #FFF171\",
\"m 	c #FFED64\",
\"n 	c #FFE44A\",
\"o 	c #FFE03D\",
\"p 	c #FEDB2F\",
\"q 	c #F9D21E\",
\"r 	c #E9BC0F\",
\"s 	c #CE9C02\",
\"t 	c #F3E36A\",
\"u 	c #FCF899\",
\"v 	c #FFFCA3\",
\"w 	c #FEF694\",
\"x 	c #FFF284\",
\"y 	c #FFEE71\",
\"z 	c #FFEA62\",
\"A 	c #FDDC40\",
\"B 	c #F8D22F\",
\"C 	c #F1C61B\",
\"D 	c #DDAD0A\",
\"E 	c #CC9A02\",
\"F 	c #C89500\",
\"G 	c #F4EA77\",
\"H 	c #F7EF7F\",
\"I 	c #FFF16A\",
\"J 	c #FFEF68\",
\"K 	c #FFEE66\",
\"L 	c #FED622\",
\"M 	c #FED51E\",
\"N 	c #FED419\",
\"O 	c #E9B90E\",
\"P 	c #E7B509\",
\"Q 	c #D4A202\",
\"R 	c #CA9700\",
\"S 	c #F6E67C\",
\"T 	c #F3E67F\",
\"U 	c #FCEE7A\",
\"V 	c #FDEB66\",
\"W 	c #FEE44E\",
\"X 	c #FED313\",
\"Y 	c #FDCA03\",
\"Z 	c #F2BE01\",
\"` 	c #D4A60D\",
\" .	c #D4A206\",
\"..	c #D19C00\",
\"+.	c #CF9800\",
\"@.	c #E3AF02\",
\"#.	c #F9EB81\",
\"$.	c #FBF096\",
\"%.	c #F9E67C\",
\"&.	c #F8DC5F\",
\"*.	c #F8D548\",
\"=.	c #F9D02D\",
\"-.	c #F9C915\",
\";.	c #F7C104\",
\">.	c #EEB606\",
\",.	c #E9B704\",
\"'.	c #DEAE08\",
\").	c #414D7B\",
\"!.	c #3C5CA2\",
\"~.	c #3A65B3\",
\"{.	c #3668BB\",
\"].	c #325EAF\",
\"^.	c #F3E46E\",
\"/.	c #FCFA9B\",
\"(.	c #FFF89C\",
\"_.	c #FDEC81\",
\":.	c #FCE668\",
\"<.	c #FDDF4E\",
\"[.	c #FCDA3C\",
\"}.	c #FCD52E\",
\"|.	c #FAD026\",
\"1.	c #4662A2\",
\"2.	c #465A8D\",
\"3.	c #3F6CBA\",
\"4.	c #3A68B7\",
\"5.	c #2E529E\",
\"6.	c #2655AC\",
\"7.	c #F0DC69\",
\"8.	c #FBF78C\",
\"9.	c #FFF880\",
\"0.	c #FFF06B\",
\"a.	c #FFE03E\",
\"b.	c #FFD828\",
\"c.	c #FED015\",
\"d.	c #F5C40A\",
\"e.	c #4B70B4\",
\"f.	c #4870B7\",
\"g.	c #3C5CA1\",
\"h.	c #4070BF\",
\"i.	c #3759A0\",
\"j.	c #1D469C\",
\"k.	c #214493\",
\"l.	c #F2DD6C\",
\"m.	c #F8EB7E\",
\"n.	c #FBEE7A\",
\"o.	c #FBE461\",
\"p.	c #FADB48\",
\"q.	c #FBD631\",
\"r.	c #FED10F\",
\"s.	c #FECD07\",
\"t.	c #F1BD00\",
\"u.	c #456AAE\",
\"v.	c #4C7ECA\",
\"w.	c #487AC8\",
\"x.	c #35528F\",
\"y.	c #1B4294\",
\"z.	c #1B4193\",
\"A.	c #F9EA83\",
\"B.	c #FCF08E\",
\"C.	c #F6E16E\",
\"D.	c #F4D559\",
\"E.	c #F5CF45\",
\"F.	c #F6CB2E\",
\"G.	c #F8C611\",
\"H.	c #F6C005\",
\"I.	c #E8B300\",
\"J.	c #4268AE\",
\"K.	c #4375C4\",
\"L.	c #3F71C1\",
\"M.	c #33569B\",
\"N.	c #173F94\",
\"O.	c #183A8B\",
\"P.	c #F3E36E\",
\"Q.	c #FCF7A1\",
\"R.	c #FEF9A1\",
\"S.	c #FEEE7D\",
\"T.	c #FCE360\",
\"U.	c #FAD946\",
\"V.	c #F9D132\",
\"W.	c #F8CD26\",
\"X.	c #F7CA20\",
\"Y.	c #3B589A\",
\"Z.	c #395FA9\",
\"`.	c #3359A5\",
\" +	c #3056A3\",
\".+	c #2B468D\",
\"++	c #0A3897\",
\"@+	c #E6D465\",
\"#+	c #FDFA90\",
\"$+	c #FFF885\",
\"%+	c #FFF074\",
\"&+	c #FFEA60\",
\"*+	c #FFE246\",
\"=+	c #FFDC31\",
\"-+	c #FED51F\",
\";+	c #F7CB14\",
\">+	c #173788\",
\",+	c #063494\",
\"'+	c #E8DE7B\",
\")+	c #FFFA86\",
\"!+	c #FFF26A\",
\"~+	c #FFE84F\",
\"{+	c #FFD415\",
\"]+	c #FDCC04\",
\"^+	c #F3C001\",
\"/+	c #EBB600\",
\"(+	c #E3AF01\",
\"_+	c #D7A100\",
\":+	c #2D3E7F\",
\"<+	c #033396\",
\"[+	c #CFB954\",
\"}+	c #DBC347\",
\"|+	c #DEBF2C\",
\"1+	c #DFB718\",
\"2+	c #DFB206\",
\"3+	c #D6A505\",
\"4+	c #C6970A\",
\"5+	c #B48413\",
\"6+	c #374682\",
\"7+	c #023398\",
\"8+	c #0E3287\",
\"9+	c #253775\",
\"0+	c #05318F\",
\"a+	c #10358B\",
\"b+	c #183888\",
\"c+	c #053495\",
\"d+	c #0E348D\",
\"e+	c #183585\",
\"        . . . . . . .                           \",
\"    . . + @ # $ % & * . . .                     \",
\"  . = - ; > , ' ) ! ~ { ] ^ .                   \",
\". / ( _ : ; < [ } | 1 2 3 4 5 .                 \",
\". 6 7 8 9 > , ' ) ! ~ 0 ] a b .                 \",
\". c d 8 9 > , ' ) ! e 0 ] a f .                 \",
\". g h i j k l m | n o p q r s .                 \",
\". t u v w x y z 2 A B C D E F .                 \",
\". G H I J K L M N O P Q R F F .                 \",
\". S T U V W p X Y Z `  ...+.@.. . . . .         \",
\". #.$.%.&.*.=.-.;.>.. . ,.'.. ).!.~.{.]..       \",
\". ^./.(._.:.<.[.}.|.. 1.. . 2.3.4.. . 5.6..     \",
\". 7.8.9.0.) a.b.c.d.. e.f.g.h.i..     . j.k..   \",
\". l.m.n.o.p.q.r.s.t.. u.v.w.x..       . y.z..   \",
\". A.B.C.D.E.F.G.H.I.. J.K.L.M..       . N.O..   \",
\". P.Q.R.S.T.U.V.W.X.. Y.Z.`. +.+.     . ++.     \",
\". @+#+$+%+&+*+=+-+;+. . . . . . .   . >+,+.     \",
\"  . '+)+!+~+{ {+]+^+/+(+_+.       . :+<+.       \",
\"    . . [+}+|+1+2+3+4+5+.       . 6+7+8+.       \",
\"        . . . . . . . .       . 9+0+a+.         \",
\"                            . b+c+d+.           \",
\"                            . e+. .             \",
\"                              .                 \",
\"                                                \"};
"
  "XPM format image used as Rollback icon")

(defconst sqlplus-commit-xpm "\
/* XPM */
static char * commit_xpm[] = {
\"24 24 276 2\",
\"  	c None\",
\". 	c #000000\",
\"+ 	c #FDF57D\",
\"@ 	c #FFF676\",
\"# 	c #FFF36C\",
\"$ 	c #FFF05D\",
\"% 	c #FFEB51\",
\"& 	c #FFE445\",
\"* 	c #FDDC35\",
\"= 	c #EFEA85\",
\"- 	c #FBF68D\",
\"; 	c #FCF482\",
\"> 	c #FCF178\",
\", 	c #FCEE6E\",
\"' 	c #FCEB66\",
\") 	c #FCE85B\",
\"! 	c #FCE551\",
\"~ 	c #FDE147\",
\"{ 	c #FDDF3D\",
\"] 	c #FEDD2D\",
\"^ 	c #FCD621\",
\"/ 	c #E5BF16\",
\"( 	c #D8D479\",
\"_ 	c #FCF587\",
\": 	c #FAEF78\",
\"< 	c #FAEA6B\",
\"[ 	c #FAEA6A\",
\"} 	c #FAE968\",
\"| 	c #FAE967\",
\"1 	c #FAE865\",
\"2 	c #FAE864\",
\"3 	c #FDDD3C\",
\"4 	c #FED621\",
\"5 	c #FFD51D\",
\"6 	c #FFD51B\",
\"7 	c #FFD519\",
\"8 	c #D8B82B\",
\"9 	c #FCF790\",
\"0 	c #FBF587\",
\"a 	c #F8EF7D\",
\"b 	c #F8EC75\",
\"c 	c #F7E86B\",
\"d 	c #F8E868\",
\"e 	c #F9E663\",
\"f 	c #F9E45A\",
\"g 	c #F9E253\",
\"h 	c #F9E04C\",
\"i 	c #FBDD40\",
\"j 	c #FBDB38\",
\"k 	c #FAD933\",
\"l 	c #FAD529\",
\"m 	c #FDD810\",
\"n 	c #FFFD9E\",
\"o 	c #FFFF9A\",
\"p 	c #FFFE96\",
\"q 	c #FFFB8C\",
\"r 	c #FFF781\",
\"s 	c #FFF375\",
\"t 	c #FFEF69\",
\"u 	c #FFEA5B\",
\"v 	c #FFE750\",
\"w 	c #FFE345\",
\"x 	c #FFDF38\",
\"y 	c #FFDB2B\",
\"z 	c #FFD81F\",
\"A 	c #FFD313\",
\"B 	c #FBD007\",
\"C 	c #FBF090\",
\"D 	c #FFFDAE\",
\"E 	c #FFFEA2\",
\"F 	c #FFFA8C\",
\"G 	c #FFF780\",
\"H 	c #F6CA11\",
\"I 	c #E1AF03\",
\"J 	c #F4E36D\",
\"K 	c #FCF7A4\",
\"L 	c #FFFEBB\",
\"M 	c #FEFAA6\",
\"N 	c #FFF990\",
\"O 	c #FFF57E\",
\"P 	c #FFEE6F\",
\"Q 	c #FFEB61\",
\"R 	c #FFE856\",
\"S 	c #FFE34A\",
\"T 	c #FBDD44\",
\"U 	c #F7D535\",
\"V 	c #EBBF13\",
\"W 	c #D5A406\",
\"X 	c #C99500\",
\"Y 	c #F0DC5F\",
\"Z 	c #F3E772\",
\"` 	c #F7EC76\",
\" .	c #F6E56D\",
\"..	c #F6E369\",
\"+.	c #F6E264\",
\"@.	c #F5DF5C\",
\"#.	c #F3DB53\",
\"$.	c #F3D849\",
\"%.	c #EFD245\",
\"&.	c #ECCE3F\",
\"*.	c #E3B91F\",
\"=.	c #D3A40B\",
\"-.	c #C99600\",
\";.	c #C69200\",
\">.	c #EED95E\",
\",.	c #EDDA60\",
\"'.	c #F1DF64\",
\").	c #F2DF5E\",
\"!.	c #F2DD57\",
\"~.	c #F2D94E\",
\"{.	c #F2D644\",
\"].	c #EFD038\",
\"^.	c #ECCB34\",
\"/.	c #E6C430\",
\"(.	c #DFB71F\",
\"_.	c #D9AD17\",
\":.	c #CC9907\",
\"<.	c #C69000\",
\"[.	c #D39E00\",
\"}.	c #BB1503\",
\"|.	c #F9EA7D\",
\"1.	c #F6E57A\",
\"2.	c #F5E370\",
\"3.	c #F5DE62\",
\"4.	c #F9DF52\",
\"5.	c #FBDB3E\",
\"6.	c #FCD526\",
\"7.	c #FCCE0F\",
\"8.	c #F7C50A\",
\"9.	c #EEBA08\",
\"0.	c #E2AB03\",
\"a.	c #D7A000\",
\"b.	c #D59D00\",
\"c.	c #DFA901\",
\"d.	c #E7B402\",
\"e.	c #C91800\",
\"f.	c #F6E676\",
\"g.	c #FCF4A1\",
\"h.	c #FDF096\",
\"i.	c #FAE167\",
\"j.	c #F7D64F\",
\"k.	c #F7CF38\",
\"l.	c #F7CB26\",
\"m.	c #F6BF0C\",
\"n.	c #F1B905\",
\"o.	c #ECB309\",
\"p.	c #EBB60A\",
\"q.	c #F0BF0B\",
\"r.	c #F3C206\",
\"s.	c #E5B201\",
\"t.	c #CF9C01\",
\"u.	c #C21602\",
\"v.	c #C21703\",
\"w.	c #F2E067\",
\"x.	c #FBF78F\",
\"y.	c #FEF28A\",
\"z.	c #FEED74\",
\"A.	c #FFE85F\",
\"B.	c #FFE24D\",
\"C.	c #FFDE3A\",
\"D.	c #FED92F\",
\"E.	c #FCD325\",
\"F.	c #F8CD1A\",
\"G.	c #EDBD0A\",
\"H.	c #D9A701\",
\"I.	c #C79200\",
\"J.	c #D11D00\",
\"K.	c #EFDA64\",
\"L.	c #F7EF7F\",
\"M.	c #FCF47F\",
\"N.	c #FDEE6C\",
\"O.	c #FDE85B\",
\"P.	c #FDE249\",
\"Q.	c #FDDC36\",
\"R.	c #FCD423\",
\"S.	c #F9CC14\",
\"T.	c #F0C10E\",
\"U.	c #E6B507\",
\"V.	c #DCA900\",
\"W.	c #D29F00\",
\"X.	c #C69400\",
\"Y.	c #C99200\",
\"Z.	c #CC1B02\",
\"`.	c #C61A04\",
\" +	c #E1CF5F\",
\".+	c #EAD862\",
\"++	c #ECDB63\",
\"@+	c #EFDC5E\",
\"#+	c #EFD955\",
\"$+	c #EFD74D\",
\"%+	c #EFD444\",
\"&+	c #F0D23E\",
\"*+	c #EECE37\",
\"=+	c #E8C731\",
\"-+	c #E0B922\",
\";+	c #D09E03\",
\">+	c #CB9700\",
\",+	c #C39100\",
\"'+	c #C99400\",
\")+	c #E12400\",
\"!+	c #F2E47C\",
\"~+	c #F8ED8C\",
\"{+	c #F4E171\",
\"]+	c #F0D65B\",
\"^+	c #F0D24F\",
\"/+	c #F1CF43\",
\"(+	c #F2CD34\",
\"_+	c #F2C824\",
\":+	c #EEC527\",
\"<+	c #E7BD23\",
\"[+	c #DFAC12\",
\"}+	c #DAA203\",
\"|+	c #E5B202\",
\"1+	c #EDBA01\",
\"2+	c #D69F00\",
\"3+	c #D21E01\",
\"4+	c #D01C00\",
\"5+	c #F2E16A\",
\"6+	c #FBF59D\",
\"7+	c #FEFBAA\",
\"8+	c #FEF084\",
\"9+	c #FCE567\",
\"0+	c #FBDD50\",
\"a+	c #F8D23B\",
\"b+	c #F8CD28\",
\"c+	c #EEB51C\",
\"d+	c #DA8A13\",
\"e+	c #E29A16\",
\"f+	c #EDB111\",
\"g+	c #E5AE08\",
\"h+	c #D19C01\",
\"i+	c #C79400\",
\"j+	c #BF1603\",
\"k+	c #DD2300\",
\"l+	c #E6D261\",
\"m+	c #FCF88C\",
\"n+	c #FFF27A\",
\"o+	c #FFEC6A\",
\"p+	c #FFE655\",
\"q+	c #FFE041\",
\"r+	c #FFDA2B\",
\"s+	c #E49D14\",
\"t+	c #BA4F02\",
\"u+	c #BB6A00\",
\"v+	c #B37102\",
\"w+	c #DD2200\",
\"x+	c #CA1B02\",
\"y+	c #E6DB78\",
\"z+	c #FEFB8B\",
\"A+	c #FFF470\",
\"B+	c #FFEA56\",
\"C+	c #FFE13E\",
\"D+	c #FFDA24\",
\"E+	c #FECF0A\",
\"F+	c #F5BE01\",
\"G+	c #D37800\",
\"H+	c #D72000\",
\"I+	c #C61802\",
\"J+	c #EBD55C\",
\"K+	c #FCE353\",
\"L+	c #FFE33E\",
\"M+	c #FFDB26\",
\"N+	c #FFD20B\",
\"O+	c #FCCB01\",
\"P+	c #F0B900\",
\"Q+	c #D47D00\",
\"R+	c #E42500\",
\"S+	c #EB2900\",
\"T+	c #DF2301\",
\"U+	c #E82700\",
\"V+	c #D31F04\",
\"W+	c #C71F01\",
\"X+	c #EA2800\",
\"Y+	c #E92800\",
\"Z+	c #DD2301\",
\"`+	c #E22501\",
\"          . . . . . . .                         \",
\"    . . . + @ # $ % & * . . .                   \",
\"  . = - ; > , ' ) ! ~ { ] ^ / .                 \",
\". ( _ : < [ } | 1 2 3 4 5 6 7 8 .               \",
\". 9 0 a b c d e f g h i j k l m .               \",
\". n o p q r s t u v w x y z A B .               \",
\". C D E F G s t u v w x y z H I .               \",
\". J K L M N O P Q R S T U V W X .               \",
\". Y Z `  ...+.@.#.$.%.&.*.=.-.;..         . .   \",
\". >.,.'.).!.~.{.].^./.(._.:.<.[..       . }..   \",
\". |.1.2.3.4.5.6.7.8.9.0.a.b.c.d..       . e..   \",
\". f.g.h.i.j.k.l.m.n.o.p.q.r.s.t..     . u.v..   \",
\". w.x.n y.z.A.B.C.D.E.F.G.H.-.I..     . J..     \",
\". K.L.M.N.O.P.Q.R.S.T.U.V.W.X.Y..   . Z.`..     \",
\".  +.+++@+#+$+%+&+*+=+-+;+>+,+'+.   . )+.       \",
\". !+~+{+]+^+/+(+_+:+<+[+}+|+1+2+. . 3+4+.       \",
\". 5+6+7+8+9+0+a+b+c+d+e+f+g+h+i+. j+k+.         \",
\". l+m+q n+o+p+q+r+s+. . . t+u+v+. w+x+.         \",
\"  . y+z+A+B+C+D+E+F+G+. H+. . . I+)+.           \",
\"    . . J+K+L+M+N+O+P+Q+. R+S+T+U+V+.           \",
\"        . . . . . . . . . . W+X+Y+.             \",
\"                            . Z+`+.             \",
\"                              . .               \",
\"                                .               \"};
"
  "XPM format image used as Commit icon")

(defconst plsql-prev-mark-xpm "\
/* XPM */
static char * go_previous_xpm[] = {
\"24 24 59 1\",
\" 	c None\",
\".	c #000000\",
\"+	c #355D96\",
\"@	c #3C639B\",
\"#	c #6E92BF\",
\"$	c #41679D\",
\"%	c #6990BE\",
\"&	c #6D94C2\",
\"*	c #456DA2\",
\"=	c #628BBC\",
\"-	c #4D7BB4\",
\";	c #6991C0\",
\">	c #4971A6\",
\",	c #5D87BA\",
\"'	c #4B7BB3\",
\")	c #4979B3\",
\"!	c #5884B9\",
\"~	c #638CBC\",
\"{	c #638BBC\",
\"]	c #6089BA\",
\"^	c #4B73A9\",
\"/	c #5883B8\",
\"(	c #4A7AB3\",
\"_	c #618ABB\",
\":	c #4C74AB\",
\"<	c #547FB5\",
\"[	c #4972A9\",
\"}	c #4D79B1\",
\"|	c #4171AD\",
\"1	c #4071AD\",
\"2	c #4070AD\",
\"3	c #4171AC\",
\"4	c #4071AC\",
\"5	c #4070AC\",
\"6	c #3F70AC\",
\"7	c #3F70AB\",
\"8	c #406FAC\",
\"9	c #5781B5\",
\"0	c #4A74AC\",
\"a	c #3E6CA8\",
\"b	c #3465A4\",
\"c	c #4E78AF\",
\"d	c #446FA8\",
\"e	c #4A75AD\",
\"f	c #3F6CA6\",
\"g	c #3C6BA7\",
\"h	c #3B6BA7\",
\"i	c #4471AB\",
\"j	c #4572AB\",
\"k	c #4672AC\",
\"l	c #4571AB\",
\"m	c #3A68A3\",
\"n	c #3B6AA7\",
\"o	c #406EA9\",
\"p	c #3564A0\",
\"q	c #3868A6\",
\"r	c #305E9D\",
\"s	c #3767A5\",
\"t	c #2E5D9B\",
\"                        \",
\"                        \",
\"                        \",
\"            ..          \",
\"           .+.          \",
\"          .@#.          \",
\"         .$%&.          \",
\"        .*=-;.........  \",
\"       .>,')!~{{{{{~].  \",
\"      .^/()))(((((('_.  \",
\"     .:<)))))))))))),.  \",
\"    .[}|1123455567589.  \",
\"     .0abbbbbbbbbbbbc.  \",
\"      .dabbbbbbbbbbbe.  \",
\"       .fgbbhijjjjjkl.  \",
\"        .mnbo.........  \",
\"         .pqh.          \",
\"          .rs.          \",
\"           .t.          \",
\"            ..          \",
\"             .          \",
\"                        \",
\"                        \",
\"                        \"};
"
  "XPM format image used as Previous Mark icon")

(defconst plsql-next-mark-xpm "\
/* XPM */
static char * go_next_xpm[] = {
\"24 24 63 1\",
\" 	c None\",
\".	c #000000\",
\"+	c #365F97\",
\"@	c #6B8FBE\",
\"#	c #41689E\",
\"$	c #6990BF\",
\"%	c #466EA4\",
\"&	c #678EBD\",
\"*	c #4E7DB5\",
\"=	c #638CBC\",
\"-	c #4B72A7\",
\";	c #5B83B5\",
\">	c #628BBB\",
\",	c #5A86BA\",
\"'	c #4979B3\",
\")	c #4B7AB3\",
\"!	c #5E87B9\",
\"~	c #4E76AA\",
\"{	c #5B84B8\",
\"]	c #4E7CB5\",
\"^	c #4A7AB3\",
\"/	c #5883B7\",
\"(	c #5178AD\",
\"_	c #5982B6\",
\":	c #4C7BB4\",
\"<	c #537FB5\",
\"[	c #5079AE\",
\"}	c #507BB0\",
\"|	c #4272AD\",
\"1	c #4070AC\",
\"2	c #3F70AB\",
\"3	c #3F70AC\",
\"4	c #4071AC\",
\"5	c #4171AC\",
\"6	c #4070AD\",
\"7	c #4071AD\",
\"8	c #4171AD\",
\"9	c #4D79B1\",
\"0	c #4E76AD\",
\"a	c #4872AA\",
\"b	c #3767A5\",
\"c	c #3465A4\",
\"d	c #3D6CA8\",
\"e	c #4C76AD\",
\"f	c #2B548E\",
\"g	c #446FA8\",
\"h	c #3C6BA7\",
\"i	c #4772AA\",
\"j	c #29528E\",
\"k	c #3F6CA6\",
\"l	c #4471AB\",
\"m	c #4371AB\",
\"n	c #3B6BA7\",
\"o	c #416EA8\",
\"p	c #3F6CA7\",
\"q	c #3A69A6\",
\"r	c #3C6AA5\",
\"s	c #3B6AA5\",
\"t	c #3868A6\",
\"u	c #3765A2\",
\"v	c #3666A3\",
\"w	c #32619F\",
\"x	c #2F5D9B\",
\"                        \",
\"                        \",
\"                        \",
\"           ..           \",
\"           .+.          \",
\"           .@#.         \",
\"           .$$%.        \",
\"   .........&*=-.       \",
\"   .;>>>>>>=,')!~.      \",
\"   .{]^^^^^^''''/(.     \",
\"   ._:'''''''''''<[.    \",
\"   .}|12311145677890.   \",
\"   .abcccccccccccde.    \",
\"   .gbcccccccccchi.     \",
\"   .klmlllllhccno.      \",
\"   .........pcqr.       \",
\"           .stu.        \",
\"           .vw.         \",
\"           .x.          \",
\"           ..           \",
\"           .            \",
\"                        \",
\"                        \",
\"                        \"};
"
  "XPM format image used as Next Mark icon")

(defconst sqlplus-kill-image
  (create-image sqlplus-kill-xpm 'xpm t))

(defconst sqlplus-cancel-image
  (create-image sqlplus-cancel-xpm 'xpm t))

(defconst sqlplus-commit-image
  (create-image sqlplus-commit-xpm 'xpm t))

(defconst sqlplus-rollback-image
  (create-image sqlplus-rollback-xpm 'xpm t))

(defconst plsql-prev-mark-image
  (create-image plsql-prev-mark-xpm 'xpm t))

(defconst plsql-next-mark-image
  (create-image plsql-next-mark-xpm 'xpm t))

(defvar sqlplus-mode-syntax-table nil
  "Syntax table used while in sqlplus-mode.")

(defvar sqlplus-suppress-show-output-buffer nil)

;; Local in input buffers
(defvar sqlplus-font-lock-keywords-1 nil)
(make-variable-buffer-local 'sqlplus-font-lock-keywords-1)
(defvar sqlplus-font-lock-keywords-2 nil)
(make-variable-buffer-local 'sqlplus-font-lock-keywords-2)
(defvar sqlplus-font-lock-keywords-3 nil)
(make-variable-buffer-local 'sqlplus-font-lock-keywords-3)

(defvar sqlplus-font-lock-defaults '((sqlplus-font-lock-keywords-1 sqlplus-font-lock-keywords-2 sqlplus-font-lock-keywords-3) nil t nil nil))

(defvar sqlplus-oracle-extra-builtin-functions-re
  (concat "\\b"
          (regexp-opt '("acos" "asciistr" "asin" "atan" "atan2" "bfilename" "bin_to_num" "bitand" "cardinality" "cast" "coalesce" "collect"
                        "compose" "corr" "corr_s" "corr_k" "covar_pop" "covar_samp" "cume_dist" "current_date" "current_timestamp" "cv"
                        "dbtimezone" "decompose" "dense_rank" "depth" "deref" "empty_blob, empty_clob" "existsnode" "extract"
                        "extractvalue" "first" "first_value" "from_tz" "group_id" "grouping" "grouping_id" "iteration_number"
                        "lag" "last" "last_value" "lead" "lnnvl" "localtimestamp" "make_ref" "median" "nanvl" "nchr" "nls_charset_decl_len"
                        "nls_charset_id" "nls_charset_name" "ntile" "nullif" "numtodsinterval" "numtoyminterval" "nvl2" "ora_hash" "path"
                        "percent_rank" "percentile_cont" "percentile_disc" "powermultiset" "powermultiset_by_cardinality" "presentnnv"
                        "presentv" "previous" "rank" "ratio_to_report" "rawtonhex" "ref" "reftohex" "regexp_instr" "regexp_replace"
                        "regexp_substr" "regr_slope" "regr_intercept" "regr_count" "regr_r2" "regr_avgx" "regr_avgy" "regr_sxx" "regr_syy"
                        "regr_sxy" "remainder" "row_number" "rowidtonchar" "scn_to_timestamp" "sessiontimezone" "stats_binomial_test"
                        "stats_crosstab" "stats_f_test" "stats_ks_test" "stats_mode" "stats_mw_test" "stats_one_way_anova" "stats_t_test_one"
                        "stats_t_test_paired" "stats_t_test_indep" "stats_t_test_indepu" "stats_wsr_test" "stddev_pop" "stddev_samp"
                        "sys_connect_by_path" "sys_context" "sys_dburigen" "sys_extract_utc" "sys_guid" "sys_typeid" "sys_xmlagg" "sys_xmlgen"
                        "systimestamp" "timestamp_to_scn" "to_binary_double" "to_binary_float" "to_clob" "to_dsinterval" "to_lob" "to_nchar"
                        "to_nclob" "to_timestamp" "to_timestamp_tz" "to_yminterval" "treat" "tz_offset" "unistr" "updatexml" "value" "var_pop"
                        "var_samp" "width_bucket" "xmlagg" "xmlcolattval" "xmlconcat" "xmlelement" "xmlforest" "xmlsequence" "xmltransform") t)
          "\\b"))
(defvar sqlplus-oracle-extra-warning-words-re
  (concat "\\b"
          (regexp-opt '("access_into_null" "case_not_found" "collection_is_null" "rowtype_mismatch"
                        "self_is_null" "subscript_beyond_count" "subscript_outside_limit" "sys_invalid_rowid") t)
          "\\b"))
(defvar sqlplus-oracle-extra-keywords-re
  (concat "\\b\\("
          "\\(at\\s-+local\\|at\\s-+time\\s-+zone\\|to\\s-+second\\|to\\s-+month\\|is\\s-+present\\|a\\s-+set\\)\\|"
          (regexp-opt '("case" "nan" "infinite" "equals_path" "empty" "likec" "like2" "like4" "member"
                        "regexp_like" "submultiset" "under_path" "mlslabel") t)
          "\\)\\b"))
(defvar sqlplus-oracle-extra-pseudocolumns-re
  (concat "\\b"
          (regexp-opt '("connect_by_iscycle" "connect_by_isleaf" "versions_starttime" "versions_startscn"
                        "versions_endtime" "versions_endscn" "versions_xid" "versions_operation" "object_id" "object_value" "ora_rowscn"
                        "xmldata") t)
          "\\b"))
(defvar sqlplus-oracle-plsql-extra-reserved-words-re
  (concat "\\b"
          (regexp-opt '("array" "at" "authid" "bulk" "char_base" "day" "do" "extends" "forall" "heap" "hour"
                        "interface" "isolation" "java" "limited" "minute" "mlslabel" "month" "natural" "naturaln" "nocopy" "number_base"
                        "ocirowid" "opaque" "operator" "organization" "pls_integer" "positive" "positiven" "range" "record" "release" "reverse"
                        "rowtype" "second" "separate" "space" "sql" "timezone_region" "timezone_abbr" "timezone_minute" "timezone_hour" "year"
                        "zone") t)
          "\\b"))
(defvar sqlplus-oracle-extra-types-re
  (concat "\\b"
          (regexp-opt '("nvarchar2" "binary_float" "binary_double" "timestamp" "interval" "interval_day" "urowid" "nchar" "clob" "nclob" "bfile") t)
          "\\b"))

(defvar sqlplus-commands-regexp-1 nil)
(defvar sqlplus-commands-regexp-23 nil)
(defvar sqlplus-system-variables-regexp-1 nil)
(defvar sqlplus-system-variables-regexp-23 nil)
(defvar sqlplus-v22-commands-font-lock-keywords-1 nil)
(defvar sqlplus-v22-commands-font-lock-keywords-23 nil)
(defvar font-lock-sqlplus-face nil)

(defvar sqlplus-output-buffer-keymap nil
  "Local in output buffer.")
(make-variable-buffer-local 'sqlplus-output-buffer-keymap)

(defvar sqlplus-kill-function-inhibitor nil)

(defvar sqlplus-slip-separator-width 2
  "Only for classic table style.")

(defvar sqlplus-user-string-history nil)

(defvar sqlplus-object-types '( "CONSUMER GROUP" "SEQUENCE" "SCHEDULE" "PROCEDURE" "OPERATOR" "WINDOW"
                                "PACKAGE" "LIBRARY" "PROGRAM" "PACKAGE BODY" "JAVA RESOURCE" "XML SCHEMA"
                                "JOB CLASS" "TRIGGER" "TABLE" "SYNONYM" "VIEW" "FUNCTION" "WINDOW GROUP"
                                "JAVA CLASS" "INDEXTYPE" "INDEX" "TYPE" "EVALUATION CONTEXT" ))

(defvar sqlplus-end-of-source-sentinel "%%@@end-of-source-sentinel@@%%")

(defconst sqlplus-system-variables
  '("appi[nfo]" "array[size]" "auto[commit]" "autop[rint]" "autorecovery" "autot[race]" "blo[ckterminator]" "cmds[ep]"
    "colsep" "com[patibility]" "con[cat]" "copyc[ommit]" "copytypecheck" "def[ine]" "describe" "echo" "editf[ile]"
    "emb[edded]" "esc[ape]" "feed[back]" "flagger" "flu[sh]" "hea[ding]" "heads[ep]" "instance" "lin[esize]"
    "lobof[fset]" "logsource" "long" "longc[hunksize]" "mark[up]" "newp[age]" "null" "numf[ormat]" "num[width]"
    "pages[ize]" "pau[se]" "recsep" "recsepchar" "serverout[put]" "shift[inout]" "show[mode]" "sqlbl[anklines]"
    "sqlc[ase]" "sqlco[ntinue]" "sqln[umber]" "sqlpluscompat[ibility]" "sqlpre[fix]" "sqlp[rompt]" "sqlt[erminator]"
    "suf[fix]" "tab" "term[out]" "ti[me]" "timi[ng]" "trim[out]" "trims[pool]" "und[erline]" "ver[ify]" "wra[p]"))

(defconst sqlplus-commands
  '(("@[@]")
    (("/" "r[un]"))
    ("acc[ept]"
     (font-lock-type-face "num[ber]" "char" "date" "binary_float" "binary_double")
     (font-lock-keyword-face "for[mat]" "def[ault]" "[no]prompt" "hide"))
    ("a[ppend]")
    ("archive log"
     (font-lock-keyword-face "list" "stop" "start" "next" "all" "to"))
    ("attribute"
     (font-lock-keyword-face "ali[as]" "cle[ar]" "for[mat]" "like" "on" "off"))
    ("bre[ak]"
     (font-lock-keyword-face "on" "row" "report" "ski[p]" "page" "nodup[licates]" "dup[licates]"))
    ("bti[tle]"
     (font-lock-keyword-face "on" "off")
     (font-lock-builtin-face "bold" "ce[nter]" "col" "format" "le[ft]" "r[ight]" "s[kip]" "tab"))
    ("c[hange]")
    ("cl[ear]"
     (font-lock-keyword-face "bre[aks]" "buff[er]" "col[umns]" "comp[utes]" "scr[een]" "sql" "timi[ng]"))
    ("col[umn]"
     (font-lock-keyword-face "ali[as]" "cle[ar]" "entmap" "on" "off" "fold_a[fter]" "fold_b[efore]" "for[mat]" "hea[ding]"
                             "jus[tify]" "l[eft]" "c[enter]" "r[ight]" "like" "newl[ine]" "new_v[alue]" "nopri[nt]" "pri[nt]"
                             "nul[l]" "old_v[alue]" "wra[pped]" "wor[d_wrapped]" "tru[ncated]"))
    ("comp[ute]"
     (font-lock-keyword-face "lab[el]" "of" "on" "report" "row")
     (font-lock-builtin-face "avg" "cou[nt]" "min[imum]" "max[imum]" "num[ber]" "sum" "std" "var[iance]"))
    ("conn[ect]"
     (font-lock-keyword-face "as" "sysoper" "sysdba"))
    ("copy")
    ("def[ine]")
    ("del"
     (font-lock-keyword-face "last"))
    ("desc[ribe]")
    ("disc[onnect]")
    ("ed[it]")
    ("exec[ute]")
    (("exit" "quit")
     (font-lock-keyword-face "success" "failure" "warning" "commit" "rollback"))
    ("get"
     (font-lock-keyword-face "file" "lis[t]" "nol[ist]"))
    ("help")
    (("ho[st]" "!" "$"))
    ("i[nput]")
    ("l[ist]"
     (font-lock-keyword-face "last"))
    ("passw[ord]")
    ("pau[se]")
    ("pri[nt]")
    ("pro[mpt]")
    ("recover"
     (font-lock-keyword-face "begin" "end" "backup" "automatic" "from" "logfile" "test" "allow" "corruption" "continue" "default" "cancel"
                             "standby" "database" "until" "time" "change" "using" "controlfile" "tablespace" "datafile"
                             "consistent" "with" "[no]parallel" "managed" "disconnect" "session" "[no]timeout" "[no]delay" "next" "no" "expire"
                             "current" "through" "thread" "sequence" "all" "archivelog" "last" "switchover" "immediate" "[no]wait"
                             "finish" "skip"))
    ("rem[ark]")
    ("repf[ooter]"
     (font-lock-keyword-face "page" "on" "off")
     (font-lock-builtin-face "bold" "ce[nter]" "col" "format" "le[ft]" "r[ight]" "s[kip]" "tab"))
    ("reph[eader]"
     (font-lock-keyword-face "page" "on" "off")
     (font-lock-builtin-face "bold" "ce[nter]" "col" "format" "le[ft]" "r[ight]" "s[kip]" "tab"))
    ("sav[e]"
     (font-lock-keyword-face "file" "cre[ate]" "rep[lace]" "app[end]"))
    ("set"
     (font-lock-builtin-face sqlplus-system-variables)
     (font-lock-keyword-face "on" "off" "immediate" "trace[only]" "explain" "statistics" "native" "v7" "v8" "all" "linenum" "indent"
                             "entry" "intermediate" "full" "local" "head" "html" "body" "table" "entmap" "spool" "[pre]format"
                             "none" "[word_]wrapped" "each" "truncated" "[in]visible" "mixed" "lower" "upper"))
    ("sho[w]"
     (font-lock-keyword-face "all" "bti[tle]" "err[ors]" "function" "procedure" "package[ body]" "trigger" "view" "type[ body]"
                             "dimension" "java class" "lno" "parameters" "pno" "recyc[lebin]" "rel[ease]" "repf[ooter]" "reph[eader]"
                             "sga" "spoo[l]" "sqlcode" "tti[tle]" "user")
     (font-lock-builtin-face sqlplus-system-variables))
    ("shutdown"
     (font-lock-keyword-face "abort" "immediate" "normal" "transactional" "local"))
    ("spo[ol]"
     ("cre" "create" "rep" "replace" "app" "append" "off" "out"))
    ("sta[rt]")
    ("startup"
     (font-lock-keyword-face "force" "restrict" "pfile" "quiet" "mount" "open" "nomount" "read" "only" "write" "recover"))
    ("store"
     (font-lock-keyword-face "set" "cre[ate]" "rep[lace]" "app[end]"))
    ("timi[ng]"
     (font-lock-keyword-face "start" "show" "stop"))
    ("tti[tle]"
     (font-lock-keyword-face "tti[tle]" "on" "off")
     (font-lock-builtin-face "bold" "ce[nter]" "col" "format" "le[ft]" "r[ight]" "s[kip]" "tab"))
    ("undef[ine]")
    ("var[iable]"
     (font-lock-type-face "number" "[n]char" "byte" "[n]varchar2" "[n]clob" "refcursor" "binary_float" "binary_double"))
    ("whenever oserror"
     (font-lock-keyword-face "exit" "success" "failure" "commit" "rollback" "continue" "commit" "rollback" "none"))
    ("whenever sqlerror"
     (font-lock-keyword-face "exit" "success" "failure" "warning" "commit" "rollback" "continue" "none"))))

(defvar plsql-mode-map nil)

(defstruct sqlplus-global-struct
  font-lock-regexps
  objects-alist
  side-view-buffer
  root-dir
)

(defvar sqlplus-global-structures (make-hash-table :test 'equal)
  "Connect string -> sqlplus-global-struct")

(defun sqlplus-get-objects-alist (&optional connect-string)
  (let ((struct (gethash (car (refine-connect-string (or connect-string sqlplus-connect-string sqlplus-process-p)))
			 sqlplus-global-structures)))
    (when struct
      (sqlplus-global-struct-objects-alist struct))))

(defun sqlplus-set-objects-alist (objects-alist &optional connect-string)
  (let ((struct (gethash (car (refine-connect-string (or connect-string sqlplus-connect-string sqlplus-process-p)))
			 sqlplus-global-structures)))
    (when struct
      (setf (sqlplus-global-struct-objects-alist struct) objects-alist))))

(defun sqlplus-get-font-lock-regexps (&optional connect-string)
  (let ((struct (gethash (car (refine-connect-string (or connect-string sqlplus-connect-string sqlplus-process-p)))
			 sqlplus-global-structures)))
    (when struct
      (sqlplus-global-struct-font-lock-regexps struct))))

(defun sqlplus-set-font-lock-regexps (font-lock-regexps &optional connect-string)
  (let ((struct (gethash (car (refine-connect-string (or connect-string sqlplus-connect-string sqlplus-process-p)))
			 sqlplus-global-structures)))
    (when struct
      (setf (sqlplus-global-struct-font-lock-regexps struct) font-lock-regexps))))

(defun sqlplus-get-side-view-buffer (&optional connect-string)
  (let ((struct (gethash (car (refine-connect-string (or connect-string sqlplus-connect-string sqlplus-process-p)))
			 sqlplus-global-structures)))
    (when struct
      (sqlplus-global-struct-side-view-buffer struct))))
  
(defun sqlplus-get-root-dir (&optional connect-string)
  (let ((struct (gethash (car (refine-connect-string (or connect-string sqlplus-connect-string sqlplus-process-p)))
			 sqlplus-global-structures)))
    (when struct
      (sqlplus-global-struct-root-dir struct))))

(defun sqlplus-set-root-dir (root-dir &optional connect-string)
  (let ((struct (gethash (car (refine-connect-string (or connect-string sqlplus-connect-string sqlplus-process-p)))
			 sqlplus-global-structures)))
    (when struct
      (setf (sqlplus-global-struct-root-dir struct) root-dir))))

;;; ---

(defun sqlplus-initial-strings ()
  (append sqlplus-initial-strings
          (list 
           (concat "btitle left '" sqlplus-page-separator "'")
           (concat "repfooter left '" sqlplus-repfooter "'")
           (concat "set pagesize " (number-to-string sqlplus-pagesize)))))

(defun sqlplus-connect-string-lessp (cs1 cs2)
  "Compare two connect strings"
  (let ((cs1-pair (split-string cs1 "@"))
        (cs2-pair (split-string cs2 "@")))
    (or (string< (cadr cs1-pair) (cadr cs2-pair))
        (and (string= (cadr cs1-pair) (cadr cs2-pair))
             (string< (car cs1-pair) (car cs2-pair))))))

(defun sqlplus-divide-connect-strings ()
  "Returns (active-connect-string-list . inactive-connect-string-list)"
  (let* ((active-connect-strings
          (sort (delq nil (mapcar (lambda (buffer)
                                    (with-current-buffer buffer
                                      (when (and (eq major-mode 'sqlplus-mode)
                                                 sqlplus-connect-string)
                                        (let ((cs (car (refine-connect-string sqlplus-connect-string))))
                                          (when (and (get-buffer (sqlplus-get-process-buffer-name cs))
                                                     (get-process (sqlplus-get-process-name cs)))
                                            (downcase cs))))))
                                  (buffer-list)))
                'sqlplus-connect-string-lessp))
         (inactive-connect-strings
          (sort (delq nil (mapcar (lambda (pair)
                                    (unless (member (downcase (car pair)) active-connect-strings) (downcase (car pair))) )
                                  sqlplus-connect-strings-alist))
                'sqlplus-connect-string-lessp)))
    (setq active-connect-strings (remove-duplicates active-connect-strings :test 'equal))
    (setq inactive-connect-strings (remove-duplicates inactive-connect-strings :test 'equal))
    (cons active-connect-strings inactive-connect-strings)))

(defun sqlplus-connections-menu (menu)
  (condition-case err
      (let* ((connect-strings-pair (sqlplus-divide-connect-strings))
             (active-connect-strings (car connect-strings-pair))
             (inactive-connect-strings (cdr connect-strings-pair)))
        (append 
	 (list ["New connection..." sqlplus t])
	 (list ["Tnsnames.ora" sqlplus-find-tnsnames t])
	 (list ["Command Line" sqlplus-command-line t])
	 (when (eq major-mode 'sqlplus-mode)
	   (list
	    "----"
	    ["Evaluate Statement" sqlplus-send-current sqlplus-connect-string]
	    ["Explain Statement" sqlplus-explain sqlplus-connect-string]
	    ["Evaluate Statement (HTML)" sqlplus-send-current-html sqlplus-connect-string]
	    ["Evaluate Region" sqlplus-send-region (and (mark) sqlplus-connect-string)]))
	 (when orcl-mode
	   (list
	    "----"
	    ["Send Commit"              sqlplus-send-commit sqlplus-connect-string]
	    ["Send Rollback"            sqlplus-send-rollback sqlplus-connect-string]
	    ["Restart Connection"       sqlplus-restart-connection sqlplus-connect-string]
	    ["Show History"             sqlplus-show-history sqlplus-connect-string]
	    ["Get Source from DB"       sqlplus-get-source sqlplus-connect-string]
	    ["Interrupt Evaluation"     sqlplus-send-interrupt sqlplus-connect-string]
	    ["Compare schema to filesystem" sqlplus-compare-schema-to-filesystem sqlplus-connect-string]
	    "----"
	    (list "Output"
		  ["Show window"             sqlplus-buffer-display-window t]
		  "----"
		  ["Redisplay"               sqlplus-buffer-redisplay-current t]
		  ["Previous"                sqlplus-buffer-prev-command t]
		  ["Next"                    sqlplus-buffer-next-command t]
		  "----"
		  ["Scroll Right"            sqlplus-buffer-scroll-right t]
		  ["Scroll Left"             sqlplus-buffer-scroll-left t]
		  ["Scroll Down"             sqlplus-buffer-scroll-down t]
		  ["Scroll Up"               sqlplus-buffer-scroll-up t]
		  "----"
		  ["Bottom"                  sqlplus-buffer-bottom t]
		  ["Top"                     sqlplus-buffer-top    t]
		  "----"
		  ["Erase"                   sqlplus-buffer-erase  t])
	    ))
	 (when inactive-connect-strings
	   (append
	    (list "----")
	    (list (append (list "Recent Connections")
			  (mapcar (lambda (connect-string)
				    (vector connect-string (list 'apply ''sqlplus
								 (list 'sqlplus-read-connect-string connect-string)) t)) inactive-connect-strings)))))
	 (when active-connect-strings
	   (append
	    (list "----")
	    (mapcar (lambda (connect-string)
		      (vector connect-string (list 'apply ''sqlplus
						   (list 'sqlplus-read-connect-string connect-string)) t)) active-connect-strings)))
	 ))
    (error (message (error-message-string err)))))

(defun sqlplus-send-commit ()
  "Send 'commit' command to SQL*Process."
  (interactive)
  (sqlplus-check-connection)
  (sqlplus-execute sqlplus-connect-string "commit;" nil nil))

(defun sqlplus-send-rollback ()
  "Send 'rollback' command to SQL*Process."
  (interactive)
  (sqlplus-check-connection)
  (sqlplus-execute sqlplus-connect-string "rollback;" nil nil))

(defun sqlplus-show-history ()
  "Show command history for current connection."
  (interactive)
  (sqlplus-check-connection)
  (sqlplus-verify-buffer sqlplus-connect-string)
  (switch-to-buffer (sqlplus-get-history-buffer sqlplus-connect-string)))

(defun sqlplus-restart-connection ()
  "Kill SQL*Plus process and start again."
  (interactive)
  (sqlplus-check-connection)
  (sqlplus-verify-buffer sqlplus-connect-string)
  (let ((connect-stringos sqlplus-connect-string))
    (unwind-protect
        (progn
          (setq sqlplus-kill-function-inhibitor t)
          (sqlplus-shutdown connect-stringos t))
      (setq sqlplus-kill-function-inhibitor nil))
    (sqlplus connect-stringos (sqlplus-get-input-buffer-name connect-stringos))))

(define-skeleton plsql-begin
  "begin..end skeleton"
  "" ; interactor
  "begin" ?\n
  > _ ?\n
  "end;" >)

(define-skeleton plsql-loop
  "loop..end loop skeleton"
  "" ; interactor
  "loop" ?\n
  > _ ?\n
  "end loop;" >)

(define-skeleton plsql-if
  "if..end if skeleton"
  "" ; interactor
  "if " _ " then" ?\n
  > ?\n
  "end if;" >)

;;;  SQLPLUS-mode Keymap -

(unless orcl-mode-map
  (setq orcl-mode-map (make-sparse-keymap))
  (define-key orcl-mode-map "\C-c\C-o" 'sqlplus-buffer-display-window)
  (define-key orcl-mode-map "\C-c\C-l" 'sqlplus-buffer-redisplay-current)
  (define-key orcl-mode-map "\C-c\C-p" 'sqlplus-buffer-prev-command)
  (define-key orcl-mode-map [C-S-up] 'sqlplus-buffer-prev-command)
  (define-key orcl-mode-map "\C-c\C-n" 'sqlplus-buffer-next-command)
  (define-key orcl-mode-map [C-S-down] 'sqlplus-buffer-next-command)
  (define-key orcl-mode-map "\C-c\C-b" 'sqlplus-buffer-scroll-right)
  (define-key orcl-mode-map [C-S-left] 'sqlplus-buffer-scroll-right)
  (define-key orcl-mode-map "\C-c\C-f" 'sqlplus-buffer-scroll-left)
  (define-key orcl-mode-map [C-S-right] 'sqlplus-buffer-scroll-left)
  (define-key orcl-mode-map "\C-c\M-v" 'sqlplus-buffer-scroll-down)
  (define-key orcl-mode-map "\C-c\C-v" 'sqlplus-buffer-scroll-up)
  (define-key orcl-mode-map "\C-c>"    'sqlplus-buffer-bottom)
  (define-key orcl-mode-map "\C-c<"    'sqlplus-buffer-top)
  (define-key orcl-mode-map "\C-c\C-w" 'sqlplus-buffer-erase)
  (define-key orcl-mode-map "\C-c\C-m" 'sqlplus-send-commit)
  (define-key orcl-mode-map "\C-c\C-a" 'sqlplus-send-rollback)
  (define-key orcl-mode-map "\C-c\C-k" 'sqlplus-restart-connection)
  (define-key orcl-mode-map "\C-c\C-t" 'sqlplus-show-history)
  (define-key orcl-mode-map "\C-c\C-s" 'sqlplus-get-source)
  (define-key orcl-mode-map "\C-c\C-i" 'sqlplus-send-interrupt)
  (define-key orcl-mode-map [S-return] 'sqlplus-send-user-string)
  (define-key orcl-mode-map [tool-bar sqlplus-restart-connection]
    (list 'menu-item "Restart connection" 'sqlplus-restart-connection :image sqlplus-kill-image))
  (define-key orcl-mode-map [tool-bar sqlplus-cancel]
    (list 'menu-item "Cancel" 'sqlplus-send-interrupt :image sqlplus-cancel-image))
  (define-key orcl-mode-map [tool-bar sqlplus-rollback]
    (list 'menu-item "Rollback" 'sqlplus-send-rollback :image sqlplus-rollback-image))
  (define-key orcl-mode-map [tool-bar sqlplus-commit]
    (list 'menu-item "Commit" 'sqlplus-send-commit :image sqlplus-commit-image)))

(unless sqlplus-mode-map
  (setq sqlplus-mode-map (make-sparse-keymap))
  (define-key sqlplus-mode-map "\C-c\C-g" 'plsql-begin)
  (define-key sqlplus-mode-map "\C-c\C-q" 'plsql-loop)
  (define-key sqlplus-mode-map "\C-c\C-z" 'plsql-if)
  (define-key sqlplus-mode-map "\C-c\C-r" 'sqlplus-send-region)
  (define-key sqlplus-mode-map [C-return] 'sqlplus-send-current)
  (define-key sqlplus-mode-map [M-return] 'sqlplus-explain)
  (define-key sqlplus-mode-map "\C-c\C-e" 'sqlplus-send-current)
  (define-key sqlplus-mode-map "\C-c\C-j" 'sqlplus-send-current-html)
  (define-key sqlplus-mode-map [C-S-return] 'sqlplus-send-current-html)
  (define-key sqlplus-mode-map "\M-." 'sqlplus-file-get-source)
  (define-key sqlplus-mode-map [C-down-mouse-1] 'sqlplus-mouse-select-identifier)
  (define-key sqlplus-mode-map [C-mouse-1] 'sqlplus-file-get-source-mouse)
  )

(easy-menu-add-item nil nil sqlplus-connections-menu t)

(unless sqlplus-mode-syntax-table
  (setq sqlplus-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?/ ". 14" sqlplus-mode-syntax-table) ; comment start
  (modify-syntax-entry ?* ". 23" sqlplus-mode-syntax-table)
  (modify-syntax-entry ?+ "."    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?. "."    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?\" "."   sqlplus-mode-syntax-table)
  (modify-syntax-entry ?\\ "."   sqlplus-mode-syntax-table)
  (modify-syntax-entry ?- ". 12b"    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?\n "> b"    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?= "."    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?% "w"    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?< "."    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?> "."    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?& "w"    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?| "."    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?_ "w"    sqlplus-mode-syntax-table) ; _ is word char
  (modify-syntax-entry ?\' "\"" sqlplus-mode-syntax-table))

;;;  SQL*Plus mode

(defun connect-string-to-string ()
  (let ((txt (or (car (refine-connect-string sqlplus-connect-string)) "disconnected"))
	(result))
    (if (string-match "^\\(.*?\\)\\(\\w*prod\\w*\\)$" txt)
	(if (>= emacs-major-version 22)
	    (setq result (list (list :propertize (substring txt 0 (match-beginning 2)) 'face '((:foreground "blue")))
			       (list :propertize (substring txt (match-beginning 2)) 'face '((:foreground "red")(:weight bold)))))
	  (setq result (setq txt (propertize txt 'face '((:foreground "blue")))))
	  (put-text-property (match-beginning 2) (match-end 2) 'face '((:foreground "red")(:weight bold)) txt))
      (setq result 
	    (if (>= emacs-major-version 22)
		(list :propertize txt 'face '((:foreground "blue")))
	      (setq txt (propertize txt 'face '((:foreground "blue")))))))
    result))

(defun sqlplus-font-lock (type-symbol limit)
  (let ((sqlplus-font-lock-regexps (sqlplus-get-font-lock-regexps)))
    (when sqlplus-font-lock-regexps
      (let ((regexp (gethash type-symbol sqlplus-font-lock-regexps)))
	(when regexp
	  (re-search-forward regexp limit t))))))

;; Local in input buffer (sqlplus-mode)
(defvar sqlplus-command-overlay nil)
(make-variable-buffer-local 'sqlplus-command-overlay)
(defvar sqlplus-begin-command-overlay-arrow-position nil)
(make-variable-buffer-local 'sqlplus-begin-command-overlay-arrow-position)
(defvar sqlplus-end-command-overlay-arrow-position nil)
(make-variable-buffer-local 'sqlplus-end-command-overlay-arrow-position)

(defun sqlplus-highlight-current-sqlplus-command()
  (when (and window-system sqlplus-command-highlighting-style)
    (let* ((pair (sqlplus-mark-current))
	   (begin (and (car pair) (save-excursion (goto-char (car pair)) (skip-chars-forward " \t\n") (point))))
	   (end (and (cdr pair) (save-excursion (goto-char (cdr pair)) (skip-chars-backward " \t\n") (beginning-of-line) (point))))
	   (point-line-beg (save-excursion (beginning-of-line) (point)))
	   (overlay-begin begin)
	   (overlay-end end))
      (when (and begin end)
	(when (< end point-line-beg)
	  (save-excursion (goto-char point-line-beg) (when (eobp) (insert "\n")))
	  (setq end point-line-beg)
	  (setq overlay-end end))
	(when (or (>= begin end) (< (point) begin))
	  (when (or (< (point) begin) (> begin end))
	    (setq overlay-begin nil
		  overlay-end nil))
	  (setq begin nil
		end nil)))
      (if (and overlay-begin overlay-end (memq sqlplus-command-highlighting-style '(background fringe-and-background)))
	  (progn
	    (setq overlay-end (save-excursion
				(goto-char overlay-end)
				(beginning-of-line 2)
				(point)))
	    (move-overlay sqlplus-command-overlay overlay-begin overlay-end))
	(move-overlay sqlplus-command-overlay 1 1))
      (if (memq sqlplus-command-highlighting-style '(fringe fringe-and-background))
	  (progn
	    (put 'sqlplus-begin-command-overlay-arrow-position 'overlay-arrow-bitmap 'top-left-angle)
	    (put 'sqlplus-end-command-overlay-arrow-position 'overlay-arrow-bitmap 'bottom-left-angle)
	    (set-marker sqlplus-begin-command-overlay-arrow-position begin)
	    (set-marker sqlplus-end-command-overlay-arrow-position end))
	(set-marker sqlplus-begin-command-overlay-arrow-position nil)
	(set-marker sqlplus-end-command-overlay-arrow-position nil)))))

(defun sqlplus-find-begin-of-sqlplus-command ()
  (save-excursion
    (beginning-of-line)
    (while (and (not (bobp)) (save-excursion (end-of-line 0) (skip-chars-backward " \t") (equal (char-before) ?-)))
      (beginning-of-line 0))
    (point)))

(defun sqlplus-find-end-of-sqlplus-command ()
  (save-excursion
    (end-of-line)
    (while (progn (skip-chars-backward " \t") (and (not (eobp)) (equal (char-before) ?-)))
      (end-of-line 2))
    (point)))

(defun sqlplus-set-font-lock-emacs-structures-for-level (level mode-symbol)
  (let ((result (append sql-mode-oracle-font-lock-keywords
                        (default-value (cond ((equal level 3) 'sqlplus-font-lock-keywords-3)
                                             ((equal level 2) 'sqlplus-font-lock-keywords-2)
                                             ((equal level 1) 'sqlplus-font-lock-keywords-1)
                                             (t nil))))))
    (when (featurep 'plsql)
      (setq result (append (symbol-value 'plsql-oracle-font-lock-fix-re) result)))
    (setq result
          (append
           ;; Names for schemas, tables, synonyms, views, columns, sequences, packages, triggers and indexes
           (when (> level 2)
             (mapcar (lambda (pair)
                       (let ((type-symbol (car pair))
                             (face (cadr pair)))
                         (cons (eval `(lambda (limit) (sqlplus-font-lock ',type-symbol limit))) face)))
                     sqlplus-syntax-faces))
           ;; SQL*Plus
           (when (eq mode-symbol 'sqlplus-mode)
             (unless sqlplus-commands-regexp-1
               (flet ((first-form-fun (cmds) (mapcar (lambda (name) (car (sqlplus-full-forms name))) cmds))
                      (all-forms-fun (cmds) (mapcan 'sqlplus-full-forms cmds))
                      (sqlplus-commands-regexp-fun (form-fun cmds) (concat "^" (regexp-opt (funcall form-fun cmds) t) "\\b"))
                      (sqlplus-system-variables-fun (form-fun vars) (concat "\\b" (regexp-opt (funcall form-fun vars) t) "\\b")))
                 (flet ((sqlplus-v22-commands-font-lock-keywords-fun
                         (form-fun)
                         (delq nil
                               (mapcar
                                (lambda (command-info)
                                  (let* ((names (car command-info))
                                         (names-list (if (listp names) names (list names)))
                                         (sublists (cdr command-info)))
                                    (when sublists
                                      (append (list (sqlplus-commands-regexp-fun form-fun names-list))
                                              (mapcar (lambda (sublist)
                                                        (let ((face (car sublist))
                                                              (regexp (concat "\\b"
                                                                              (regexp-opt (mapcan (lambda (name) (sqlplus-full-forms name))
                                                                                                  (mapcan (lambda (elem)
                                                                                                            (if (symbolp elem)
                                                                                                                (copy-list (symbol-value elem))
                                                                                                              (list elem)))
                                                                                                          (cdr sublist)))
                                                                                          t)
                                                                              "\\b")))
                                                          (list regexp '(sqlplus-find-end-of-sqlplus-command) nil (list 1 face))))
                                                      sublists)
                                              (list '("\\(\\w+\\)" (sqlplus-find-end-of-sqlplus-command) nil (1 font-lock-sqlplus-face)))))))
                                sqlplus-commands))))
                   (let ((commands (mapcan
                                    (lambda (command-info) (let ((names (car command-info))) (if (listp names) (copy-list names) (list names))))
                                    sqlplus-commands)))
                     (setq sqlplus-commands-regexp-1 (sqlplus-commands-regexp-fun 'first-form-fun commands))
                     (setq sqlplus-commands-regexp-23 (sqlplus-commands-regexp-fun 'all-forms-fun commands))
                     (if (<= emacs-major-version 21)
                         (setq sqlplus-system-variables-regexp-1 (sqlplus-system-variables-fun 'first-form-fun sqlplus-system-variables)
                               sqlplus-system-variables-regexp-23 (sqlplus-system-variables-fun 'all-forms-fun sqlplus-system-variables))
                       (setq sqlplus-v22-commands-font-lock-keywords-1 (sqlplus-v22-commands-font-lock-keywords-fun 'first-form-fun)
                             sqlplus-v22-commands-font-lock-keywords-23 (sqlplus-v22-commands-font-lock-keywords-fun 'all-forms-fun)))))))
             (append (list
                      ;; Comments (REM command)
                      (cons "^\\(rem\\)\\b\\(.*?\\)$" '((1 font-lock-keyword-face nil nil) (2 font-lock-comment-face t nil)))
                      ;; Predefined SQL*Plus variables
                      (cons (concat "\\b"
                                    (regexp-opt '("_CONNECT_IDENTIFIER" "_DATE" "_EDITOR" "_O_VERSION" "_O_RELEASE" "_PRIVILEGE"
                                                  "_SQLPLUS_RELEASE" "_USER") t)
                                    "\\b")
                            'font-lock-builtin-face)
                      ;; SQL*Plus commands (+ shortcuts if level >= 2)
                      (cons
                       (concat (if (>= level 2) sqlplus-commands-regexp-23 sqlplus-commands-regexp-1) "\\|^\\(@@\\|@\\|!\\|/\\|\\$\\)" )
                       'font-lock-keyword-face))
                     (if (<= emacs-major-version 21)
                         ;; SQL*Plus system variables (+ shortcuts if level >= 2)
                         (list (cons (if (>= level 2) sqlplus-system-variables-regexp-23 sqlplus-system-variables-regexp-1) 'font-lock-builtin-face))
                       ;; ver. >= 22
                       (if (>= level 2) sqlplus-v22-commands-font-lock-keywords-23 sqlplus-v22-commands-font-lock-keywords-1))))
            ; (cons "\\b\\([a-zA-Z$_#0-9]+\\)\\b\\.\\(\\b[a-zA-Z$_#0-9]+\\b\\)" '((1 font-lock-type-face nil nil)(2 font-lock-variable-name-face nil nil)))
           (list
            ;; Extra Oracle syntax highlighting, not recognized by sql-mode or plsql-mode
            (cons sqlplus-oracle-extra-types-re 'font-lock-type-face)
            (cons sqlplus-oracle-extra-warning-words-re 'font-lock-warning-face)
            (cons sqlplus-oracle-extra-types-re 'font-lock-type-face)
            (cons sqlplus-oracle-extra-keywords-re 'font-lock-keyword-face)
            (cons sqlplus-oracle-plsql-extra-reserved-words-re 'font-lock-keyword-face)
            (if (string-match "XEmacs\\|Lucid" emacs-version)
                (cons sqlplus-oracle-extra-pseudocolumns-re 'font-lock-preprocessor-face)
              (cons sqlplus-oracle-extra-pseudocolumns-re 'font-lock-builtin-face))
            (if (string-match "XEmacs\\|Lucid" emacs-version)
                (cons sqlplus-oracle-extra-builtin-functions-re 'font-lock-preprocessor-face)
              (cons sqlplus-oracle-extra-builtin-functions-re 'font-lock-builtin-face))
            ;; SQL*Plus variable names, like '&name' or '&&name'
            (cons "\\(\\b&[&a-zA-Z$_#0-9]+\\b\\)" 'font-lock-variable-name-face))
           result
           ;; Function calls
           (when (>= level 2)
             (list (cons "\\b\\(\\([a-zA-Z$_#0-9]+\\b\\)\\.\\)?\\(\\b[a-zA-Z$_#0-9]+\\b\\)\\s-*("
                         '((2 font-lock-type-face nil t)
                           (3 font-lock-function-name-face nil nil)))))))
    result))

(defun sqlplus-mode nil
  "Mode for editing and executing SQL*Plus commands.  Entry into this mode runs the hook
'sqlplus-mode-hook'.

Use \\[sqlplus] to start the SQL*Plus interpreter.

Just position the cursor on or near the SQL*Plus statement you
wish to send and press '\\[sqlplus-send-current]' to run it and
display the results.

Mode Specific Bindings:

\\{sqlplus-mode-map}"
  (interactive)
  (run-hooks 'change-major-mode-hook)
  (setq major-mode 'sqlplus-mode
	mode-name "SQL*Plus")
  (use-local-map sqlplus-mode-map)
  (set-syntax-table sqlplus-mode-syntax-table)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (setq comment-start "/* "
        comment-end " */")
  (orcl-mode 1)
  (setq sqlplus-font-lock-keywords-1 (sqlplus-set-font-lock-emacs-structures-for-level 1 major-mode)
        sqlplus-font-lock-keywords-2 (sqlplus-set-font-lock-emacs-structures-for-level 2 major-mode)
        sqlplus-font-lock-keywords-3 (sqlplus-set-font-lock-emacs-structures-for-level 3 major-mode))
  (when (featurep 'plsql)
    (set (make-local-variable 'indent-line-function)
	 (lambda () (interactive) (condition-case err (funcall (symbol-function 'plsql-indent)) (error (message "Error: %S" err)))))
    (set (make-local-variable 'indent-region-function) 'plsql-indent-region)
    (set (make-local-variable 'align-mode-rules-list) 'plsql-align-rules-list))
  (setq font-lock-defaults sqlplus-font-lock-defaults)
  (unless sqlplus-connect-string
    (let ((potential-connect-string (sqlplus-get-potential-connect-string (buffer-file-name))))
      (when (and potential-connect-string
                 (get-process (sqlplus-get-process-name potential-connect-string)))
        (setq sqlplus-connect-string potential-connect-string))))
  (set (make-local-variable 'font-lock-extend-after-change-region-function)
       (lambda (beg end old-len)
         (cons (save-excursion (goto-char beg) (sqlplus-find-begin-of-sqlplus-command))
               (save-excursion (goto-char end) (sqlplus-find-end-of-sqlplus-command)))))
  (unless font-lock-sqlplus-face
    (copy-face 'default 'font-lock-sqlplus-face)
    (setq font-lock-sqlplus-face 'font-lock-sqlplus-face))
  (turn-on-font-lock)
  (unless frame-background-mode
    (setq frame-background-mode (if (< (sqlplus-color-percentage (face-background 'default)) 50) 'dark 'light)))
  (setq imenu-generic-expression '((nil "^--[ ]*\\([^;.\n]*\\)" 1)))
  ;; if input buffer has sqlplus-mode then prepare it for command under cursor selection
  (when (and (eq major-mode 'sqlplus-mode) (null sqlplus-begin-command-overlay-arrow-position))
    (setq sqlplus-begin-command-overlay-arrow-position (make-marker)
	  sqlplus-end-command-overlay-arrow-position (make-marker)
	  sqlplus-command-overlay (make-overlay 1 1))
    (overlay-put sqlplus-command-overlay 'face 'sqlplus-command-highlight-face)
    (when (and (>= emacs-major-version 22) (not (memq 'sqlplus-begin-command-overlay-arrow-position overlay-arrow-variable-list)))
      (push 'sqlplus-begin-command-overlay-arrow-position overlay-arrow-variable-list))
    (when (and (>= emacs-major-version 22) (not (memq 'sqlplus-end-command-overlay-arrow-position overlay-arrow-variable-list)))
      (push 'sqlplus-end-command-overlay-arrow-position overlay-arrow-variable-list))
    (add-hook 'pre-command-hook (lambda ()
				  (set-marker sqlplus-begin-command-overlay-arrow-position nil)
				  (set-marker sqlplus-end-command-overlay-arrow-position nil))
	      nil t)
    (add-hook 'post-command-hook (lambda ()
				   (sqlplus-clear-mouse-selection)
				   (set-marker sqlplus-begin-command-overlay-arrow-position nil)
				   (set-marker sqlplus-end-command-overlay-arrow-position nil))
	      nil t))
  (run-hooks 'sqlplus-mode-hook))

(defun sqlplus-color-percentage (color)
  (truncate (* (/ (/ (reduce '+ (color-values color)) 3.0) 65535.0) 100.0)))

(defun sqlplus-get-potential-connect-string (file-path)
  (when file-path
    (let* ((file-name (file-name-nondirectory file-path))
           (extension (file-name-extension file-name))
           (case-fold-search t))
      (when (and extension
		 (string-match (concat "^" sqlplus-session-file-extension "$") extension)
		 (string-match "@" file-name))
	(car (refine-connect-string (file-name-sans-extension file-name)))))))

(defun sqlplus-check-connection ()
  (if orcl-mode
      (unless sqlplus-connect-string
        (let* ((potential-connect-string (sqlplus-get-potential-connect-string (buffer-file-name)))
               (connect-string (car (sqlplus-read-connect-string nil (or potential-connect-string
									 (caar (sqlplus-divide-connect-strings)))))))
          (sqlplus connect-string (buffer-name))))
    (error "Current buffer is not determined to communicate with Oracle")))

;;;  Utilitities

(defun sqlplus-echo-in-buffer (buffer-name string &optional force-display hide-after-head)
  "Displays string in the named buffer, creating the buffer if needed.  If force-display is true, the buffer will appear
if not already shown."
  (let ((buffer (get-buffer buffer-name)))
    (when buffer
      (if force-display (display-buffer buffer))
      (with-current-buffer buffer
	(while (and (> (buffer-size) sqlplus-output-buffer-max-size)
		    (progn (goto-char (point-min))
			   (unless (eobp) (forward-char))
			   (re-search-forward  (concat "^" (regexp-quote sqlplus-output-separator)) nil t)))
	  (delete-region 1 (- (point) (length sqlplus-output-separator))))

	(goto-char (point-max))
	(let ((start-point (point)))
	  (insert string)
	  (when hide-after-head
	    (let ((from-pos (string-match "\n" string))
		  (keymap (make-sparse-keymap))
		  overlay)
	      (when from-pos
		(setq overlay (make-overlay (+ start-point from-pos) (- (+ start-point (length string)) 2)))
		(when (or (not (consp buffer-invisibility-spec))
			  (not (assq 'hide-symbol buffer-invisibility-spec)))
		  (add-to-invisibility-spec '(hide-symbol . t)))
		(overlay-put overlay 'invisible 'hide-symbol)
		(put-text-property start-point (- (+ start-point (length string)) 2) 'help-echo string)
		(put-text-property start-point (- (+ start-point (length string)) 2) 'mouse-face 'highlight)
		(put-text-property start-point (- (+ start-point (length string)) 2) 'keymap sqlplus-output-buffer-keymap)))))
	(if force-display
	    (set-window-point (get-buffer-window buffer-name) (point-max)))))))

(defun sqlplus-verify-buffer (connect-string)
  (let ((output-buffer-name (sqlplus-get-output-buffer-name connect-string))
	(process-buffer-name (sqlplus-get-process-buffer-name connect-string)))
    (when (not (get-buffer process-buffer-name))
      (sqlplus-shutdown connect-string)
      (error "No SQL*Plus session!  Use 'M-x sqlplus' to start the SQL*Plus interpreter"))
    (unless (get-buffer-process process-buffer-name)
      (sqlplus-shutdown connect-string)
      (error "Buffer '%s' is not talking to anybody!" output-buffer-name)))
  t)

(defun sqlplus-get-context (connect-string &optional id)
  (let ((process-buffer (sqlplus-get-process-buffer-name connect-string)))
    (when process-buffer
      (with-current-buffer process-buffer
        (when id
          (while (and sqlplus-command-contexts
                      (not (equal (sqlplus-get-context-value (car sqlplus-command-contexts) :id) id)))
            (setq sqlplus-command-contexts (cdr sqlplus-command-contexts))))
        (car sqlplus-command-contexts)))))

(defun sqlplus-get-context-value (context var-symbol)
  (cdr (assq var-symbol context)))

(defun sqlplus-set-context-value (context var-symbol value)
  (let ((association (assq var-symbol context)))
    (if association
        (setcdr association value)
      (setcdr context (cons (cons var-symbol value) (cdr context))))
    context))

(defun sqlplus-mark-current ()
  "Marks the current SQL for sending to the SQL*Plus process.  Marks are placed around a region defined by empty lines."
  (let (begin end empty-line-p empty-line-p next-line-included tail-p)
    (save-excursion
      (beginning-of-line)
      (setq empty-line-p (when (looking-at "^[ \t]*\\(\n\\|\\'\\)") (point)))
      (setq next-line-included (and empty-line-p (save-excursion (skip-chars-forward " \t\n") (> (current-column) 0))))
      (setq tail-p (and empty-line-p
			(or (bobp) (save-excursion (beginning-of-line 0) (looking-at "^[ \t]*\n"))))))
    (unless tail-p
      (save-excursion
	(end-of-line)
	(re-search-backward "\\`\\|\n[\r\t ]*\n[^ \t]" nil t)
	(skip-syntax-forward "-")
	(setq begin (point)))
      (save-excursion
	(beginning-of-line)
	(re-search-forward "\n[\r\t ]*\n[^ \t]\\|\\'" nil t)
	(unless (zerop (length (match-string 0)))
	  (backward-char 1))
	(skip-syntax-backward "-")
	(setq end (or (and (not next-line-included) empty-line-p) (point)))))
    (cons begin end)))

;;;  Transmission Commands

(defun sqlplus-send-current (arg &optional html)
  "Send the current SQL command(s) to the SQL*Plus process.  With argument, show results in raw form."
  (interactive "P")
  (sqlplus-check-connection)
  (when (buffer-file-name)
    (condition-case err
	(save-buffer)
      (error (message (error-message-string err)))))
  (let ((region (sqlplus-mark-current)))
    (setq sqlplus-region-beginning-pos (car region)
          sqlplus-region-end-pos (cdr region)))
  (if (and sqlplus-region-beginning-pos sqlplus-region-end-pos)
      (sqlplus-send-region arg sqlplus-region-beginning-pos sqlplus-region-end-pos nil html)
    (error "Point doesn't indicate any command to execute")))

(defun sqlplus-send-current-html (arg)
  (interactive "P")
  (sqlplus-send-current arg t))


;;;  SQLPLUS-Output Buffer Operations -

(defun sqlplus--show-buffer (connect-string fcn args)
  (let* ((output-buffer-name (sqlplus-get-output-buffer-name connect-string)))
    (sqlplus-verify-buffer connect-string)
    (if sqlplus-suppress-show-output-buffer
        (with-current-buffer (get-buffer output-buffer-name)
          (if fcn (condition-case err (apply fcn args) (error (message (error-message-string err))))))
      (if (not (eq (window-buffer (selected-window)) (get-buffer output-buffer-name)))
          (switch-to-buffer-other-window output-buffer-name))
      (if fcn (condition-case err (apply fcn args) (error (message (error-message-string err))))))))

(defun sqlplus-show-buffer (&optional connect-string fcn &rest args)
  "Makes the SQL*Plus output buffer visible in the other window."
  (interactive)
  (setq connect-string (or connect-string sqlplus-connect-string))
  (unless connect-string
    (error "Current buffer is disconnected!"))
  (let ((output-buffer-name (sqlplus-get-output-buffer-name connect-string)))
    (if (and output-buffer-name
             (eq (current-buffer) (get-buffer output-buffer-name)))
        (sqlplus--show-buffer connect-string fcn args)
      (save-excursion
        (save-selected-window
          (sqlplus--show-buffer connect-string fcn args))))))

(fset 'sqlplus-buffer-display-window 'sqlplus-show-buffer)

(defun sqlplus-buffer-scroll-up (&optional connect-string)
  "Scroll-up in the SQL*Plus output buffer window."
  (interactive)
  (sqlplus-show-buffer (or connect-string sqlplus-connect-string) 'scroll-up))

(defun sqlplus-buffer-scroll-down (&optional connect-string)
  "Scroll-down in the SQL*Plus output buffer window."
  (interactive)
  (sqlplus-show-buffer (or connect-string sqlplus-connect-string) 'scroll-down))

(defun sqlplus-scroll-left (num)
  (call-interactively 'scroll-left))

(defun sqlplus-scroll-right (num)
  (call-interactively 'scroll-right))

(defun sqlplus-buffer-scroll-left (num &optional connect-string)
  "Scroll-left in the SQL*Plus output buffer window."
  (interactive "p")
  (sqlplus-show-buffer (or connect-string sqlplus-connect-string) 'sqlplus-scroll-left (* num (/ (window-width) 2))))

(defun sqlplus-buffer-scroll-right (num &optional connect-string)
  "Scroll-right in the SQL*Plus output buffer window."
  (interactive "p")
  (sqlplus-show-buffer (or connect-string sqlplus-connect-string) 'sqlplus-scroll-right (* num (/ (window-width) 2))))

(defun sqlplus-buffer-mark-current (&optional connect-string)
  "Mark the current position in the SQL*Plus output window."
  (sqlplus-show-buffer (or connect-string sqlplus-connect-string) 'sqlplus-buffer-make-mark))

(defun sqlplus-buffer-make-mark (&optional connect-string)
  "Set the sqlplus-buffer-marker."
  (setq sqlplus-buffer-mark (copy-marker (point))))

(defun sqlplus-buffer-redisplay-current (&optional connect-string)
  "Go to the current sqlplus-buffer-mark."
  (interactive)
  (sqlplus-show-buffer (or connect-string sqlplus-connect-string) 'sqlplus-goto-mark))

(defun sqlplus-goto-mark ()
  (goto-char sqlplus-buffer-mark)
  (recenter 0))

(defun sqlplus-buffer-top (&optional connect-string)
  "Goto the top of the SQL*Plus output buffer."
  (interactive)
  (sqlplus-show-buffer (or connect-string sqlplus-connect-string) 'sqlplus-beginning-of-buffer))

(defun sqlplus-beginning-of-buffer nil (goto-char (point-min)))

(defun sqlplus-buffer-bottom (&optional connect-string)
  "Goto the bottom of the SQL*Plus output buffer."
  (interactive)
  (sqlplus-show-buffer (or connect-string sqlplus-connect-string) 'sqlplus-end-of-buffer))

(defun sqlplus-end-of-buffer nil (goto-char (point-max)) (unless sqlplus-suppress-show-output-buffer (recenter -1)))

(defun sqlplus-buffer-erase (&optional connect-string)
  "Clear the SQL output buffer."
  (interactive)
  (sqlplus-show-buffer (or connect-string sqlplus-connect-string) 'erase-buffer))

(defun sqlplus-buffer-next-command (&optional connect-string)
  "Search for the next command in the SQL*Plus output buffer."
  (interactive)
  (sqlplus-show-buffer (or connect-string sqlplus-connect-string) 'sqlplus-next-command))

(defun sqlplus-next-command nil
  "Search for the next command in the SQL*Plus output buffer."
  (cond ((re-search-forward  (concat "^" (regexp-quote sqlplus-output-separator)) nil t)
	 (forward-line 2)
	 (recenter 0))
	(t (beep) (message "No more commands."))))

(defun sqlplus-buffer-prev-command (&optional connect-string)
  "Search for the previous command in the SQL*Plus output buffer."
  (interactive)
  (sqlplus-show-buffer (or connect-string sqlplus-connect-string) 'sqlplus-previous-command))

(defun sqlplus-previous-command nil
  "Search for the previous command in the SQL*Plus output buffer."
  (let ((start (point)))
    (re-search-backward (concat "^" (regexp-quote sqlplus-output-separator)) nil t)
    (cond ((re-search-backward (concat "^" (regexp-quote sqlplus-output-separator)) nil t)
	   (forward-line 2)
	   (recenter 0))
	  (t
	   (message "No more commands.") (beep)
	   (goto-char start)))))

(defun sqlplus-send-interrupt nil
  "Send an interrupt the the SQL*Plus interpreter process."
  (interactive)
  (sqlplus-check-connection)
  (let ((connect-string sqlplus-connect-string))
    (sqlplus-verify-buffer connect-string)
    (interrupt-process (get-process (sqlplus-get-process-name connect-string)))))


;;;  SQL Interpreter

(defun refine-connect-string (connect-string &optional no-slash)
  "Z connect stringa do SQL*Plusa wycina haslo, tj. np. 'ponaglenia/x@SID' -> ('ponaglenia@SID' . 'x')."
  (let (result passwd)
    (when connect-string
      (setq result
	    (if (string-match "\\(\\`[^@/]*?\\)/\\([^/@:]*\\)\\(.*?\\'\\)" connect-string)
                (progn
                  (setq passwd (match-string 2 connect-string))
                  (concat (match-string 1 connect-string) (match-string 3 connect-string)))
	      connect-string))
      (when no-slash
	(while (string-match "/" result)
	  (setq result (replace-match "!" nil t result)))))
    (cons result passwd)))

(defun sqlplus-get-output-buffer-name (connect-string)
  (concat "*" (car (refine-connect-string connect-string)) "*"))

(defun sqlplus-get-input-buffer-name (connect-string)
  (concat (car (refine-connect-string connect-string)) (concat "." sqlplus-session-file-extension)))

(defun sqlplus-get-history-buffer-name (connect-string)
  (concat " " (car (refine-connect-string connect-string)) "-hist"))

(defun sqlplus-get-process-buffer-name (connect-string)
  (concat " " (car (refine-connect-string connect-string))))

(defun sqlplus-get-process-name (connect-string)
  (car (refine-connect-string connect-string)))

(defun sqlplus-read-connect-string (&optional connect-string default-connect-string)
  "Ask user for connect string with password, with DEFAULT-CONNECT-STRING proposed.
DEFAULT-CONNECT-STRING nil means first inactive connect-string on sqlplus-connect-strings-alist.
CONNECT-STRING non nil means ask for password only if CONNECT-STRING has no password itself.
Returns (qualified-connect-string refined-connect-string)."
  (unless default-connect-string
    (let ((inactive-connect-strings (cdr (sqlplus-divide-connect-strings))))
      (setq default-connect-string
	    (some (lambda (pair)
		    (when (member (car pair) inactive-connect-strings) (car pair)))
		  sqlplus-connect-strings-alist))))
  (let* ((cs (downcase (or connect-string 
			   (read-string (format "Connect string%s: " (if default-connect-string (format " [default %s]" default-connect-string) ""))
					nil 'sqlplus-connect-string-history default-connect-string))))
         (pair (refine-connect-string cs))
         (refined-cs (car pair))
         (password (cdr pair))
         (was-password password)
         (association (assoc refined-cs sqlplus-connect-strings-alist)))
    (unless (or password current-prefix-arg)
      (setq password (cdr association)))
    (unless password
      (setq password (read-passwd (format "Password for %s: " cs))))
    (unless was-password
      (if (string-match "@" cs)
          (setq cs (replace-match (concat "/" password "@") t t cs))
        (setq cs (concat cs "/" password))))
    (list cs refined-cs)))

;;;###autoload
(defun sqlplus (connect-string &optional input-buffer-name output-buffer-flag)
  "Create SQL*Plus process connected to Oracle according to
CONNECT-STRING, open (or create) input buffer with specified
name (do not create if INPUT-BUFFER-NAME is nil).
OUTPUT-BUFFER-FLAG has meanings: nil or SHOW-OUTPUT-BUFFER -
create output buffer and show it, DONT-SHOW-OUTPUT-BUFFER -
create output buffer but dont show it, DONT-CREATE-OUTPUT-BUFFER
- dont create output buffer"
  (interactive (let ((pair (sqlplus-read-connect-string)))
		 (list (car pair) (concat (cadr pair) (concat "." sqlplus-session-file-extension)))))
  (set (make-local-variable 'comment-start-skip) "/\\*+ *\\|--+ *")
  (set (make-local-variable 'comment-multi-line) t)
  ;; create sqlplus-session-cache-dir if not exists
  (when sqlplus-session-cache-dir
    (condition-case err
        (unless (file-directory-p sqlplus-session-cache-dir)
          (make-directory sqlplus-session-cache-dir t))
      (error (message (error-message-string err)))))
  (let* ((was-input-buffer (and input-buffer-name (get-buffer input-buffer-name)))
         (input-buffer (or was-input-buffer
                           (when input-buffer-name
                             (if sqlplus-session-cache-dir
                                 (let ((buf (find-file-noselect
                                             (concat
                                              (file-name-as-directory sqlplus-session-cache-dir)
                                              (car (refine-connect-string connect-string t))
                                              (concat "." sqlplus-session-file-extension)))))
                                   (condition-case nil
                                       (with-current-buffer buf
                                         (rename-buffer input-buffer-name))
                                     (error nil))
                                   buf)
                               (get-buffer-create input-buffer-name)))))
         (output-buffer (or (and (not (eq output-buffer-flag 'dont-create-output-buffer))
				 (get-buffer-create (sqlplus-get-output-buffer-name connect-string)))
			    (get-buffer (sqlplus-get-output-buffer-name connect-string))))
	 (process-name (sqlplus-get-process-name connect-string))
	 (process-buffer-name (sqlplus-get-process-buffer-name connect-string))
         (was-process (get-process process-name))
         process-created
         (process (or was-process
                      (let (proc)
			(puthash (car (refine-connect-string connect-string))
				 (make-sqlplus-global-struct :font-lock-regexps (make-hash-table :test 'equal)
							     :side-view-buffer (when (featurep 'ide-skel) (sqlplus-create-side-view-buffer connect-string)))
				 sqlplus-global-structures)
			;; push current connect string to the beginning of sqlplus-connect-strings-alist
			(let* ((refined-cs (refine-connect-string connect-string)))
			  (setq sqlplus-connect-strings-alist (delete* (car refined-cs) sqlplus-connect-strings-alist :test 'string= :key 'car))
			  (push refined-cs sqlplus-connect-strings-alist))
			(sqlplus-get-history-buffer connect-string)
			(when output-buffer
			  (with-current-buffer output-buffer
			    (erase-buffer)))
			(setq process-created t
			      proc (start-process process-name process-buffer-name sqlplus-command connect-string))
			(set-process-sentinel proc (lambda (process event)
							 (let ((proc-buffer (buffer-name (process-buffer process)))
							       (output-buffer (get-buffer (sqlplus-get-output-buffer-name (process-name process))))
							       err-msg
							       (exited-abnormally (string-match "\\`exited abnormally with code" event)))
							   (when output-buffer
							     (with-current-buffer output-buffer
							       (goto-char (point-max))
							       (insert (format "\n%s" event))
							       (when exited-abnormally
								 (setq sqlplus-connect-strings-alist
								       (delete* (car (refine-connect-string sqlplus-connect-string))
										sqlplus-connect-strings-alist :test 'string= :key 'car))
								 (when proc-buffer
								   (with-current-buffer proc-buffer
								     (save-excursion
								       (goto-char (point-min))
								       (when (re-search-forward "^ORA-[0-9]+.*$" nil t)
									 (setq err-msg (match-string 0))))
								     (erase-buffer)))
								 (when err-msg
								   (insert (concat "\n" err-msg)))))))))
			(process-kill-without-query proc (not sqlplus-kill-processes-without-query-on-exit-flag))
			(set-process-filter proc 'sqlplus-process-filter)
			(with-current-buffer (get-buffer process-buffer-name)
			  (setq sqlplus-process-p connect-string))
			proc))))
    (when output-buffer
      (with-current-buffer output-buffer
	(orcl-mode 1)
	(set (make-local-variable 'line-move-ignore-invisible) t)
	(setq sqlplus-output-buffer-keymap (make-sparse-keymap)
	      sqlplus-connect-string connect-string
	      truncate-lines t)
	(define-key sqlplus-output-buffer-keymap "\C-m" (lambda () (interactive) (sqlplus-output-buffer-hide-show)))
	(define-key sqlplus-output-buffer-keymap [S-mouse-2] (lambda (event) (interactive "@e") (sqlplus-output-buffer-hide-show)))
	(local-set-key [S-return] 'sqlplus-send-user-string)))
    (when input-buffer
      (with-current-buffer input-buffer
        (setq sqlplus-connect-string connect-string)))
    ;; if input buffer was created then switch it to sqlplus-mode
    (when (and input-buffer (not was-input-buffer))
      (with-current-buffer input-buffer
        (unless (eq major-mode 'sqlplus-mode)
          (sqlplus-mode)))
      (when font-lock-mode (font-lock-mode 1))
      (set-window-buffer (sqlplus-get-workbench-window) input-buffer))
    ;; if process was created then get information for font lock
    (when process-created
      (sqlplus-execute connect-string nil nil (sqlplus-initial-strings) 'no-echo)
      (let ((plsql-font-lock-level (sqlplus-font-lock-value-in-major-mode font-lock-maximum-decoration 'plsql-mode))
            (sqlplus-font-lock-level (sqlplus-font-lock-value-in-major-mode font-lock-maximum-decoration 'sqlplus-mode)))
        (when (or (equal plsql-font-lock-level t) (equal sqlplus-font-lock-level t)
                  (and (numberp plsql-font-lock-level) (>= plsql-font-lock-level 2))
                  (and (numberp sqlplus-font-lock-level) (>= sqlplus-font-lock-level 2)))
          (sqlplus-hidden-select connect-string 
                                 (concat "select distinct column_name, 'COLUMN', ' ' from user_tab_columns where column_name not like 'BIN$%'\n"
                                         "union\n"
                                         "select username, 'SCHEMA', ' ' from all_users where username not like 'BIN$%'\n"
                                         "union\n"
                                         "select object_name, object_type, decode( status, 'INVALID', 'I', ' ' ) from user_objects\n"
                                         "where object_name not like 'BIN$%'\n"
					 "and object_type in ('VIEW', 'SEQUENCE', 'PACKAGE', 'TRIGGER', 'TABLE', 'SYNONYM', 'INDEX', 'FUNCTION', 'PROCEDURE');")
                                 'sqlplus-my-handler))))
    (when input-buffer
      (save-selected-window
        (when (equal (selected-window) (sqlplus-get-side-window))
          (select-window (sqlplus-get-workbench-window)))
        (switch-to-buffer input-buffer)))
    (let ((saved-window (cons (selected-window) (window-buffer (selected-window))))
	  (input-buffer (get-buffer (sqlplus-get-input-buffer-name connect-string))))
      (when (or (eq output-buffer-flag 'show-output-buffer) (null output-buffer-flag))
	(sqlplus-show-buffer connect-string))
      (if (window-live-p (car saved-window))
	  (select-window (car saved-window))
	(if (get-buffer-window (cdr saved-window))
	    (select-window (get-buffer-window (cdr saved-window)))
	  (when (and input-buffer
		     (get-buffer-window input-buffer))
	    (select-window (get-buffer-window input-buffer))))))
    ;; executing initial sequence (between /* init */ and /* end */)
    (when (and (not was-process) input-buffer)
      (with-current-buffer input-buffer
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward (concat "^" sqlplus-init-sequence-start-regexp "\\s-*\n\\(\\(.\\|\n\\)*?\\)\n" sqlplus-init-sequence-end-regexp) nil t)
            (when (match-string 1)
              (sqlplus-send-region nil (match-beginning 1) (match-end 1) t))))))))

;; Command under cursor selection mechanism
(when window-system
  (run-with-idle-timer 0 t (lambda () (when (eq major-mode 'sqlplus-mode) (sqlplus-highlight-current-sqlplus-command))))
  (run-with-idle-timer 1 t (lambda ()
			     (when (eq major-mode 'sqlplus-mode)
			       (if (>= (sqlplus-color-percentage (face-background 'default)) 50)
				   (set-face-attribute 'sqlplus-command-highlight-face nil
						       :background (sqlplus-shine-color (face-background 'default) (- sqlplus-command-highlighting-percentage)))
				 (set-face-attribute 'sqlplus-command-highlight-face nil
						     :background (sqlplus-shine-color (face-background 'default) sqlplus-command-highlighting-percentage)))))))

(defun sqlplus-output-buffer-hide-show ()
  (if (and (consp buffer-invisibility-spec)
           (assq 'hide-symbol buffer-invisibility-spec))
      (remove-from-invisibility-spec '(hide-symbol . t))
    (add-to-invisibility-spec '(hide-symbol . t)))
  (let ((overlay (car (overlays-at (point)))))
    (when overlay
      (goto-char (overlay-start overlay))
      (beginning-of-line)))
  (recenter 0))

(defun sqlplus-font-lock-value-in-major-mode (alist mode-symbol)
  (if (consp alist)
      (cdr (or (assq mode-symbol alist) (assq t alist)))
    alist))

(defun sqlplus-get-history-buffer (connect-string)
  (let* ((history-buffer-name (sqlplus-get-history-buffer-name connect-string))
         (history-buffer (get-buffer history-buffer-name)))
    (unless history-buffer
      (setq history-buffer (get-buffer-create history-buffer-name))
      (with-current-buffer history-buffer
        (setq sqlplus-cs connect-string)
        (add-hook 'kill-buffer-hook 'sqlplus-history-buffer-kill-function nil t)))
    history-buffer))

(defun sqlplus-history-buffer-kill-function ()
  (when sqlplus-history-dir
    (condition-case err
        (progn
          (unless (file-directory-p sqlplus-history-dir)
            (make-directory sqlplus-history-dir t))
          (append-to-file 1 (buffer-size) (concat (file-name-as-directory sqlplus-history-dir) (car (refine-connect-string sqlplus-cs t)) "-hist.txt")))
      (error (message (error-message-string err))))))

(defun sqlplus-soft-shutdown (connect-string)
  (unless (some (lambda (buffer)
		(with-current-buffer buffer
		  (and sqlplus-connect-string
		       (equal (car (refine-connect-string sqlplus-connect-string))
			      (car (refine-connect-string connect-string))))))
	      (buffer-list))
    (sqlplus-shutdown connect-string)))

;;;###autoload
(defun sqlplus-shutdown (connect-string &optional dont-kill-input-buffer)
  "Kill input, output and process buffer for specified CONNECT-STRING."
  (let ((input-buffers (delq nil (mapcar (lambda (buffer) (with-current-buffer buffer
                                                            (when (and (eq major-mode 'sqlplus-mode)
                                                                       (equal (car (refine-connect-string sqlplus-connect-string))
                                                                              (car (refine-connect-string connect-string))))
                                                              buffer))) (buffer-list))))
	(output-buffer (get-buffer (sqlplus-get-output-buffer-name connect-string)))
        (history-buffer (get-buffer (sqlplus-get-history-buffer-name connect-string)))
	(process-buffer (get-buffer (sqlplus-get-process-buffer-name connect-string))))
    (when history-buffer
      (kill-buffer history-buffer))
    (when (and process-buffer
	       (with-current-buffer process-buffer sqlplus-process-p))
      (when (get-process (sqlplus-get-process-name connect-string))
        (delete-process (sqlplus-get-process-name connect-string)))
      (kill-buffer process-buffer))
    (when (and output-buffer
	       (with-current-buffer output-buffer sqlplus-connect-string))
      (when (buffer-file-name output-buffer)
	(with-current-buffer output-buffer
	  (save-buffer)))
      (kill-buffer output-buffer))
    (dolist (input-buffer input-buffers)
      (when (buffer-file-name input-buffer)
        (with-current-buffer input-buffer
          (save-buffer)))
      (unless dont-kill-input-buffer
        (kill-buffer input-buffer)))))

(defun sqlplus-magic ()
  (let (bottom-message pos)
    (delete-region (point) (progn (beginning-of-line 3) (point)))
    (setq bottom-message (buffer-substring (point) (save-excursion (end-of-line) (point))))
    (setq pos (point))
    (when (re-search-forward "^-------" nil t)
      (delete-region pos (progn (beginning-of-line 2) (point)))
      (while (re-search-forward "|" (save-excursion (end-of-line) (point)) t)
	(save-excursion
	  (backward-char)
	  (if (or (bolp) (save-excursion (forward-char) (eolp)))
	      (while (member (char-after) '(?- ?|))
		(delete-char 1)
		(sqlplus-next-line))
	    (while (member (char-after) '(?- ?|))
	      (delete-char 1)
	      (insert " ")
	      (backward-char)
	      (sqlplus-next-line)))))
      (beginning-of-line 3)
      (re-search-forward "^---" nil t)
      (goto-char (match-beginning 0))
      (delete-region (point) (point-max))
      (insert (format "%s\n\n%s\n" sqlplus-repfooter bottom-message))
      )))
  

(defun sqlplus-process-command-output (context connect-string begin end interrupted)
  (let* ((output-buffer-name (sqlplus-get-output-buffer-name connect-string))
	 (output-buffer (get-buffer output-buffer-name))
         (process-buffer (sqlplus-get-process-buffer-name connect-string))
         str
	 error-list show-errors-p
	 slips-count
         (user-function (sqlplus-get-context-value context :user-function))
         (result-function (sqlplus-get-context-value context :result-table-function))
         (last-compiled-file-path (sqlplus-get-context-value context :last-compiled-file-path))
         (compilation-expected (sqlplus-get-context-value context :compilation-expected))
         (columns-count (sqlplus-get-context-value context :columns-count))
         (sql (sqlplus-get-context-value context :sql))
	 (original-buffer (current-buffer))
	 explain-plan
	 table-data)
    (setq slips-count columns-count)
    (with-temp-buffer
      (insert-buffer-substring original-buffer begin end)
      (goto-char (point-min))
      (while (re-search-forward (concat "\n+" (regexp-quote sqlplus-page-separator) "\n") nil t)
	(replace-match "\n"))
      (goto-char (point-min))
      (setq str (buffer-string))
      (while (string-match (concat "^" (regexp-quote sqlplus-repfooter) "\n") str)
	(setq str (replace-match "" nil t str)))

      ;; compilation errors?
      (goto-char (point-min))
      (skip-chars-forward "\n\t ")
      (when (and ;;(not (equal (point) (point-max)))
	     plsql-auto-parse-errors-flag
	     output-buffer
	     last-compiled-file-path
	     (re-search-forward "^\\(LINE/COL\\|\\(SP2\\|CPY\\|ORA\\)-[0-9]\\{4,5\\}:\\|No errors\\|Nie ma b..d.w\\|Keine Fehler\\|No hay errores\\|Identificateur erron\\|Nessun errore\\|N..o h.. erros\\)" nil t))
	(goto-char (point-min))
	(setq error-list (plsql-parse-errors last-compiled-file-path)
	      show-errors-p compilation-expected))

      ;; explain?
      (let ((case-fold-search t))
	(goto-char (point-min))
	(skip-chars-forward "\n\t ")
	(when (and sql
	           (string-match "^[\n\t ]*explain\\>" sql)
		   (looking-at "Explained[.]"))
	  (delete-region (point-min) (point-max))
	  (setq str "")
	  (sqlplus--send connect-string
			 "select plan_table_output from table(dbms_xplan.display(null, null, 'TYPICAL'));"
			 nil
			 'no-echo
			 nil)))

      ;; plan table output?
      (goto-char (point-min))
      (skip-chars-forward "\n\t ")
      (when (and (looking-at "^PLAN_TABLE_OUTPUT\n")
		 sqlplus-format-output-tables-flag
		 (not compilation-expected)
		 (not show-errors-p))
	(sqlplus-magic) ;; TODO
	(goto-char (point-min))
	(re-search-forward "^[^\n]+" nil t)
	(delete-region (point-min) (progn (beginning-of-line) (point)))
	;; (setq slips-count 1)
	(setq explain-plan t)
	(setq table-data (save-excursion (sqlplus-parse-output-table interrupted))))
	
      ;; query result?
      (goto-char (point-min))
      (when (and sqlplus-format-output-tables-flag
		 (not compilation-expected)
		 (not table-data)
		 (not show-errors-p)
		 (not (re-search-forward "^LINE/COL\\>" nil t)))
	(setq table-data (save-excursion (sqlplus-parse-output-table interrupted))))
      (if user-function
	  (funcall user-function connect-string context (or table-data str))
	(when output-buffer
	  (with-current-buffer output-buffer
	    (save-excursion
	      (goto-char (point-max))
	      (cond (show-errors-p
		     (insert str)
		     (plsql-display-errors (file-name-directory last-compiled-file-path) error-list)
		     (let* ((plsql-buf (get-file-buffer last-compiled-file-path))
			    (win (when plsql-buf (car (get-buffer-window-list plsql-buf)))))
		       (when win
			 (select-window win))))
		    ((and table-data
			  (car table-data))
		       (if result-function
			   (funcall result-function connect-string table-data)
			 (let ((b (point))
			       (warning-regexp (regexp-opt sqlplus-explain-plan-warning-regexps))
			       e)
			   (sqlplus-draw-table table-data slips-count)
			   (when interrupted (insert ". . .\n"))
			   (setq e (point))
			   (when explain-plan
			     (save-excursion
			       (goto-char b)
			       (while (re-search-forward warning-regexp nil t)
				 (add-text-properties (match-beginning 0) (match-end 0)
						      (list 'face (list (cons 'foreground-color "red") (list :weight 'bold)
									(get-text-property (match-beginning 0) 'face))))))))))
		    (t
		     (insert str))))))))))

(defun sqlplus-result-online (connect-string context string last-chunk)
  (let ((output-buffer (sqlplus-get-output-buffer-name connect-string)))
    (when output-buffer
      (with-current-buffer output-buffer
        (save-excursion
          (goto-char (point-max))
          (insert string))))))

(defvar sqlplus-prompt-regexp (concat "^" (regexp-quote sqlplus-prompt-prefix) "\\([0-9]+\\)" (regexp-quote sqlplus-prompt-suffix)))

(defvar sqlplus-page-separator-regexp (concat "^" (regexp-quote sqlplus-page-separator)))

(defun sqlplus-process-filter (process string)
  (with-current-buffer (process-buffer process)
    (let* ((prompt-safe-len (+ (max (+ (length sqlplus-prompt-prefix) (length sqlplus-prompt-suffix)) (length sqlplus-page-separator)) 10))
           current-context-id filter-input-processed
           (connect-string sqlplus-process-p)
           (chunk-begin-pos (make-marker))
           (chunk-end-pos (make-marker))
           (prompt-found (make-marker))
	   (context (sqlplus-get-context connect-string current-context-id))
	   (current-command-input-buffer-name (sqlplus-get-context-value context :current-command-input-buffer-name))
	   (current-command-input-buffer-names (when current-command-input-buffer-name (list current-command-input-buffer-name))))
      (set-marker chunk-begin-pos (max 1 (- (point) prompt-safe-len)))
      (goto-char (point-max))
      (insert string)
      (unless current-command-input-buffer-names
	(setq current-command-input-buffer-names
	      (delq nil (mapcar (lambda (buffer) (with-current-buffer buffer
						   (when (and (memq major-mode '(sqlplus-mode plsql-mode))
							      sqlplus-connect-string
							      (equal (car (refine-connect-string sqlplus-connect-string))
								     (car (refine-connect-string connect-string))))
						     buffer))) (buffer-list)))))
      ;; fan animation
      (dolist (current-command-input-buffer-name current-command-input-buffer-names)
	(let ((input-buffer (get-buffer current-command-input-buffer-name)))
	  (when input-buffer
	    (with-current-buffer input-buffer
	      (setq sqlplus-fan
		    (cond ((equal sqlplus-fan "|") "/")
			  ((equal sqlplus-fan "/") "-")
			  ((equal sqlplus-fan "-") "\\")
			  ((equal sqlplus-fan "\\") "|")))
	      (put-text-property 0 (length sqlplus-fan) 'face '((foreground-color . "red")) sqlplus-fan)
	      (put-text-property 0 (length sqlplus-fan) 'help-echo (sqlplus-get-context-value context :sql) sqlplus-fan)
	      (force-mode-line-update)))))
      (unwind-protect
          (while (not filter-input-processed)
            (let* ((context (sqlplus-get-context connect-string current-context-id))
		   (dont-parse-result (sqlplus-get-context-value context :dont-parse-result))
                   (current-command-input-buffer-name (sqlplus-get-context-value context :current-command-input-buffer-name))
                   (result-function (sqlplus-get-context-value context :result-function))
                   (skip-to-the-end-of-command (sqlplus-get-context-value context :skip-to-the-end-of-command)))
              (set-marker prompt-found nil)
	      (goto-char chunk-begin-pos)
	      (set-marker chunk-end-pos
			  (if (or (re-search-forward sqlplus-prompt-regexp nil t)
				  (re-search-forward "^SQL> " nil t))
			      (progn
				(set-marker prompt-found (match-end 0))
				(when (match-string 1)
				  (setq current-context-id (string-to-number (match-string 1))))
				(match-beginning 0))
			    (point-max)))
              (cond ((and (equal chunk-begin-pos chunk-end-pos) ; at the end of command
                          (marker-position prompt-found))
		     ;; deactivate fan
		     (dolist (current-command-input-buffer-name current-command-input-buffer-names)
                       (let ((input-buffer (get-buffer current-command-input-buffer-name)))
                         (when input-buffer
                           (with-current-buffer input-buffer
			     (remove-text-properties 0 (length sqlplus-fan) '(face nil) sqlplus-fan)
                             (force-mode-line-update)))))
                     (delete-region 1 prompt-found)
		     (when dont-parse-result
		       (funcall (or result-function 'sqlplus-result-online) connect-string context "" t))
                     (sqlplus-set-context-value context :skip-to-the-end-of-command nil)
                     (set-marker chunk-begin-pos 1))
                    ((equal chunk-begin-pos chunk-end-pos)
		     (when dont-parse-result
		       (delete-region 1 (point-max)))
                     (setq filter-input-processed t))
                    (dont-parse-result
                     (funcall (or result-function 'sqlplus-result-online)
                              connect-string
                              context
                              (buffer-substring chunk-begin-pos chunk-end-pos)
                              (marker-position prompt-found))
                     (set-marker chunk-begin-pos chunk-end-pos))
                    (t
		     (when (not skip-to-the-end-of-command)
		       (goto-char (max 1 (- chunk-begin-pos 4010)))
		       (let ((page-separator-found 
			      (save-excursion (let ((pos (re-search-forward (concat sqlplus-page-separator-regexp "[^-]*\\(^-\\|^<th\\b\\)") nil t)))
						(when (and pos
							   (or (not (marker-position prompt-found))
							       (< pos prompt-found)))
						  (match-beginning 0))))))
			 (when (or (marker-position prompt-found) page-separator-found)
			   (goto-char (or page-separator-found chunk-end-pos))
			   (let ((end-pos (point))
				 (cur-msg (or (current-message) "")))
			     (sqlplus-set-context-value context :skip-to-the-end-of-command page-separator-found)
			     (when page-separator-found
			       (interrupt-process)
			       (save-excursion
				 (re-search-backward "[^ \t\n]\n" nil t)
				 (setq end-pos (match-end 0))))
			     (if result-function
				 (save-excursion (funcall result-function context connect-string 1 end-pos page-separator-found))
			       (with-temp-message "Formatting output..."
				 (save-excursion (sqlplus-process-command-output context connect-string 1 end-pos page-separator-found)))
			       (message "%s" cur-msg))
			     (when page-separator-found
			       (delete-region 1 (+ page-separator-found (length sqlplus-page-separator)))
			       (set-marker chunk-end-pos 1))))))
		     (set-marker chunk-begin-pos chunk-end-pos)))))
	(goto-char (point-max))
        (set-marker chunk-begin-pos nil)
        (set-marker chunk-end-pos nil)
        (set-marker prompt-found nil)))))

(defadvice switch-to-buffer (around switch-to-buffer-around-advice (buffer-or-name &optional norecord force-same-window))
  ad-do-it
  (when (and sqlplus-connect-string
	     (eq major-mode 'sqlplus-mode))
    (let ((side-window (sqlplus-get-side-window))
          (output-buffer (get-buffer (sqlplus-get-output-buffer-name sqlplus-connect-string))))
      (when (and side-window
                 (not (eq (window-buffer) output-buffer)))
        (save-selected-window
          (switch-to-buffer-other-window output-buffer))))))
(ad-activate 'switch-to-buffer)

(defun sqlplus-kill-function ()
  (unless sqlplus-kill-function-inhibitor
    ;; shutdown connection if it is SQL*Plus output buffer or SQL*Plus process buffer
    (if (or (and sqlplus-connect-string (equal (buffer-name) (sqlplus-get-output-buffer-name sqlplus-connect-string)))
            sqlplus-process-p)
        (sqlplus--enqueue-task 'sqlplus-shutdown (or sqlplus-connect-string sqlplus-process-p))
      ;; input buffer or another buffer connected to SQL*Plus - possibly shutdown
      (when sqlplus-connect-string
        (let ((counter 0)
              (scs sqlplus-connect-string))
          (dolist (buffer (buffer-list))
            (with-current-buffer buffer
              (when (equal sqlplus-connect-string scs) (incf counter))))
          (when (<= counter 2)
            (let* ((process (get-process (sqlplus-get-process-name sqlplus-connect-string))))
              (when (or (not process)
                        (memq (process-status process) '(exit signal))
                        (y-or-n-p (format "Kill SQL*Plus process %s " (car (refine-connect-string sqlplus-connect-string)))))
                (sqlplus--enqueue-task 'sqlplus-shutdown sqlplus-connect-string)))))))))

(defun sqlplus-emacs-kill-function ()
  ;; save and kill all sqlplus-mode buffers
  (let (buffers-to-kill)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(when (and sqlplus-connect-string
		   (eq major-mode 'sqlplus-mode))
	  (when (buffer-file-name)
	    (save-buffer))
	  (push buffer buffers-to-kill))))
    (setq sqlplus-kill-function-inhibitor t)
    (condition-case nil
	(unwind-protect
	    (dolist (buffer buffers-to-kill)
	      (kill-buffer buffer))
	  (setq sqlplus-kill-function-inhibitor nil))
      (error nil))
    t))

(push 'sqlplus-emacs-kill-function kill-emacs-query-functions)

(add-hook 'kill-buffer-hook 'sqlplus-kill-function)

;; kill all history buffers so that they can save themselves
(add-hook 'kill-emacs-hook (lambda ()
                             (dolist (buf (copy-list (buffer-list)))
                               (when (and (string-match "@.*-hist" (buffer-name buf))
                                          (with-current-buffer buf sqlplus-cs))
                                 (kill-buffer buf)))))

(defun sqlplus-find-output-table (interrupted)
  "Search for table in last SQL*Plus command result, and return
list (BEGIN END MSG) for first and last table char, or nil if
table is not found."
  (let (begin end)
    (goto-char (point-min))
    (when (re-search-forward "^[^\n]+\n\\( \\)?-" nil t)
      (let (msg
	    (indent (when (match-string 1) -1))) ; result of 'describe' sqlplus command
	(forward-line -1)
	;; (untabify (point) (buffer-size))
	(setq begin (point))
	(when indent
	  (indent-rigidly begin (point-max) indent)
	  (goto-char begin))
	(if indent
	    (progn
	      (goto-char (point-max))
	      (skip-chars-backward "\n\t ")
	      (setq end (point))
	      (goto-char (point-max)))
	  (or (re-search-forward (concat "^" (regexp-quote sqlplus-repfooter) "\n[\n\t ]*") nil t)
              (when interrupted (re-search-forward "\\'" nil t))) ; \\' means end of buffer
	  (setq end (match-beginning 0))
	  (setq msg (buffer-substring (match-end 0) (point-max))))
	(list begin end msg)))))

(defstruct col-desc
  id              ; from 0
  name            ; column name
  start-pos       ; char column number
  end-pos         ; char column number
  max-width       ; max. column width
  preferred-width ; preferred column width
  min-prefix-len  ; min. prefix (spaces only)
  numeric         ; y if column is numeric, n if is not, nil if don't know
  has-eol         ; temporary value for processing current row
)

(defun sqlplus-previous-line ()
  (let ((col (current-column)))
    (forward-line -1)
    (move-to-column col t)))

(defun sqlplus-next-line ()
  (let ((col (current-column)))
    (forward-line 1)
    (move-to-column col t)))

(defun sqlplus--correct-column-name (max-col-no)
  (let ((counter 0)
	(big (1- (save-excursion (beginning-of-line) (point)))))
    (skip-chars-forward " ")    
    (when (re-search-forward "  [^ \n]" (+ big max-col-no) t)
      (backward-char)
      (while (< (point) (+ big max-col-no))
	(setq counter (1+ counter))
	(insert " ")))
    counter))

(defun sqlplus-parse-output-table (interrupted)
  "Parse table and return list (COLUMN-INFOS ROWS MSG) where
COLUMN-INFOS is a col-desc structures list, ROWS is a table of
records (record is a list of strings).  Return nil if table is
not detected."
  (let ((region (sqlplus-find-output-table interrupted)))
    (when region
      (let ((begin (car region))
	    (end (cadr region))
	    (last-msg (caddr region))
	    (col-counter 0)
	    column-infos rows
	    (record-lines 1)
	    finish)
	;; (message "'%s'\n'%s'" (buffer-substring begin end) last-msg)
	(goto-char begin)
	;; we are at the first char of column name
        ;; move to the first char of '-----' column separator
	(beginning-of-line 2)
	(while (not finish)
	  (if (equal (char-after) ?-)
              ;; at the first column separator char
	      (let* ((beg (point))
		     (col-begin (current-column))
		     (col-max-width (skip-chars-forward "-"))
                     ;; after last column separator char
		     (ed (point))
		     (col-end (+ col-begin col-max-width))
		     (col-name (let* ((b (progn
					   (goto-char beg)
					   (sqlplus-previous-line)
					   (save-excursion
					     (let ((counter (sqlplus--correct-column-name (1+ col-end))))
					       (setq beg (+ beg counter))
					       (setq ed (+ ed counter))))
					   (point)))
				      (e (+ b col-max-width)))
				 (skip-chars-forward " \t")
				 (setq b (point))
				 (goto-char (min (save-excursion (end-of-line) (point)) e))
				 (skip-chars-backward " \t")
				 (setq e (point))
				 (if (> e b)
				     (buffer-substring b e)
				   "")))
		     (col-preferred-width (string-width col-name)))
		;; (put-text-property 0 (length col-name) 'face '(bold) col-name)
		(push (make-col-desc :id col-counter :name col-name :start-pos col-begin
				     :end-pos col-end :max-width col-max-width :preferred-width col-preferred-width :min-prefix-len col-max-width)
		      column-infos)
		(incf col-counter)
		(goto-char ed)
		(if (equal (char-after) ?\n)
		    (progn
		      (beginning-of-line 3)
		      (incf record-lines))
		  (forward-char)))
	    (setq finish t)))
	(decf record-lines)
	(setq column-infos (nreverse column-infos))
	(forward-line -1)

        ;; at the first char of first data cell.
	;; table parsing...
	(while (< (point) end)
	  (let (record last-start-pos)
	    (dolist (column-info column-infos)
	      (let ((start-pos (col-desc-start-pos column-info))
		    (end-pos (col-desc-end-pos column-info))
		    width len value b e l)
		(when (and last-start-pos
			   (<= start-pos last-start-pos))
		  (forward-line))
		(setq last-start-pos start-pos)
		(move-to-column start-pos)
		(setq b (point))
		(move-to-column end-pos)
		(setq e (point))
		(move-to-column start-pos)
		(setq l (skip-chars-forward " " e))
		(when (and (col-desc-min-prefix-len column-info)
			   (< l (- e b))
			   (< l (col-desc-min-prefix-len column-info)))
		  (setf (col-desc-min-prefix-len column-info)
			(if (looking-at "[0-9]") l nil)))
		(move-to-column end-pos)
		(skip-chars-backward " " b)
		(setq value (if (> (point) b) (buffer-substring b (point)) ""))
		(setq len (length value)
		      width (string-width value))
		(when (and sqlplus-select-result-max-col-width
			   (> len sqlplus-select-result-max-col-width))
		  (setq value (concat (substring value 0 sqlplus-select-result-max-col-width) "...")
			len (length value)
			width (string-width value)))
		(when (> width (col-desc-preferred-width column-info))
		  (setf (col-desc-preferred-width column-info) width))
                (when (and (< l (- e b))
                           (memq (col-desc-numeric column-info) '(nil y)))
                  (setf (col-desc-numeric column-info)
                        (if (string-match "\\` *[-+0-9Ee.,$]+\\'" value) 'y 'n)))
		(push value record)))
	    (forward-line)
	    (when (> record-lines 1)
	      (forward-line))
	    (setq last-start-pos nil
		  record (nreverse record))
	    (push record rows)))
	(setq rows (nreverse rows))
	(list column-infos rows last-msg)))))

(defun sqlplus-draw-table (lst &optional slips-count)
  "SLIPS-COUNT (nil means compute automatically)."
  ;; current buffer: SQL*Plus output buffer
  (when window-system
    (if (>= (sqlplus-color-percentage (face-background 'default)) 50)
	(progn
	  (set-face-attribute 'sqlplus-table-head-face nil
			      :background (sqlplus-shine-color (face-background 'default) -70) :foreground (face-background 'default))
	  (set-face-attribute 'sqlplus-table-even-rows-face nil
			      :background (sqlplus-shine-color (face-background 'default) -20) :overline (face-background 'default))
	  (set-face-attribute 'sqlplus-table-odd-rows-face nil
			      :background (sqlplus-shine-color (face-background 'default) -30) :overline (face-background 'default)))
      (set-face-attribute 'sqlplus-table-head-face nil
			  :background (sqlplus-shine-color (face-background 'default) +70) :foreground (face-background 'default))
      (set-face-attribute 'sqlplus-table-even-rows-face nil
			  :background (sqlplus-shine-color (face-background 'default) +20) :overline (face-background 'default))
      (set-face-attribute 'sqlplus-table-odd-rows-face nil
			  :background (sqlplus-shine-color (face-background 'default) +30) :overline (face-background 'default))))
  (let* ((column-infos (car lst))
         (rows (cadr lst))
         (slip-width 0)
         (table-header-height 1)
         (table-area-width (1- (let ((side-window (sqlplus-get-side-window))) (if side-window (window-width side-window) (frame-width)))))
         ;; may be nil, which means no limit
         (table-area-height (let ((side-window (sqlplus-get-side-window)))
                              (when side-window
                                (- (window-height side-window) 2 (if mode-line-format 1 0) (if header-line-format 1 0)))))
         (column-separator-width (if sqlplus-elegant-style 1.2 (max (length sqlplus-table-col-separator) (length sqlplus-table-col-head-separator))))
         rows-per-slip ;; data rows per slip
         (slip-separator-width (if sqlplus-elegant-style 1.5 sqlplus-slip-separator-width))
         (slip-separator (make-string (max 0 (if sqlplus-elegant-style 1 sqlplus-slip-separator-width)) ?\ ))
         (last-msg (caddr lst)))
    (when sqlplus-elegant-style
      (put-text-property 0 1 'display (cons 'space (list :width slip-separator-width)) slip-separator))
    (when (<= table-area-height table-header-height)
      (setq table-area-height nil))
    (when (and window-system sqlplus-elegant-style table-area-height (> table-area-height 3))
      ;; overline makes glyph higher...
      (setq table-area-height (- table-area-height (round (/ (* 20.0 (- table-area-height 3)) (face-attribute 'default :height))))))
    (when column-infos
      (goto-char (point-max))
      (beginning-of-line)
      ;; slip width (without separator between slips)
      (dolist (col-info column-infos)
        (when (col-desc-min-prefix-len col-info)
          (setf (col-desc-preferred-width col-info) (max (string-width (col-desc-name col-info))
                                                         (- (col-desc-preferred-width col-info) (col-desc-min-prefix-len col-info)))))
	(incf slip-width (+ (col-desc-preferred-width col-info) column-separator-width)))
      (when (> slip-width 0)
        (setq slip-width (+ (- slip-width column-separator-width) (if sqlplus-elegant-style 1.0 0))))
      ;; computing slip count if not known yet
      (unless slips-count
	(setq slips-count
	      (if table-area-height (min (ceiling (/ (float (length rows)) (max 1 (- table-area-height table-header-height 2))))
					 (max 1 (floor (/ (float table-area-width) (+ slip-width slip-separator-width)))))
		1)))
      (setq slips-count (max 1 (min slips-count (length rows)))) ; slip count <= data rows
      (setq rows-per-slip (ceiling (/ (float (length rows)) slips-count)))
      (when (> rows-per-slip 0)
        (setq slips-count (max 1 (min (ceiling (/ (float (length rows)) rows-per-slip)) slips-count))))

      (let ((table-begin-point (point)))
	(dotimes (slip-no slips-count)
	  (let ((row-no 0)
		(slip-begin-point (point))
		(rows-processed 0))
	    ;; column names
	    (dolist (col-info column-infos)
	      (let* ((col-name (col-desc-name col-info))
		     (spaces (max 0 (- (col-desc-preferred-width col-info) (string-width col-name))))
                     (last-col-p (>= (1+ (col-desc-id col-info)) (length column-infos)))
		     (val (format (if sqlplus-elegant-style " %s%s %s" "%s%s%s")
                                  col-name
                                  (make-string spaces ?\ )
				  (if last-col-p "" (if sqlplus-elegant-style " " sqlplus-table-col-separator)))))
                (put-text-property 0 (if (or (not sqlplus-elegant-style) last-col-p) (length val) (1- (length val))) 
                                   'face 'sqlplus-table-head-face val)
                (when sqlplus-elegant-style
                  (put-text-property 0 1 'display '(space . (:width 0.5)) val)
                  (put-text-property (- (length val) (if last-col-p 1 2)) (- (length val) (if last-col-p 0 1)) 'display '(space . (:width 0.5)) val)
                  (unless last-col-p
                    (put-text-property (- (length val) 1) (length val) 'display '(space . (:width 0.2)) val)))
		(insert val)))
	    (insert slip-separator)
	    (insert "\n")
	    ;; data rows
	    (while (and (< rows-processed rows-per-slip)
			rows)
	      (let ((row (car rows)))
		(setq rows (cdr rows))
		(incf rows-processed)
		(let ((col-infos column-infos))
		  (dolist (value row)
		    (let* ((col-info (car col-infos))
			   (numeric-p (eq (col-desc-numeric col-info) 'y))
			   (min-prefix (col-desc-min-prefix-len col-info)))
		      (when (and min-prefix
				 value
				 (>= (length value) min-prefix))
			(setq value (substring value min-prefix)))
		      (let* ((spaces (max 0 (- (col-desc-preferred-width col-info) (string-width value))))
			     (val (if numeric-p
				      (format (if sqlplus-elegant-style " %s%s %s" "%s%s%s")
                                              (make-string spaces ?\ )
                                              value
                                              (if (cdr col-infos) (if sqlplus-elegant-style " " sqlplus-table-col-separator) ""))
				    (format (if sqlplus-elegant-style " %s%s %s" "%s%s%s")
                                            value
                                            (make-string spaces ?\ ) 
                                            (if (cdr col-infos) (if sqlplus-elegant-style " " sqlplus-table-col-separator) "")))))
			(put-text-property 0 (if (and sqlplus-elegant-style (cdr col-infos)) (- (length val) 1) (length val))
                                           'face (if (evenp row-no)
                                                     'sqlplus-table-even-rows-face
                                                   'sqlplus-table-odd-rows-face) val)
                        (when sqlplus-elegant-style
                          (put-text-property 0 1 'display '(space . (:width 0.5)) val)
                          (put-text-property (- (length val) (if (cdr col-infos) 2 1))
                                             (- (length val) (if (cdr col-infos) 1 0))
                                             'display '(space . (:width 0.5)) val)
                          (when (cdr col-infos)
                            (put-text-property (- (length val) 1) (length val) 'display '(space . (:width 0.2)) val)))
			(setq col-infos (cdr col-infos))
			(insert val))))
		  (incf row-no)
		  (insert slip-separator)
		  (insert "\n"))))
	    (when (> slip-no 0)
	      (delete-backward-char 1)
	      (let ((slip-end-point (point)))
		(kill-rectangle slip-begin-point slip-end-point)
		(delete-region slip-begin-point (point-max))
		(goto-char table-begin-point)
		(end-of-line)
		(yank-rectangle)
		(goto-char (point-max))
		))))
	(goto-char (point-max))
	(when (and last-msg (> (length last-msg) 0))
          (unless sqlplus-elegant-style (insert "\n"))
          (let ((s (format "%s\n\n" (replace-regexp-in-string "\n+" " " last-msg))))
            (when sqlplus-elegant-style
              (put-text-property (- (length s) 2) (1- (length s)) 'display '(space . (:height 1.5)) s))
            (insert s)))))))

(defun sqlplus-send-user-string (str)
  (interactive (progn (sqlplus-check-connection)
                      (if sqlplus-connect-string
                          (list (read-string "Send to process: " nil 'sqlplus-user-string-history ""))
                        (error "Works only in SQL*Plus buffer"))))
  (let ((connect-string sqlplus-connect-string))
    (sqlplus-verify-buffer connect-string)
    (let* ((process (get-process (sqlplus-get-process-name connect-string)))
           (output-buffer-name (sqlplus-get-output-buffer-name connect-string)))
      (sqlplus-echo-in-buffer output-buffer-name (concat str "\n"))
      (send-string process (concat str "\n")))))

(defun sqlplus-prepare-update-alist (table-data)
  (let ((column-infos (car table-data))
        (rows (cadr table-data))
        (msg (caddr table-data))
        alist)
    (dolist (row rows)
      (let* ((object-name (car row))
             (object-type (intern (downcase (cadr row))))
	     (status (caddr row))
             (regexp-list (cdr (assq object-type alist)))
	     (pair (cons object-name (equal status "I"))))
        (if regexp-list
            (setcdr regexp-list (cons pair (cdr regexp-list)))
          (setq regexp-list (list pair))
          (setq alist (cons (cons object-type regexp-list) alist)))))
    alist))

(defun sqlplus-my-update-handler (connect-string table-data)
  (let ((alist (sqlplus-prepare-update-alist table-data)))
    (when (featurep 'ide-skel)
      (funcall 'sqlplus-side-view-update-data connect-string alist))))

(defun sqlplus-my-handler (connect-string table-data)
  (let ((alist (sqlplus-prepare-update-alist table-data))
	(sqlplus-font-lock-regexps (sqlplus-get-font-lock-regexps connect-string)))
    (sqlplus-set-objects-alist alist connect-string)
    (when (featurep 'ide-skel)
      (funcall 'sqlplus-side-view-update-data connect-string alist))
    (clrhash sqlplus-font-lock-regexps)
    (dolist (lst sqlplus-syntax-faces)
      (let* ((object-type (car lst))
	     (regexp-list (append (caddr lst) (mapcar 'car (cdr (assq object-type alist))))))
	(when regexp-list
	  (puthash object-type (concat "\\b" (regexp-opt regexp-list t) "\\b") sqlplus-font-lock-regexps))))
    (let ((map sqlplus-font-lock-regexps))
      (mapc (lambda (buffer)
	      (with-current-buffer buffer
		(when (and (memq major-mode '(sqlplus-mode plsql-mode))
			   (equal sqlplus-connect-string connect-string))
		  (when font-lock-mode (font-lock-mode 1)))))
	    (buffer-list)))))

(defun sqlplus-get-source-function (connect-string context string last-chunk)
  (let* ((source-text (sqlplus-get-context-value context :source-text))
	 (source-type (sqlplus-get-context-value context :source-type))
	 (source-name (sqlplus-get-context-value context :source-name))
	 (source-extension (sqlplus-get-context-value context :source-extension))
	 (name (concat (upcase source-name) "." source-extension))
         finish)
    (unless (sqlplus-get-context-value context :finished)
      (setq source-text (concat source-text string))
      (sqlplus-set-context-value context :source-text source-text)
      (when last-chunk
	(if (string-match (regexp-quote sqlplus-end-of-source-sentinel) source-text)
	    (when (< (length source-text) (+ (length sqlplus-end-of-source-sentinel) 5))
	      (setq last-chunk nil
		    finish "There is no such database object"))
	  (setq last-chunk nil)))
      (when last-chunk
	(setq finish t))
      (when finish
	(sqlplus-set-context-value context :finished t)
	(if (stringp finish)
	    (message finish)
	  (with-temp-buffer
	    (insert source-text)
	    (goto-char (point-min))
	    (re-search-forward (regexp-quote sqlplus-end-of-source-sentinel) nil t)
	    (replace-match "")
	    (goto-char (point-max))
	    (forward-comment (- (buffer-size)))
	    (when (equal source-type "TABLE")
	      (goto-char (point-min))
	      (insert (format "table %s\n(\n" source-name))
	      (goto-char (point-max))
	      (delete-region (re-search-backward "," nil t) (point-max))
	      (insert "\n);"))
	    (insert "\n/\n")
	    (unless (member source-type '("SEQUENCE" "TABLE" "SYNONYM" "INDEX"))
	      (insert "show err\n"))
	    (goto-char (point-min))
	    (insert "create " (if (member source-type '("INDEX" "SEQUENCE" "TABLE")) "" "or replace "))
	    (setq source-text (buffer-string)))
	  (with-current-buffer (get-buffer-create name)
	    (setq buffer-read-only nil)
	    (erase-buffer)
	    (insert source-text)
	    (goto-char (point-min))
	    (set-visited-file-name (concat (file-name-as-directory temporary-file-directory)
					   (concat (make-temp-name (sqlplus-canonize-file-name (concat (upcase source-name) "_") "[$]")) "." source-extension)))
	    (rename-buffer name)
	    (condition-case err
		(funcall (symbol-function 'plsql-mode))
	      (error nil))
	    (setq sqlplus-connect-string connect-string
		  buffer-read-only sqlplus-source-buffer-readonly-by-default-flag)
	    (save-buffer)
	    (save-selected-window
	      (let ((win (selected-window)))
		(when (or (equal win (sqlplus-get-side-window))
			  (and (fboundp 'ide-skel-side-view-window-p)
			       (funcall 'ide-skel-side-view-window-p win)))
		  (setq win (sqlplus-get-workbench-window)))
		(set-window-buffer win (current-buffer))))))))))
    
(defun sqlplus-get-source (connect-string name type &optional schema-name)
  "Fetch source for database object NAME in current or specified SCHEMA-NAME, and show the source in new buffer.
Possible TYPE values are in 'sqlplus-object-types'."
  (interactive (let* ((thing (thing-at-point 'symbol))
                      (obj-raw-name (read-string (concat "Object name" (if thing (concat " [default " thing "]") "") ": ")
                                                 nil
                                                 'sqlplus-get-source-history (when thing thing)))
                      (completion-ignore-case t)
                      (type (completing-read "Object type: " (mapcar (lambda (type) (cons type nil)) sqlplus-object-types) nil t)))
                 (string-match "^\\(\\([^.]+\\)[.]\\)?\\(.*\\)$" obj-raw-name)
                 (list sqlplus-connect-string (match-string 3 obj-raw-name) type (match-string 2 obj-raw-name))))
  (setq type (upcase type))
  (let* ((sql
          (cond ((equal type "SEQUENCE")
                 (format (concat "select 'sequence %s' || sequence_name || "
                                 "decode( increment_by, 1, '', ' increment by ' || increment_by ) || "
                                 "case when increment_by > 0 and max_value >= (1.0000E+27)-1 or increment_by < 0 and max_value = -1 then '' "
                                 "else decode( max_value, null, ' nomaxvalue', ' maxvalue ' || max_value) end || "
                                 "case when increment_by > 0 and min_value = 1 or increment_by < 0 and min_value <= (-1.0000E+26)+1 then '' "
                                 "else decode( min_value, null, ' nominvalue', ' minvalue ' || min_value) end || "
                                 "decode( cycle_flag, 'Y', ' cycle', '' ) || "
                                 "decode( cache_size, 20, '', 0, ' nocache', ' cache ' || cache_size ) || "
                                 "decode( order_flag, 'Y', ' order', '' ) "
                                 "from %s where sequence_name = '%s'%s;")
                         (if schema-name (concat (upcase schema-name) ".") "")
                         (if schema-name "all_sequences" "user_sequences")
                         (upcase name)
                         (if schema-name (format " and sequence_owner = '%s'" (upcase schema-name)) "")))
                ((equal type "TABLE")
                 (format (concat "select '  ' || column_name || ' ' || data_type || "
                                 "decode( data_type,"
                                 " 'VARCHAR2', '(' || to_char( data_length, 'fm9999' ) || ')',"
                                 " 'NUMBER', decode( data_precision,"
                                 "             null, '',"
                                 "             '(' || to_char( data_precision, 'fm9999' ) || decode( data_scale,"
                                 "                                                      null, '',"
                                 "                                                      0, '',"
                                 "                                                      ',' || data_scale ) || ')' ),"
                                 " '') || "
                                 "decode( nullable, 'Y', ' not null', '') || ','"
                                 "from all_tab_columns "
                                 "where owner = %s and table_name = '%s' "
                                 "order by column_id;")
                         (if schema-name (concat "'" (upcase schema-name) "'") "user")
                         (upcase name)))
                ((equal type "SYNONYM")
                 (format (concat "select "
                                 "decode( owner, 'PUBLIC', 'public ', '' ) || 'synonym ' || "
                                 "decode( owner, 'PUBLIC', '', user, '', owner || '.' ) || synonym_name || ' for ' || "
                                 "decode( table_owner, user, '', table_owner || '.' ) || table_name || "
                                 "decode( db_link, null, '', '@' || db_link ) "
                                 "from all_synonyms where (owner = 'PUBLIC' or owner = %s) and synonym_name = '%s';")
                         (if schema-name (concat "'" (upcase schema-name) "'") "user")
                         (upcase name)))
                ((equal type "VIEW")
                 (if schema-name (format "select 'view %s.' || view_name || ' as ', text from all_views where owner = '%s' and view_name = '%s';"
                                         (upcase schema-name) (upcase schema-name) (upcase name))
                   (format "select 'view ' || view_name || ' as ', text from user_views where view_name = '%s';" (upcase name))))
		((or (equal type "PROCEDURE")
		     (equal type "FUNCTION"))
		 (if schema-name (format "select text from all_source where owner = '%s' and name = '%s' and type in ('PROCEDURE', 'FUNCTION') order by line;"
                                         (upcase schema-name) (upcase name))
                   (format "select text from user_source where name = '%s' and type in ('PROCEDURE', 'FUNCTION') order by line;"
                           (upcase name))))
                (t
                 (if schema-name (format "select text from all_source where owner = '%s' and name = '%s' and type = '%s' order by line;"
                                         (upcase schema-name) (upcase name) (upcase type))
                   (format "select text from user_source where name = '%s' and type = '%s' order by line;"
                           (upcase name) (upcase type))))))
         (prolog-commands (list "set echo off"
                                "set newpage 0"
                                "set space 0"
                                "set pagesize 0"
                                "set feedback off"
                                "set long 4000"
                                "set longchunksize 4000"
                                "set wrap on"
                                "set heading off"
                                "set trimspool on"
                                "set linesize 4000"
                                "set timing off"))
         (extension (if (equal (downcase type) "package") "pks" "sql"))
         (source-buffer-name (concat " " (upcase name) "." extension))
         (context-options (list (cons :dont-parse-result 'dont-parse)
                                (cons :source-text nil)
                                (cons :source-type type)
                                (cons :source-name name)
				(cons :source-extension extension)
                                (cons :result-function 'sqlplus-get-source-function))))
    (sqlplus-execute connect-string sql context-options prolog-commands t t)
    (sqlplus-execute connect-string (format "select '%s' from dual;" sqlplus-end-of-source-sentinel) context-options prolog-commands t t)))

(defun sqlplus-canonize-file-name (file-name regexp)
  (while (string-match regexp file-name)
    (setq file-name (replace-match "!" nil t file-name)))
  file-name)

(defun sqlplus-define-user-variables (string)
  (when string
    (let (variables-list
          define-commands
          (index 0))
      (while (setq index (string-match "&+\\(\\(\\sw\\|\\s_\\)+\\)" string index))
        (let ((var-name (match-string 1 string)))
          (setq index (+ 2 index))
          (unless (member var-name variables-list)
            (push var-name variables-list))))
      (dolist (var-name (reverse variables-list))
        (let* ((default-value (gethash var-name sqlplus-user-variables nil))
               (value (read-string (format (concat "Variable value for %s" (if default-value (format " [default: %s]" default-value) "") ": ") var-name)
                                   nil 'sqlplus-user-variables-history default-value)))
          (unless value
            (error "There is no value for %s defined" var-name))
          (setq define-commands (cons (format "define %s=%s" var-name value) define-commands))
          (puthash var-name value sqlplus-user-variables)))
      define-commands)))
    
(defun sqlplus-parse-region (start end)
  (let ((sql (buffer-substring start end)))
    (save-excursion
      ;; Strip whitespace from beginning and end, just to be neat.
      (if (string-match "\\`[ \t\n]+" sql)
          (setq sql (substring sql (match-end 0))))
      (if (string-match "[ \t\n]+\\'" sql)
          (setq sql (substring sql 0 (match-beginning 0))))
      (setq sql (replace-regexp-in-string "^[ \t]*--.*[\n]?" "" sql))
      (when (zerop (length sql))
	(error "Nothing to send"))
      ;; Now the string should end with an sqlplus-terminator.
      (if (not (string-match "\\(;\\|/\\|[.]\\)\\'" sql))
          (setq sql (concat sql ";"))))
    sql))

(defun sqlplus-show-html-fun (context connect-string begin end interrupted)
  (let ((output-file (expand-file-name (substitute-in-file-name sqlplus-html-output-file-name)))
        (sql (sqlplus-get-context-value context :htmlized-html-command))
        (html (buffer-substring begin end))
        (header-html (eval sqlplus-html-output-header)))
    (let ((case-fold-search t))
      (while (and (string-match "\\`[ \t\n]*\\(<br>\\|<p>\\)?" html) (match-string 0 html) (> (length (match-string 0 html)) 0))
        (setq html (replace-match "" nil t html)))
      (when (> (length html) 0)
        (sqlplus-execute connect-string "" nil '("set markup html off") 'no-echo 'dont-show-output-buffer)
        (find-file output-file)
        (erase-buffer)
        (insert (concat "<html>\n"
                        "<head>\n"
                        "  <meta http-equiv=\"content-type\" content=\"text/html; charset=" sqlplus-html-output-encoding "\">\n"
                        (sqlplus-get-context-value context :head) "\n"
                        "</head>\n"
                        "<body " (sqlplus-get-context-value context :body) ">\n"
                        (if header-html header-html "")
                        (if sqlplus-html-output-sql sql "")
                        "<p>"
                        html "\n"
                        "</body>\n"
                        "</html>"))
        (goto-char (point-min))
        (save-buffer)))))

(defun sqlplus-refine-html (html remove-entities)
  (string-match "\\`\"?\\(\\(.\\|\n\\)*?\\)\"?[ \t\n]*\\'" html)
  (setq html (match-string 1 html))
  (if remove-entities
      (progn
        (while (string-match "&quot;" html) (setq html (replace-match "\"" nil t html)))
        (while (string-match "&lt;" html) (setq html (replace-match "<" nil t html)))
        (while (string-match "&gt;" html) (setq html (replace-match ">" nil t html)))
        (while (string-match "&amp;" html) (setq html (replace-match "&" nil t html))))
    (while (string-match "&" html) (setq html (replace-match "&amp;" nil t html)))
    (while (string-match ">" html) (setq html (replace-match "&gt;" nil t html)))
    (while (string-match "<" html) (setq html (replace-match "&lt;" nil t html)))
    (while (string-match "\"" html) (setq html (replace-match "&quot;" nil t html))))
  (string-match "\\`\"?\\(\\(.\\|\n\\)*?\\)\"?[ \t\n]*\\'" html)
  (setq html (match-string 1 html))
  html)

(defun sqlplus-show-markup-fun (context connect-string begin end interrupted)
  (goto-char begin)
  (let ((head "")
        (body "")
        preformat)
    (when (re-search-forward (concat "\\bHEAD\\b[ \t\n]*\\(\\(.\\|\n\\)*\\)[ \t\n]*"
                                     "\\bBODY\\b[ \t\n]*\\(\\(.\\|\n\\)*\\)[ \t\n]*"
                                     "\\bTABLE\\b\\(.\\|\n\\)*PREFORMAT[ \t\n]+\\(ON\\|OFF\\)\\b") nil t)
      (setq head (match-string 1)
            body (match-string 3)
            preformat (string= (downcase (match-string 6)) "on"))
      (setq head (sqlplus-refine-html head t)
            body (sqlplus-refine-html body t))
      (let ((context-options (list (cons :result-function 'sqlplus-show-html-fun)
                                   (cons :current-command-input-buffer-name (sqlplus-get-context-value context :current-command-input-buffer-name))
                                   (cons :html-command (sqlplus-get-context-value context :html-command))
                                   (cons :htmlized-html-command (sqlplus-get-context-value context :htmlized-html-command))
                                   (cons :head head)
                                   (cons :body body)))
            (prolog-commands (list "set wrap on"
                                   (format "set linesize %S" (if preformat (1- (frame-width)) 4000))
                                   "set pagesize 50000"
                                   "btitle off"
                                   "repfooter off"
                                   "set markup html on")))
        (sqlplus-execute connect-string (sqlplus-get-context-value context :html-command) context-options prolog-commands 'no-echo 'dont-show-output-buffer)))))

(defun sqlplus-htmlize (begin end)
  (let (result)
    (when (featurep 'htmlize)
      (let* ((htmlize-output-type 'font)
             (buffer (funcall (symbol-function 'htmlize-region) begin end)))
        (with-current-buffer buffer
          (goto-char 1)
          (re-search-forward "<pre>[ \t\n]*\\(\\(.\\|\n\\)*?\\)[ \t\n]*</pre>" nil t)
          (setq result (concat "<pre>" (match-string 1) "</pre>")))
        (kill-buffer buffer)))
    (unless result
      (setq result (sqlplus-refine-html (buffer-substring begin end) nil)))
    result))

(defun sqlplus--send (connect-string sql &optional arg no-echo html start end)
  (if html
      (let* ((context-options (list (cons :result-function 'sqlplus-show-markup-fun)
                                    (cons :current-command-input-buffer-name (buffer-name))
                                    (cons :html-command sql)
                                    (cons :htmlized-html-command (if (and (eq sqlplus-html-output-sql 'elegant) (featurep 'htmlize))
                                                                     (sqlplus-htmlize start end)
                                                                   (sqlplus-refine-html sql nil))))))
	(sqlplus-execute connect-string "show markup\n" context-options nil 'no-echo 'dont-show-output-buffer))
    (let* ((no-parse (consp arg))
	   (context-options (list (cons :dont-parse-result (consp arg))
                                  (cons :columns-count (if (integerp arg)
                                                           (if (zerop arg) nil arg)
                                                         (if sqlplus-multi-output-tables-default-flag nil 1)))
                                  (cons :current-command-input-buffer-name (buffer-name))))
           (prolog-commands (list (format "set wrap %s" (if no-parse "on" sqlplus-default-wrap))
                                  (format "set linesize %s" (if (consp arg) (1- (frame-width)) 4000))
                                  (format "set pagesize %S" (if no-parse 50000 sqlplus-pagesize))
                                  (format "btitle %s"
                                          (if no-parse "off" (concat "left '" sqlplus-page-separator "'")))
                                  (format "repfooter %s"
                                          (if no-parse "off" (concat "left '" sqlplus-repfooter "'"))))))
      (sqlplus-execute connect-string sql context-options prolog-commands no-echo))))

(defun sqlplus-explain ()
  (interactive)
  (sqlplus-check-connection)
  (when (buffer-file-name)
    (condition-case err
	(save-buffer)
      (error (message (error-message-string err)))))
  (let* ((region (sqlplus-mark-current)))
    (setq sqlplus-region-beginning-pos (car region)
          sqlplus-region-end-pos (cdr region))
    (if (and sqlplus-region-beginning-pos sqlplus-region-end-pos)
	(let ((sql (sqlplus-parse-region (car region) (cdr region)))
	      (case-fold-search t))
	  (if (string-match "^[\n\t ]*explain[\n\t ]+plan[\t\t ]+for\\>" sql)
	      (sqlplus--send sqlplus-connect-string sql nil nil nil)
	    (setq sql (concat (sqlplus-fontify-string sqlplus-connect-string "explain plan for ") sql))
	    (sqlplus--send sqlplus-connect-string sql nil nil nil)))
      (error "Point doesn't indicate any command to execute"))))
      
(defun sqlplus-send-region (arg start end &optional no-echo html)
  "Send a region to the SQL*Plus process."
  (interactive "P\nr")
  (sqlplus-check-connection)
  (sqlplus--send sqlplus-connect-string (sqlplus-parse-region start end) arg no-echo html start end))

(defun sqlplus-user-command (connect-string sql result-proc)
  (let* ((context-options (list (cons :user-function result-proc)
                                (cons :columns-count 1)))
        (prolog-commands (list (format "set wrap %s" sqlplus-default-wrap)
                               "set linesize 4000"
			       "set timing off"
                               "set pagesize 50000"
                               "btitle off"
                               (format "repfooter %s" (concat "left '" sqlplus-repfooter "'")))))
    (sqlplus-execute connect-string sql context-options prolog-commands 'no-echo 'dont-show-output-buffer)))
  

(defun sqlplus-hidden-select (connect-string sql result-proc)
  (let* ((context-options (list (cons :result-table-function result-proc)
                                (cons :columns-count 1)))
        (prolog-commands (list (format "set wrap %s" sqlplus-default-wrap)
                               "set linesize 4000"
                               "set pagesize 50000"
                               "btitle off"
                               (format "repfooter %s" (concat "left '" sqlplus-repfooter "'")))))
    (sqlplus-execute connect-string sql context-options prolog-commands 'no-echo 'dont-show-output-buffer)))

;; "appi[nfo]" -> '("appinfo" "appi")
(defun sqlplus-full-forms (name)
  (if (string-match "\\`\\([^[]*\\)?\\[\\([^]]+\\)\\]\\([^]]*\\)?\\'" name)
      (list (replace-match "\\1\\2\\3" t nil name)
            (replace-match "\\1\\3" t nil name))
    (list name)))

(defun sqlplus-get-canonical-command-name (name)
  (let ((association (assoc (downcase name) sqlplus-system-variables)))
    (if association (cdr association) name)))
    

(defun sqlplus-execute (connect-string sql context-options prolog-commands &optional no-echo dont-show-output-buffer)
  (sqlplus-verify-buffer connect-string)
  (let* ((process-buffer-name (sqlplus-get-process-buffer-name connect-string))
         (process-buffer (get-buffer process-buffer-name))
         (output-buffer-name (sqlplus-get-output-buffer-name connect-string))
         (echo-prolog (concat "\n" sqlplus-output-separator " " (current-time-string) "\n\n"))
         (process (get-buffer-process process-buffer-name))
         set-prolog-commands commands command-no
	 (history-buffer (sqlplus-get-history-buffer connect-string))
         (defines (sqlplus-define-user-variables sql)))
    (setq prolog-commands (append (sqlplus-initial-strings) prolog-commands))
    (when process-buffer
      (with-current-buffer process-buffer
	(setq command-no sqlplus-command-seq)
	(incf sqlplus-command-seq)
	(setq context-options (append (list (cons :id command-no) (cons :sql sql)) (copy-list context-options)))
	(setq sqlplus-command-contexts (reverse (cons context-options (reverse sqlplus-command-contexts))))))
    ;; move all "set" commands from prolog-commands to set-prolog-commands
    (setq prolog-commands (delq nil (mapcar (lambda (command) (if (string-match "^\\s-*[sS][eE][tT]\\s-+" command)
                                                                  (progn
                                                                    (setq set-prolog-commands
                                                                          (append set-prolog-commands
                                                                                  (list (substring command (length (match-string 0 command))))))
                                                                    nil)
                                                                command))
                                            prolog-commands)))
    ;; remove duplicates commands from prolog-commands (last entries win)
    (let (spc-alist)
      (dolist (command prolog-commands)
        (let* ((name (progn (string-match "^\\S-+" command) (downcase (match-string 0 command))))
               (association (assoc name spc-alist)))
          (if (and association (not (equal name "define")))
              (setcdr association command)
            (setq spc-alist (cons (cons name command) spc-alist)))))
      (setq prolog-commands (mapcar (lambda (pair) (cdr pair)) (reverse spc-alist))))

    (setq prolog-commands (append prolog-commands defines))
    (setq set-prolog-commands (append (list (format "sqlprompt '%s%S%s'" sqlplus-prompt-prefix command-no sqlplus-prompt-suffix)) set-prolog-commands))

    ;; remove duplicates from set-prolog-commands (last entries win)
    (let (spc-alist)
      (dolist (set-command set-prolog-commands)
        (let* ((name (progn (string-match "^\\S-+" set-command) (downcase (sqlplus-get-canonical-command-name (match-string 0 set-command)))))
               (association (assoc name spc-alist)))
          (if association
              (setcdr association set-command)
            (setq spc-alist (cons (cons name set-command) spc-alist)))))
      (setq set-prolog-commands (mapcar (lambda (pair) (cdr pair)) (reverse spc-alist))))
          
    (setq commands (concat (mapconcat 'identity (append
						 (list (concat "set " (mapconcat 'identity set-prolog-commands " ")))
						 prolog-commands
						 (list sql)) "\n")
                           "\n"))
    (when history-buffer
      (with-current-buffer history-buffer
	(goto-char (point-max))
	(insert echo-prolog)
	(insert (concat commands "\n"))))
    (let ((saved-window (cons (selected-window) (window-buffer (selected-window))))
	  (input-buffer (get-buffer (sqlplus-get-input-buffer-name connect-string))))
      (unless no-echo
	(sqlplus-echo-in-buffer output-buffer-name echo-prolog)
	(let ((old-suppress-show-output-buffer sqlplus-suppress-show-output-buffer))
	  (unwind-protect
	      (save-selected-window
		(setq sqlplus-suppress-show-output-buffer dont-show-output-buffer)
		(when (and output-buffer-name
			   (get-buffer output-buffer-name))
		  (with-current-buffer (get-buffer output-buffer-name)
		    (sqlplus-buffer-bottom connect-string)
		    (sqlplus-buffer-mark-current connect-string))))
	    (setq sqlplus-suppress-show-output-buffer old-suppress-show-output-buffer)))
	(sqlplus-echo-in-buffer output-buffer-name (concat sql "\n\n") nil t)
	(save-selected-window
	  (unless dont-show-output-buffer
	    (when (and output-buffer-name
		       (get-buffer output-buffer-name))
	      (with-current-buffer (get-buffer output-buffer-name)
		(sqlplus-buffer-redisplay-current connect-string))))))
      (if (window-live-p (car saved-window))
	  (select-window (car saved-window))
	(if (get-buffer-window (cdr saved-window))
	    (select-window (get-buffer-window (cdr saved-window)))
	  (when (and input-buffer
		     (get-buffer-window input-buffer))
	    (select-window (get-buffer-window input-buffer))))))
    (send-string process commands)))

(defun sqlplus-fontify-string (connect-string string)
  (let* ((input-buffer-name (sqlplus-get-input-buffer-name connect-string))
	 (input-buffer (when input-buffer-name (get-buffer input-buffer-name)))
	 (result string))
    (when (and input-buffer (buffer-live-p input-buffer))
      (with-current-buffer input-buffer
	(save-excursion
	  (goto-char (point-max))
	  (let ((pos (point)))
	    (insert "\n\n")
	    (insert string)
	    (font-lock-fontify-block (+ (count "\n" string) 2))
	    (setq result (buffer-substring (+ pos 2) (point-max)))
	    (delete-region pos (point-max))))))
    result))

(defvar plsql-mark-backward-list nil)

(unless plsql-mode-map
  (setq plsql-mode-map (copy-keymap sql-mode-map))
  (define-key plsql-mode-map "\M-." 'sqlplus-file-get-source)
  (define-key plsql-mode-map [C-down-mouse-1] 'sqlplus-mouse-select-identifier)
  (define-key plsql-mode-map [C-mouse-1] 'sqlplus-file-get-source-mouse)
  (define-key plsql-mode-map "\C-c\C-g" 'plsql-begin)
  (define-key plsql-mode-map "\C-c\C-q" 'plsql-loop)
  (define-key plsql-mode-map "\C-c\C-z" 'plsql-if)
  (define-key plsql-mode-map "\C-c\C-c" 'plsql-compile)
  (define-key plsql-mode-map [tool-bar plsql-prev-mark]
    (list 'menu-item "Previous mark" 'plsql-prev-mark
	  :image plsql-prev-mark-image
	  :enable 'plsql-mark-backward-list)))

(defvar plsql-continue-anyway nil
  "Local in input buffer (plsql-mode).")
(make-variable-buffer-local 'plsql-continue-anyway)

(defun sqlplus-switch-to-buffer (buffer-or-path &optional line-no)
  (if (fboundp 'ide-skel-select-buffer)
      (funcall 'ide-skel-select-buffer buffer-or-path line-no)
    (let ((buffer (or (and (bufferp buffer-or-path) buffer-or-path)
		      (find-file-noselect buffer-or-path))))
      (switch-to-buffer buffer)
      (goto-line line-no))))

(defun plsql-prev-mark ()
  (interactive)
  (let (finish)
    (while (and plsql-mark-backward-list
		(not finish))
      (let* ((marker (pop plsql-mark-backward-list))
	     (buffer (marker-buffer marker))
	     (point (marker-position marker)))
	(set-marker marker nil)
	(when (and buffer
		   (or (not (eq (current-buffer) buffer))
		       (not (eql (point) point))))
	  (sqlplus-switch-to-buffer buffer)
	  (goto-char point)
	  (setq finish t))))
    ;; (message "BACK: %S -- FORWARD: %S" plsql-mark-backward-list plsql-mark-forward-list)
    (force-mode-line-update)
    (sit-for 0)))

(defun sqlplus-mouse-select-identifier (event)
  (interactive "@e")
  (with-selected-window (posn-window (event-start event))
    (save-excursion
      (let* ((point (posn-point (event-start event)))
	     (identifier (progn (goto-char point) (thing-at-point 'symbol)))
	     (ident-regexp (when identifier (regexp-quote identifier))))
	(push (point-marker) plsql-mark-backward-list)
	(when ident-regexp
	  (save-excursion
	    (while (not (looking-at ident-regexp))
	      (backward-char))
	    (sqlplus-mouse-set-selection (current-buffer) (point) (+ (point) (length identifier)) 'highlight)))))))

(defun sqlplus-file-get-source-mouse (event)
  (interactive "@e")
  (let (ident)
    (with-selected-window (posn-window (event-start event))
      (save-excursion
	(goto-char (posn-point (event-start event)))
	(setq ident (thing-at-point 'symbol))))
    (sqlplus-file-get-source sqlplus-connect-string ident nil)
    (sit-for 0)))

(defun plsql-compile (&optional arg)
  "Save buffer and send its content to SQL*Plus.
You must enter connect-string if buffer is disconnected; with
argument you can change connect-string even for connected
buffer."
  (interactive "P")
  (let (aborted
        exists-show-error-command
        (case-fold-search t))
    (save-window-excursion
      (save-excursion
        ;; ask for "/" and "show err" if absent
        (let ((old-point (point))
              show-err-needed
              exists-run-command best-point finish)
          (goto-char (point-min))
          (setq show-err-needed (let ((case-fold-search t))
                                  (re-search-forward "create\\([ \t\n]+or[ \t\n]+replace\\)?[ \t\n]+\\(package\\|procedure\\|function\\|trigger\\|view\\|type\\)" nil t)))
          (goto-char (point-max))
          (forward-comment (- (buffer-size)))
          (re-search-backward "^\\s-*show\\s-+err" nil t)
          (forward-comment (- (buffer-size)))
          (condition-case nil (forward-char) (error nil))
          (setq best-point (point))
          (goto-char (point-min))
          (setq exists-run-command (re-search-forward "^\\s-*/[^*]" nil t))
          (goto-char (point-min))
          (setq exists-show-error-command (or (not show-err-needed) (re-search-forward "^\\s-*show\\s-+err" nil t)))
          (while (and (not plsql-continue-anyway) (or (not exists-run-command) (not exists-show-error-command)) (not finish))
            (goto-char best-point)
            (let ((c (read-char 
                      (format "Cannot find %s.  (I)nsert it at point, (A)bort, (C)ontinue anyway"
                              (concat (unless exists-run-command "\"/\"")
                                      (unless (or exists-run-command exists-show-error-command) " and ")
                                      (unless exists-show-error-command "\"show err\""))))))
              (cond ((memq c '(?i ?I))
                     (unless exists-run-command (insert "/\n"))
                     (unless exists-show-error-command (insert "show err\n"))
                     (setq finish t))
                    ((memq c '(?a ?A))
                     (setq aborted t
                           finish t))
                    ((memq c '(?c ?C))
                     (setq plsql-continue-anyway t)
                     (setq finish t))))))))
    (unless aborted
      (save-buffer)
      (let* ((buffer (current-buffer))
             (input-buffer-name (buffer-name))
             (file-path (sqlplus-file-truename (buffer-file-name)))
             (compilation-buffer (get-buffer sqlplus-plsql-compilation-results-buffer-name))
             (context-options (list (cons :last-compiled-file-path file-path)
                                    (cons :current-command-input-buffer-name input-buffer-name)
                                    (cons :compilation-expected exists-show-error-command)))
             (prolog-commands (list (format "set wrap %s" sqlplus-default-wrap)
                                    "set linesize 4000"
                                    (format "set pagesize %S" sqlplus-pagesize)
                                    (format "btitle %s" (concat "left '" sqlplus-page-separator "'"))
                                    (format "repfooter %s" (concat "left '" sqlplus-repfooter "'")))))
        (when (or (not sqlplus-connect-string)
                  arg)
          (setq sqlplus-connect-string (car (sqlplus-read-connect-string nil (caar (sqlplus-divide-connect-strings))))))
        (sqlplus sqlplus-connect-string nil (when plsql-auto-parse-errors-flag 'dont-show-output-buffer))
        (set-buffer buffer)
        (force-mode-line-update)
	(when font-lock-mode (font-lock-mode 1))
        (when compilation-buffer
          (with-current-buffer compilation-buffer
	    (let ((inhibit-read-only t))
	      (erase-buffer))))
        (setq prolog-commands (append prolog-commands (sqlplus-define-user-variables (buffer-string))))
        (sqlplus-execute sqlplus-connect-string (concat "@" file-path) context-options prolog-commands nil exists-show-error-command)))))

(defun plsql-parse-errors (last-compiled-file-path)
  (let ((file-name (file-name-nondirectory last-compiled-file-path))
        error-list)
    (put-text-property 0 (length file-name) 'face 'font-lock-warning-face file-name)
    (save-excursion 
      (when (re-search-forward "^LINE/COL\\>" nil t)
        (beginning-of-line 3)
        (while (re-search-forward "^\\([0-9]+\\)/\\([0-9]+\\)\\s-*\\(\\(.\\|\n\\)*?\\)[\r\t ]*\n\\([\r\t ]*\\(\n\\|\\'\\)\\|[0-9]+\\)" nil t)
          (let ((line-no (match-string 1))
                (column-no (match-string 2))
                (errmsg (match-string 3))
                label)
            (goto-char (match-beginning 5))
            (while (string-match "\\s-\\s-+" errmsg)
              (setq errmsg (replace-match " " nil t errmsg)))
            (put-text-property 0 (length line-no) 'face 'font-lock-variable-name-face line-no)
            (put-text-property 0 (length column-no) 'face 'font-lock-variable-name-face column-no)
            (setq label (concat file-name ":" line-no ":" column-no ": " errmsg))
            (put-text-property 0 (length label) 'mouse-face 'highlight label)
            (push label error-list)))))
    (save-excursion
      (while (re-search-forward "\\s-\\([0-9]+\\):\n\\(ORA-[0-9]+[^\n]*\\)\n" nil t)
        (let ((line-no (match-string 1))
              (errmsg (match-string 2))
              label)
          (put-text-property 0 (length line-no) 'face 'font-lock-variable-name-face line-no)
          (setq label (concat file-name ":" line-no ": " errmsg))
          (put-text-property 0 (length label) 'mouse-face 'highlight label)
          (push label error-list))))
    (save-excursion
      (while (re-search-forward "\\(\\(SP2\\|CPY\\)-[0-9]+:[^\n]*\\)\n" nil t)
        (let ((errmsg (match-string 1))
              label)
          (setq label (concat file-name ":" errmsg))
          (put-text-property 0 (length label) 'mouse-face 'highlight label)
          (push label error-list))))
    error-list))

(defun plsql-display-errors (dir error-list)
  (let ((buffer (get-buffer-create sqlplus-plsql-compilation-results-buffer-name)))
    (save-selected-window
      (save-excursion
        (set-buffer buffer)
	(let ((inhibit-read-only t))
	  (erase-buffer)
	  (setq default-directory dir)
	  (insert (format "cd %s\n" default-directory))
	  (insert (format "Compilation results\n"))
	  (compilation-minor-mode 1)
	  (dolist (msg (reverse error-list))
	    (insert msg)
	    (insert "\n"))
	  (insert (format "\n(%s errors)\n" (length error-list))))
        (when (and error-list (fboundp 'compile-reinitialize-errors) (funcall (symbol-function 'compile-reinitialize-errors) t)))
        (switch-to-buffer-other-window buffer)
        (goto-line 1)
        (goto-line 3)))))


(defun sqlplus-file-truename (file-name)
  (if file-name
      (file-truename file-name)
    file-name))

(defun sqlplus--hidden-buffer-name-p (buffer-name)
  (equal (elt buffer-name 0) 32))

(defun sqlplus-get-workbench-window ()
  "Return upper left window"
  (if (fboundp 'ide-get-workbench-window)
      (funcall (symbol-function 'ide-get-workbench-window))
    (let (best-window)
      (dolist (win (copy-list (window-list nil 1)))
	(when (not (sqlplus--hidden-buffer-name-p (buffer-name (window-buffer win))))
	  (if (null best-window)
	      (setq best-window win)
	    (let* ((best-window-coords (window-edges best-window))
		   (win-coords (window-edges win)))
	      (when (or (< (cadr win-coords) (cadr best-window-coords))
			(and (= (cadr win-coords) (cadr best-window-coords))
			     (< (car win-coords) (car best-window-coords))))
		(setq best-window win))))))
      ;; (message "BEST-WINDOW: %S" best-window)
      best-window)))

(defun sqlplus-get-side-window ()
  "Return bottom helper window, or nil if not found"
  (if (fboundp 'ide-get-side-window)
      (funcall (symbol-function 'ide-get-side-window))
    (let* ((workbench-window (sqlplus-get-workbench-window))
	   best-window)
      (dolist (win (copy-list (window-list nil 1)))
	(when (and (not (sqlplus--hidden-buffer-name-p (buffer-name (window-buffer win))))
		   (not (eq win workbench-window)))
	  (if (null best-window)
	      (setq best-window win)
	    (when (> (cadr (window-edges win)) (cadr (window-edges best-window)))
	      (setq best-window win)))))
      best-window)))

(defvar sqlplus--idle-tasks nil)

(defun sqlplus--enqueue-task (fun &rest params)
  (setq sqlplus--idle-tasks (reverse (cons (cons fun params) (reverse sqlplus--idle-tasks)))))

(defun sqlplus--execute-tasks ()
  (dolist (task sqlplus--idle-tasks)
    (let ((fun (car task))
          (params (cdr task)))
      (condition-case var
          (apply fun params)
        (error (message (error-message-string var))))))
  (setq sqlplus--idle-tasks nil))

(add-hook 'post-command-hook 'sqlplus--execute-tasks)

(defvar sqlplus-mouse-selection nil)

(defun sqlplus-mouse-set-selection (buffer begin end mouse-face)
  (interactive "@e")
  (let ((old-buffer-modified-p (buffer-modified-p)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
	(unwind-protect
	    (put-text-property begin end 'mouse-face mouse-face)
	  (set-buffer-modified-p old-buffer-modified-p)
	  (setq sqlplus-mouse-selection (when mouse-face (list buffer begin end))))))))

(defun sqlplus-clear-mouse-selection ()
  (when (and sqlplus-mouse-selection
	     (eq (event-basic-type last-input-event) 'mouse-1)
	     (not (memq 'down (event-modifiers last-input-event))))
    (sqlplus-mouse-set-selection (car sqlplus-mouse-selection) (cadr sqlplus-mouse-selection) (caddr sqlplus-mouse-selection) nil)))
  
(add-hook 'plsql-mode-hook
          (lambda ()
            (modify-syntax-entry ?. "." sql-mode-syntax-table)
            (setq sqlplus-font-lock-keywords-1 (sqlplus-set-font-lock-emacs-structures-for-level 1 major-mode))
            (setq sqlplus-font-lock-keywords-2 (sqlplus-set-font-lock-emacs-structures-for-level 2 major-mode))
            (setq sqlplus-font-lock-keywords-3 (sqlplus-set-font-lock-emacs-structures-for-level 3 major-mode))
            (setq font-lock-defaults '((sqlplus-font-lock-keywords-1 sqlplus-font-lock-keywords-2 sqlplus-font-lock-keywords-3)
                                       nil t ((?_ . "w") (?$ . "w") (?# . "w") (?& . "w"))))
	    (orcl-mode 1)
            (use-local-map plsql-mode-map) ; std
	    (add-hook 'post-command-hook 'sqlplus-clear-mouse-selection nil t)))

(setq recentf-exclude (cons (concat "^" (regexp-quote (file-name-as-directory temporary-file-directory)))
			    (when (boundp 'recentf-exclude)
			      recentf-exclude)))

(defun get-all-dirs (root-dir)
  (let ((list-to-see (list root-dir))
        result-list)
    (while list-to-see
      (let* ((dir (pop list-to-see))
             (children (directory-files dir t)))
        (push dir result-list)
        (dolist (child children)
          (when (and (not (string-match "^[.]+"(file-name-nondirectory child)))
                     (file-directory-p child))
            (push child list-to-see)))))
    result-list))

(defun sqlplus-command-line ()
  (interactive)
  (if (comint-check-proc "*SQL*")
      (pop-to-buffer "*SQL*")
    (let* ((pair (sqlplus-read-connect-string nil (when sqlplus-connect-string (car (refine-connect-string sqlplus-connect-string)))))
	   (qualified-cs (car pair))
	   (refined-cs (cadr pair))
	   (password (cdr (refine-connect-string qualified-cs))))
      (if (string-match "^\\([^@]*\\)@\\(.*\\)$" refined-cs)
	  (let ((old-sql-get-login-fun (symbol-function 'sql-get-login)))
	    (setq sql-user (match-string 1 refined-cs)
		  sql-password password
		  sql-database (match-string 2 refined-cs))
	    (unwind-protect
		(progn
		  (fset 'sql-get-login (lambda (&rest whatever) nil))
		  (sql-oracle))
	      (fset 'sql-get-login old-sql-get-login-fun)))
	(error "Connect string must be in form login@sid")))))

(defun sqlplus-find-tnsnames ()
  (interactive)
  (let* ((ora-home-dir (or (getenv "ORACLE_HOME") (error "Environment variable ORACLE_HOME not set")))
	 found
	 (list-to-see (list ora-home-dir)))
    (while (and (not found) list-to-see)
      (let* ((dir (pop list-to-see))
	     (children (condition-case nil (directory-files dir t) (error nil))))
	(dolist (child children)
	  (unless found
	    (if (string-match "admin.tnsnames\.ora$" child)
		(progn
		  (setq found t)
		  (find-file child))
	      (if (and (not (string-match "^[.]+" (file-name-nondirectory child)))
		       (file-directory-p child))
		  (push child list-to-see)))))))
    (unless found
      (message "File tnsnames.ora not found"))))

(defun sqlplus-remove-help-echo (list)
  "Remove all HELP-ECHO properties from mode-line format value"
  (when (listp list)
      (if (eq (car list) :propertize)
	  (while list
	    (when (eq (cadr list) 'help-echo)
	      (setcdr list (cdddr list)))
	    (setq list (cdr list)))
	(dolist (elem list) (sqlplus-remove-help-echo elem)))))

(when (>= emacs-major-version 22)
  (sqlplus-remove-help-echo mode-line-modes))

(defun sqlplus-get-project-root-dir (path)
  (let ((path (file-truename (substitute-in-file-name path)))
	dir)
    (if (file-directory-p path)
	(progn
	  (setq path (file-name-as-directory path))
	  (setq dir path))
      (setq dir (file-name-as-directory (file-name-directory path))))
    (let ((last-project-dir dir)
	  (dir-list (split-string dir "/"))
	  is-project)
      (while (directory-files dir t (concat "^" "\\(\\.svn\\|CVS\\)$") t)
	(setq is-project t
	      last-project-dir (file-name-as-directory dir)
	      dir (file-name-as-directory (file-name-directory (directory-file-name dir)))))
      (when is-project
	(let ((list (nthcdr (1- (length (split-string last-project-dir "/"))) dir-list)))
	  (cond ((equal (car list) "trunk")
		 (setq last-project-dir (concat last-project-dir "trunk/")))
		((member (car list) '("branches" "tags"))
		 (setq last-project-dir (concat last-project-dir (car list) "/" (when (cdr list) (concat (cadr list) "/")))))
		(t)))
	(setq dir last-project-dir)))
    dir))

(defvar sqlplus-search-buffer-name "*search*")

(defvar sqlplus-object-types-regexps 
  '(
    ("TABLE" . "\\bcreate\\s+table\\s+[^(]*?\\b#\\b")
    ("VIEW"  . "\\bview\\s+.*?\\b#\\b")
    ("INDEX" . "\\b(constraint|index)\\s+.*?\\b#\\b")
    ("TRIGGER" . "\\btrigger\\s+.*?\\b#\\b")
    ("SEQUENCE" . "\\bsequence\\s+.*?\\b#\\b")
    ("SYNONYM"  . "\\bsynonym\\s+.*?\\b#\\b")
    ("SCHEMA"   . "\\bcreate\\b.*?\\buser\\b.*?\\b#\\b")
    ("PROCEDURE" . "\\b(procedure|function)\\b[^(]*?\\b#\\b")
    ("PACKAGE"   . "\\bpackage\\s+.*?\\b#\\b")))

(defvar sqlplus-root-dir-history nil)

(defvar sqlplus-compare-report-buffer-name "*Comparation Report*")

(defun sqlplus-compare-schema-to-filesystem (&optional arg)
  (interactive "P")
  (let* ((connect-string sqlplus-connect-string)
	 (objects-alist (sqlplus-get-objects-alist sqlplus-connect-string))
	 (report-buffer (get-buffer-create sqlplus-compare-report-buffer-name))
	 (types-length (- (length objects-alist) 2))
	 (root-dir (or (sqlplus-get-root-dir connect-string)
		       (sqlplus-set-project-for-connect-string connect-string)
		       (error "Root dir not set")))
	 (counter 0))
    (unless objects-alist
      (error "Not ready yet - try again later"))
    (save-excursion
      (switch-to-buffer-other-window report-buffer))
    (with-current-buffer report-buffer
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert (format "%s %s vs. %s\n\n" (current-time-string) (car (refine-connect-string connect-string)) root-dir))
	(sit-for 0)))
    (dolist (pair objects-alist)
      (let ((type (upcase (format "%s" (car pair))))
	    (names (cdr pair)))
	(unless (member type '("SCHEMA" "COLUMN"))
	  (incf counter)
	  (message (format "%s (%d/%d)..." type counter types-length))
	  (dolist (name-pair names)
	    (let* ((name (car name-pair))
		   (grep-result (sqlplus-file-get-source sqlplus-connect-string name type 'batch-mode)))
	      (with-current-buffer report-buffer
		(let ((inhibit-read-only t))
		  (goto-char (point-max))
		  (cond ((eql (length grep-result) 0)
			 (insert (format "%s %s: not found\n" type name))
			 (sit-for 0))
			((and arg
			      (> (length grep-result) 1))
			 (insert (format "%s %s:\n" type name))
			 (dolist (list grep-result)
			   (insert (format "  %s:%d %s\n" (car list) (cadr list) (caddr list))))
			 (sit-for 0))
			(t)))))))))
    (message "Done.")
    (with-current-buffer report-buffer
      (goto-char (point-min)))))

(defun sqlplus-proj-find-files (dir file-predicate &optional dir-predicate)
  (setq dir (file-name-as-directory (file-truename (substitute-in-file-name dir))))
  (let (result-list)
    (mapcar (lambda (path)
	      (if (file-directory-p path)
		  (when (and (file-accessible-directory-p path)
			     (or (null dir-predicate)
				 (funcall dir-predicate path)))
		    (setq result-list (append result-list (sqlplus-proj-find-files path file-predicate dir-predicate))))
		(when (or (null file-predicate)
			  (funcall file-predicate path))
		  (push path result-list))))
	    (delete (concat (file-name-as-directory dir) ".") 
		    (delete (concat (file-name-as-directory dir) "..")
			    (directory-files dir t nil t))))
    result-list))

(defvar sqlplus-proj-ignored-extensions '("semantic.cache"))

(defun sqlplus-mode-file-regexp-list (mode-symbol-list)
  (delq nil (mapcar (lambda (element)
		      (let ((fun-name (if (listp (cdr element)) (cadr element) (cdr element))))
			(when (memq fun-name mode-symbol-list) (cons (car element) fun-name))))
		    auto-mode-alist)))

(defun sqlplus-find-project-files (root-dir mode-symbol-list predicate)
  (let ((obj-file-regexp-list (delq nil (mapcar (lambda (element)
						  (let ((len (length element)))
						    (unless (and (> len 0)
								 (equal (elt element (1- len)) ?/))
						      (concat (regexp-quote element) "$"))))
						(append sqlplus-proj-ignored-extensions completion-ignored-extensions))))
	(mode-file-regexp-list (sqlplus-mode-file-regexp-list mode-symbol-list))) ; (file-path-regexp . major-mode-function-symbol)
    (when (and mode-symbol-list
	       (not mode-file-regexp-list))
      (error (format "No rules for %s major modes in auto-mode-alist." (mapconcat 'identity mode-symbol-list ", "))))
    (sqlplus-proj-find-files root-dir
			      (lambda (file-name)
				(and (not (string-match "#" file-name))
				     (not (string-match "semantic.cache" file-name))
				     (or (and (not mode-symbol-list)
					      (not (some (lambda (regexp)
								  (string-match regexp file-name))
								obj-file-regexp-list)))
					 (and mode-symbol-list
					      (some (lambda (element)
						      (let ((freg (if (string-match "[$]" (car element))
								      (car element)
								    (concat (car element) "$"))))
							(when (string-match freg file-name)
							  (cdr element))))
						    mode-file-regexp-list)))
				     (or (not predicate)
					 (funcall predicate file-name))))
			      (lambda (dir-path)
				(not (string-match "/\\(\\.svn\\|CVS\\)$" dir-path))))))


(defun sqlplus-file-get-source (connect-string object-name object-type &optional batch-mode)
  (interactive
   (progn
     (push (point-marker) plsql-mark-backward-list)
     (list sqlplus-connect-string (thing-at-point 'symbol) nil)))
  (unless object-name
    (error "Nothing to search"))
  (let* ((root-dir (or (and (not object-type)
			    (eq major-mode 'plsql-mode)
			    (buffer-file-name)
			    (sqlplus-get-project-root-dir (buffer-file-name)))
		       (sqlplus-get-root-dir connect-string)
		       (sqlplus-set-project-for-connect-string connect-string)
		       (error "Root dir not set")))
	 (mode-symbol-list '(plsql-mode sql-mode))
	 (files-to-grep (sqlplus-find-project-files root-dir mode-symbol-list nil))
	 (temp-file-path (concat (file-name-as-directory temporary-file-directory) (make-temp-name "ide-")))
	 (search-buffer (get-buffer sqlplus-search-buffer-name))
	 (regexp (let ((index 0)
		       (len (length object-name))
		       result)
		   (setq result
			 (if object-type
			     (let ((type (cond ((equal object-type "FUNCTION") "PROCEDURE")
					       ((equal object-type "PACKAGE BODY") "PACKAGE")
					       (t object-type))))
			       (cdr (assoc type sqlplus-object-types-regexps)))
			   (mapconcat 'cdr sqlplus-object-types-regexps "|")))
		   (unless result
		     (error "Not implemented"))
		   (while (and (< index (length result))
			       (string-match "#" result index))
		     (setq index (+ (match-beginning 0) len))
		     (setq result (replace-match object-name t t result)))
		   (setq index 0)
		   (while (and (< index (length result))
			       (string-match "[$]\\(\\\\b\\)?" result index))
		     (setq index (+ (match-end 0) 1))
		     (setq result (replace-match "\\$" t t result)))
		   result))
	 grep-command
	 grep-result)
    (when search-buffer
      (with-current-buffer search-buffer
	(let ((inhibit-read-only t))
	  (erase-buffer))))
    ;; (message "Object type: %S, object name: %S, regexp: %S" object-type object-name regexp)
    (with-temp-file temp-file-path
      (dolist (path files-to-grep)
	(insert (concat "'" path "'\n"))))
    (let* ((grep-command (format "cat %s | xargs grep -nHiE -e '%s'" temp-file-path regexp))
	   (raw-grep-result (split-string (shell-command-to-string grep-command) "\n" t))
	   (grep-result (delq nil (mapcar (lambda (line)
					    (string-match "^\\(.*?\\):\\([0-9]+\\):\\(.*\\)$" line)
					    (let* ((path (match-string 1 line))
						   (line-no (string-to-number (match-string 2 line)))
						   (text (match-string 3 line))
						   (text2 text)
						   (syn-table (copy-syntax-table))
						   (case-fold-search t))
					      (modify-syntax-entry ?$ "w" syn-table)
					      (modify-syntax-entry ?# "w" syn-table)
					      (modify-syntax-entry ?_ "w" syn-table)
					      (with-syntax-table syn-table
						(when (and (or (and (not object-type)
								    (> (length raw-grep-result) 1))
							       (equal object-type "SYNONYM"))
							   (string-match "\\<\\(for\\|from\\|on\\|as\\)\\>" text2))
						  (setq text2 (substring text2 0 (match-beginning 0))))
						;; (message "GREP-RESULT: %s" text2)
						(unless (or (not (string-match (concat "\\<" (regexp-quote object-name) "\\>") text2))
							    (string-match (concat "\\(--\\|\\<pro\\>\\|\\<prompt\\>\\|\\<drop\\>\\|\\<grant\\>\\).*\\<"
										  (regexp-quote object-name) "\\>") text2)
							    (and (or (and (not object-type)
									  (> (length raw-grep-result) 1))
								     (equal object-type "TRIGGER"))
								 (string-match "\\<\\(alter\\|disable\\|enable\\)\\>" text2))
							    (and (or (and (not object-type)
									  (string-match "\\<package\\>" text2)
									  current-prefix-arg)
								     (equal object-type "PACKAGE"))
								 (string-match "\\<body\\>" text2))
							    (and (or (and (not object-type)
									  (string-match "\\<package\\>" text2)
									  (not current-prefix-arg))
								     (equal object-type "PACKAGE BODY"))
								 (not (string-match "\\<body\\>" text2)))
							    (and (not object-type)
								 (not current-prefix-arg)
								 (string-match "[.]pks$" path)))
						  (list path line-no text)))))
					  raw-grep-result))))
      (if batch-mode
	  grep-result
	(cond ((not grep-result)
	       (error "Not found"))
	      ((eql (length grep-result) 1)
	       (sqlplus-switch-to-buffer (caar grep-result) (cadar grep-result))
	       (when connect-string
		 (setq sqlplus-connect-string connect-string)))
	      (t
	       (let ((search-buffer (get-buffer-create sqlplus-search-buffer-name)))
		 (with-current-buffer search-buffer
		   (setq buffer-read-only t)
		   (let ((inhibit-read-only t))
		     (setq default-directory root-dir)
		     (erase-buffer)
		     (insert "Root dir: ")
		     (sqlplus-proj-insert-with-face root-dir 'font-lock-keyword-face)
		     (insert "; Range: ")
		     (sqlplus-proj-insert-with-face (mapconcat (lambda (sym) (sqlplus-mode-name-stringify sym)) mode-symbol-list ", ")
						     'font-lock-keyword-face)
		     (insert "; Object type: ")
		     (sqlplus-proj-insert-with-face (or object-type "unspecified") 'font-lock-keyword-face)
		     (insert "; Object name: ")
		     (sqlplus-proj-insert-with-face object-name 'font-lock-keyword-face)
		     (insert "\n\n")
		     (compilation-minor-mode 1)
		     (dolist (result grep-result)
		       (let ((relative-path (concat "./" (file-relative-name (car result) root-dir)))
			     (line-no (cadr result))
			     (text (caddr result)))
			 (put-text-property 0 (length relative-path) 'mouse-face 'highlight relative-path)
			 (insert relative-path)
			 (insert (format ":%S:1 %s\n" line-no text))))
		     (insert (format "\n%d matches found." (length grep-result)))
		     (goto-char (point-min))
		     (when (and grep-result (fboundp 'compile-reinitialize-errors) (funcall (symbol-function 'compile-reinitialize-errors) t)))
		     (switch-to-buffer-other-window search-buffer)
		     (goto-line 1)
		     (goto-line 3))))))))))

(defun sqlplus-mode-name-stringify (mode-name)
  (let ((name (format "%s" mode-name)))
    (replace-regexp-in-string "-" " "
			      (capitalize
			       (if (string-match "^\\(.*\\)-mode" name)
				   (match-string 1 name)
				 name)))))

(defun sqlplus-proj-insert-with-face (string face)
  (let ((point (point)))
    (insert string)
    (let ((overlay (make-overlay point (point))))
      (overlay-put overlay 'face face))))

(defun sqlplus-set-project-for-connect-string (connect-string)
  (if (featurep 'ide-skel)
      ;; Prepare sqlplus-root-dir-history (file-name-history) for user convenience
      ;; 0. previous project root
      ;; 1. current editor file project root
      ;; 2. previous choices
      ;; 3. new project roots
      (let* ((prev-proj-root-dir (sqlplus-get-root-dir connect-string))
	     (last-sel-window (funcall 'ide-skel-get-last-selected-window))
	     (editor-file-proj-root-dir (when last-sel-window
					  (let* ((buffer (window-buffer last-sel-window))
						 (path (and buffer (buffer-file-name buffer)))
						 (project (and path (car (funcall 'ide-skel-proj-get-project-create path)))))
					    (when (funcall 'ide-skel-project-p project)
					      (funcall 'ide-skel-project-root-path project))))))
	(setq sqlplus-root-dir-history
	      (delete-dups
	       (delq nil
		     (mapcar (lambda (dir)
			       (when dir
				 (directory-file-name (file-truename (substitute-in-file-name dir)))))
			     (append
			      (list editor-file-proj-root-dir prev-proj-root-dir)
			      sqlplus-root-dir-history
			      (mapcar (lambda (project) (funcall 'ide-skel-project-root-path project))
				      (symbol-value 'ide-skel-projects)))))))
	(let* ((file-name-history (cdr sqlplus-root-dir-history))
	       (use-file-dialog nil)
	       (dir (directory-file-name (file-truename (substitute-in-file-name
							 (read-directory-name (format "Root dir for %s: " (car (refine-connect-string connect-string)))
									      (car sqlplus-root-dir-history)
									      (car sqlplus-root-dir-history)
									      t
									      nil))))))
	  (funcall 'ide-skel-proj-get-project-create dir)
	  (sqlplus-set-root-dir dir connect-string)
	  (message (format "Root dir for %s set to %s" (car (refine-connect-string connect-string)) dir))
	  dir))
    (let* ((use-file-dialog nil)
	   (dir (directory-file-name (file-truename (substitute-in-file-name
						    (read-directory-name (format "Root dir for %s: " (car (refine-connect-string connect-string)))
									 nil nil t nil))))))
      (sqlplus-set-root-dir dir connect-string)
      (message (format "Root dir for %s set to %s" (car (refine-connect-string connect-string)) dir))
      dir)))

;;; Plugin for ide-skel.el

(defstruct sqlplus-tab
  id
  name              ; tab name
  symbol            ; view/sequence/schema/trigger/index/table/package/synonym/procedure
  help-string
  (display-start 1) ; display-start in side view window
  (data nil)        ; '(("name" . status)...), where status t means 'invalid'
  draw-function     ; parameters: sqlplus-tab
  click-function    ; parameters: event "@e"
  (errors-count 0)
  (refresh-in-progress t)
  update-select)

(defvar sqlplus-side-view-connect-string nil)
(make-variable-buffer-local 'sqlplus-side-view-connect-string)

(defvar sqlplus-side-view-active-tab nil)
(make-variable-buffer-local 'sqlplus-side-view-active-tab)

(defvar sqlplus-side-view-tabset nil)
(make-variable-buffer-local 'sqlplus-side-view-tabset)

(defface sqlplus-side-view-face '((t :inherit variable-pitch :height 0.8))
  "Default face used in right view"
  :group 'sqlplus)

(defvar sqlplus-side-view-keymap nil)
(unless sqlplus-side-view-keymap
  (setq sqlplus-side-view-keymap (make-sparse-keymap))
  (define-key sqlplus-side-view-keymap [mode-line down-mouse-1] 'ignore)
  (define-key sqlplus-side-view-keymap [mode-line mouse-1] 'sqlplus-side-view-tab-click))

(defun sqlplus-side-view-tab-click (event)
  (interactive "@e")
  (with-selected-window (posn-window (event-start event))
    (let* ((previous-sel-tab-info (nth sqlplus-side-view-active-tab sqlplus-side-view-tabset))
	   (target (posn-string (event-start event)))
	   (tab-info (get-text-property (cdr target) 'tab-info (car target))))
      (setf (sqlplus-tab-display-start previous-sel-tab-info) (line-number-at-pos (window-start)))
      (setq sqlplus-side-view-active-tab (sqlplus-tab-id tab-info))
      (sqlplus-side-view-redraw (current-buffer) t)
      (sqlplus-side-view-buffer-mode-line))))
  
(defun sqlplus-side-view-buffer-mode-line ()
  (let* ((separator (propertize " "
				'face 'header-line
				'display '(space :width 0.2)
				'pointer 'arrow)))
    (setq mode-line-format
	  (concat separator
		  (mapconcat (lambda (tab)
			       (let ((face (if (eq (sqlplus-tab-id tab) sqlplus-side-view-active-tab) 
					       'tabbar-selected 
					     'tabbar-unselected))
				     (help-echo (concat (sqlplus-tab-help-string tab)
							(if (> (sqlplus-tab-errors-count tab) 0)
							    (format "\n(%s error%s)" (sqlplus-tab-errors-count tab)
								    (if (> (sqlplus-tab-errors-count tab) 1) "s" ""))
							  ""))))
				 (propertize (format " %s " (sqlplus-tab-name tab))
					     'local-map sqlplus-side-view-keymap
					     'tab-info tab
					     'help-echo help-echo
					     'mouse-face 'tabbar-highlight
					     'face (if (> (sqlplus-tab-errors-count tab) 0)
						       (list '(foreground-color . "red") face)
						     face)
					     'pointer 'hand)))
			     sqlplus-side-view-tabset
			     separator)
		  separator))))

(defun sqlplus-side-view-click-on-default-handler (event)
  (interactive "@e")
  (with-selected-window (posn-window (event-start event))
    (let* ((posn-point (posn-point (event-start event)))
	   (object-name (get-text-property posn-point 'object-name))
	   (object-type (get-text-property posn-point 'object-type))
	   (type (car event)))
      (when (eq type 'mouse-3)
	(setq type (car (x-popup-menu t (append (list 'keymap object-name)
						    (list '(sqlplus-refresh-side-view-buffer "Refresh" t))
						    (list '(mouse-1 "Get source from Oracle" t))
						    (list '(M-mouse-1 "Search source in filesystem" t))
						    (list (list 'C-M-mouse-1 (concat "Set root dir for " (car (refine-connect-string sqlplus-side-view-connect-string))) t))
						    )))))
      (cond ((eq type 'mouse-1)
	     (sqlplus-get-source sqlplus-side-view-connect-string object-name object-type))
	    ((eq type 'M-mouse-1)
	     (sqlplus-file-get-source sqlplus-side-view-connect-string object-name object-type))
	    ((eq type 'C-M-mouse-1)
	     (sqlplus-set-project-for-connect-string sqlplus-side-view-connect-string))
	    ((eq type nil))
	    (t
	     (condition-case err
		 (funcall type)
	       (error nil)))))))

(defun sqlplus-side-view-click-on-index-handler (event)
  (interactive "@e")
  (with-selected-window (posn-window (event-start event))
    (let* ((posn-point (posn-point (event-start event)))
	   (object-name (get-text-property posn-point 'object-name))
	   (object-type (get-text-property posn-point 'object-type))
	   (type (car event)))
      (when (eq type 'mouse-3)
	(setq type (car (x-popup-menu t (append (list 'keymap object-name)
						    (list '(sqlplus-refresh-side-view-buffer "Refresh" t))
						    (list '(mouse-1 "Get source from Oracle" t))
						    (list '(M-mouse-1 "Search source in filesystem" t))
						    (list (list 'C-M-mouse-1 (concat "Set root dir for " (car (refine-connect-string sqlplus-side-view-connect-string))) t))
						    )))))
      (cond ((eq type 'mouse-1)
	     (sqlplus-get-source sqlplus-side-view-connect-string object-name object-type))
	    ((eq type 'M-mouse-1)
	     (sqlplus-file-get-source sqlplus-side-view-connect-string object-name object-type))
	    ((eq type 'C-M-mouse-1)
	     (sqlplus-set-project-for-connect-string sqlplus-side-view-connect-string))
	    ((eq type nil))
	    (t
	     (condition-case err
		 (funcall type)
	       (error nil)))))))

(defun sqlplus-side-view-click-on-schema-handler (event)
  (interactive "@e")
  (with-selected-window (posn-window (event-start event))
    (let* ((posn-point (posn-point (event-start event)))
	   (object-name (get-text-property posn-point 'object-name))
	   (object-type (get-text-property posn-point 'object-type))
	   (last-selected-win (funcall 'ide-skel-get-last-selected-window))
	   (type (car event)))
      (when (eq type 'mouse-3)
	(setq type (car (x-popup-menu t (append (list 'keymap object-name)
						    (list '(sqlplus-refresh-side-view-buffer "Refresh" t))
						    (list '(mouse-1 "Connect to schema" t))
						    (list '(M-mouse-1 "Search source in filesystem" t))
						    (list (list 'C-M-mouse-1 (concat "Set root dir for " (car (refine-connect-string sqlplus-side-view-connect-string))) t))
						    )))))
      (cond ((eq type 'mouse-1)
	     (when (string-match "@.*$" sqlplus-side-view-connect-string)
	       (let* ((cs (downcase (concat object-name (match-string 0 sqlplus-side-view-connect-string))))
		      (pair (sqlplus-read-connect-string cs cs)))
		 (select-window (or last-selected-win (funcall 'ide-skel-get-editor-window)))
		 (sqlplus (car pair) (concat (cadr pair) (concat "." sqlplus-session-file-extension))))))
	    ((eq type 'M-mouse-1)
	     (sqlplus-file-get-source sqlplus-side-view-connect-string object-name object-type))
	    ((eq type 'C-M-mouse-1)
	     (sqlplus-set-project-for-connect-string sqlplus-side-view-connect-string))
	    ((eq type nil))
	    (t
	     (condition-case err
		 (funcall type)
	       (error nil))))
      (select-window (funcall 'ide-skel-get-last-selected-window)))))

(defun sqlplus-side-view-click-on-table-handler (event)
  (interactive "@e")
  (with-selected-window (posn-window (event-start event))
    (let* ((posn-point (posn-point (event-start event)))
	   (object-name (get-text-property posn-point 'object-name))
	   (object-type (get-text-property posn-point 'object-type))
	   (type (car event)))
      (when (eq type 'mouse-3)
	(setq type (car (x-popup-menu t (append (list 'keymap object-name)
						    (list '(sqlplus-refresh-side-view-buffer "Refresh" t))
						    (list '(mouse-1 "Show description" t))
						    (list '(C-mouse-1 "Select *" t))
						    (list '(S-mouse-1 "Get source from Oracle" t))
						    (list '(M-mouse-1 "Search source in filesystem" t))
						    (list (list 'C-M-mouse-1 (concat "Set root dir for " (car (refine-connect-string sqlplus-side-view-connect-string))) t))
						    )))))
      (cond ((eq type 'mouse-1)
	     (sqlplus-execute sqlplus-side-view-connect-string
			      (sqlplus-fontify-string sqlplus-side-view-connect-string (format "desc %s;" object-name))
			      nil nil))
	    ((eq type 'C-mouse-1)
	     (sqlplus-execute sqlplus-side-view-connect-string
			      (sqlplus-fontify-string sqlplus-side-view-connect-string (format "select * from %s;" object-name))
			      nil nil))
	    ((eq type 'S-mouse-1)
	     (sqlplus-get-source sqlplus-side-view-connect-string object-name object-type))
	    ((eq type 'M-mouse-1)
	     (sqlplus-file-get-source sqlplus-side-view-connect-string object-name object-type))
	    ((eq type 'C-M-mouse-1)
	     (sqlplus-set-project-for-connect-string sqlplus-side-view-connect-string))
	    ((eq type nil))
	    (t
	     (condition-case err
		 (funcall type)
	       (error nil))))
      (select-window (funcall 'ide-skel-get-last-selected-window)))))

(defun sqlplus-side-view-click-on-package-handler (event)
  (interactive "@e")
  (with-selected-window (posn-window (event-start event))
    (let* ((posn-point (posn-point (event-start event)))
	   (object-name (get-text-property posn-point 'object-name))
	   (object-type (get-text-property posn-point 'object-type))
	   (type (car event)))
      (when (eq type 'mouse-3)
	(setq type (car (x-popup-menu t (append (list 'keymap object-name)
						(list '(sqlplus-refresh-side-view-buffer "Refresh" t))
						(list '(S-mouse-1 "Get package header from Oracle" t))
						(list '(mouse-1 "Get package body from Oracle" t))
						(list '(S-M-mouse-1 "Search header source in filesystem" t))
						(list '(M-mouse-1 "Search body source in filesystem" t))
						(list (list 'C-M-mouse-1 (concat "Set root dir for " (car (refine-connect-string sqlplus-side-view-connect-string))) t))
						)))))
      (cond ((eq type 'S-mouse-1)
	     (sqlplus-get-source sqlplus-side-view-connect-string object-name object-type))
	    ((eq type 'mouse-1)
	     (sqlplus-get-source sqlplus-side-view-connect-string object-name "PACKAGE BODY"))
	    ((eq type 'M-mouse-1)
	     (sqlplus-file-get-source sqlplus-side-view-connect-string object-name "PACKAGE BODY"))
	    ((eq type 'S-M-mouse-1)
	     (sqlplus-file-get-source sqlplus-side-view-connect-string object-name "PACKAGE"))
	    ((eq type 'C-M-mouse-1)
	     (sqlplus-set-project-for-connect-string sqlplus-side-view-connect-string))
	    ((eq type nil))
	    (t
	     (condition-case err
		 (funcall type)
	       (error nil)))))))

(defun sqlplus-side-view-default-draw-panel (tab-info click-function)
  (let ((pairs (sort (sqlplus-tab-data tab-info) 
		     (lambda (pair1 pair2) (string< (car pair1) (car pair2)))))
	(type-name (upcase (symbol-name (sqlplus-tab-symbol tab-info)))))
    (dolist (pair pairs)
      (let* ((label (format "  % -100s" (car pair)))
	     (km (make-sparse-keymap)))
	(define-key km [down-mouse-1] 'ignore)
	(define-key km [mouse-1] click-function)
	(define-key km [C-down-mouse-1] 'ignore)
	(define-key km [C-mouse-1] click-function)
	(define-key km [S-down-mouse-1] 'ignore)
	(define-key km [S-mouse-1] click-function)
	(define-key km [down-mouse-3] 'ignore)
	(define-key km [mouse-3] click-function)
	(setq label (propertize label
				'mouse-face 'ide-skel-highlight-face
				'face (if (cdr pair)
					  '(sqlplus-side-view-face (foreground-color . "red"))
					'sqlplus-side-view-face)
				'local-map km
				'pointer 'hand
				'object-name (car pair)
				'object-type type-name))
	(insert label)
	(insert "\n")))))

(defun sqlplus-refresh-side-view-buffer ()
  (let* ((tab-info (nth sqlplus-side-view-active-tab sqlplus-side-view-tabset))
	 (update-select (sqlplus-tab-update-select tab-info)))
    (unless (sqlplus-tab-refresh-in-progress tab-info)
      (sqlplus-hidden-select sqlplus-side-view-connect-string update-select 'sqlplus-my-update-handler))))
	
(defun sqlplus-get-default-update-select (symbol)
  (concat "select object_name, object_type, decode( status, 'INVALID', 'I', ' ' ) from user_objects\n"
	  "where object_name not like 'BIN$%'\n"
	  (format "and object_type = '%s';" (upcase (symbol-name symbol)))))

(defun sqlplus-create-side-view-buffer (connect-string)
  (let* ((original-connect-string connect-string)
	 (connect-string (car (refine-connect-string connect-string)))
	 (buffer (funcall 'ide-skel-get-side-view-buffer-create
		 (concat " Ide Skel Right View SQL " connect-string)
		 'right "SQL" (concat "SQL Panel for " connect-string)
		 (lambda (editor-buffer)
		   (let ((connect-string sqlplus-side-view-connect-string))
		     (with-current-buffer editor-buffer
		       (and connect-string
			    (equal (car (refine-connect-string sqlplus-connect-string))
				   (car (refine-connect-string connect-string)))
			    )))))))
    (with-current-buffer buffer
      (set 'ide-skel-tabbar-menu-function
	   (lambda ()
	      (let ((tab-info (nth sqlplus-side-view-active-tab sqlplus-side-view-tabset)))
		(list
		 (unless (sqlplus-tab-refresh-in-progress tab-info) 
		   '(sqlplus-refresh-side-view-buffer "Refresh" t))))))
      (setq sqlplus-side-view-connect-string original-connect-string
	    sqlplus-side-view-active-tab 0
	    sqlplus-side-view-tabset
	    (list
	     (make-sqlplus-tab :id 0 :name "Tab" :symbol 'table :help-string "Tables" :draw-function 'sqlplus-side-view-default-draw-panel
			       :update-select (sqlplus-get-default-update-select 'table)
			       :click-function 'sqlplus-side-view-click-on-table-handler)
	     (make-sqlplus-tab :id 1 :name "Vie" :symbol 'view :help-string "Views" :draw-function 'sqlplus-side-view-default-draw-panel
			       :update-select (sqlplus-get-default-update-select 'view)
			       :click-function 'sqlplus-side-view-click-on-table-handler)
	     (make-sqlplus-tab :id 2 :name "Idx" :symbol 'index :help-string "Indexes" :draw-function 'sqlplus-side-view-default-draw-panel
			       :update-select (sqlplus-get-default-update-select 'index)
			       :click-function 'sqlplus-side-view-click-on-index-handler)
	     (make-sqlplus-tab :id 3 :name "Tri" :symbol 'trigger :help-string "Triggers" :draw-function 'sqlplus-side-view-default-draw-panel
			       :update-select (sqlplus-get-default-update-select 'trigger)
			       :click-function 'sqlplus-side-view-click-on-default-handler)
	     (make-sqlplus-tab :id 4 :name "Seq" :symbol 'sequence :help-string "Sequences" :draw-function 'sqlplus-side-view-default-draw-panel
			       :update-select (sqlplus-get-default-update-select 'sequence)
			       :click-function 'sqlplus-side-view-click-on-default-handler)
	     (make-sqlplus-tab :id 5 :name "Syn" :symbol 'synonym :help-string "Synonyms" :draw-function 'sqlplus-side-view-default-draw-panel
			       :update-select (sqlplus-get-default-update-select 'synonym)
			       :click-function 'sqlplus-side-view-click-on-default-handler)
	     (make-sqlplus-tab :id 6 :name "Pkg" :symbol 'package :help-string "PL/SQL Packages" :draw-function 'sqlplus-side-view-default-draw-panel
			       :update-select (sqlplus-get-default-update-select 'package)
			       :click-function 'sqlplus-side-view-click-on-package-handler)
	     (make-sqlplus-tab :id 7 :name "Prc" :symbol 'procedure :help-string "PL/SQL Functions & Procedures" :draw-function 'sqlplus-side-view-default-draw-panel
			       :update-select (concat "select object_name, object_type, decode( status, 'INVALID', 'I', ' ' ) from user_objects\n"
						      "where object_name not like 'BIN$%'\n"
						      "and object_type in ('FUNCTION', 'PROCEDURE');")
			       :click-function 'sqlplus-side-view-click-on-default-handler)
	     (make-sqlplus-tab :id 8 :name "Sch" :symbol 'schema :help-string "Schemas" :draw-function 'sqlplus-side-view-default-draw-panel
			       :update-select "select username, 'SCHEMA', ' ' from all_users where username not like 'BIN$%';"
			       :click-function 'sqlplus-side-view-click-on-schema-handler)
	     ))
      (sqlplus-side-view-buffer-mode-line))
    buffer))

(defun sqlplus-side-view-redraw (sql-view-buffer &optional window-start-from-tab-info)
  (with-current-buffer sql-view-buffer
    (let* ((point (point))
	   (tab-info (nth sqlplus-side-view-active-tab sqlplus-side-view-tabset))
	   (window-start (when (and (symbol-value 'ide-skel-current-right-view-window)
				    (eq (window-buffer (symbol-value 'ide-skel-current-right-view-window)) (current-buffer)))
			   (if window-start-from-tab-info
			       (sqlplus-tab-display-start tab-info)
			     (line-number-at-pos (window-start (symbol-value 'ide-skel-current-right-view-window)))))))
      (let ((inhibit-read-only t))
	(setq buffer-read-only nil)
	(erase-buffer)
	(when (sqlplus-tab-draw-function tab-info)
	  (funcall (sqlplus-tab-draw-function tab-info) tab-info (sqlplus-tab-click-function tab-info))))
      (if window-start
	  (let ((pos (save-excursion
		       (goto-line window-start)
		       (beginning-of-line)
		       (point))))
	    (set-window-start (symbol-value 'ide-skel-current-right-view-window) pos)
	    (setf (sqlplus-tab-display-start tab-info) window-start))
	(goto-char point)
	(beginning-of-line)))))

(defun sqlplus-side-view-update-data (connect-string alist)
  (let* ((connect-string (car (refine-connect-string connect-string)))
	 (sql-view-buffer (sqlplus-get-side-view-buffer connect-string))
	 was-proc)
    (when sql-view-buffer
      (with-current-buffer sql-view-buffer
	(dolist (pair alist)
	  (let* ((symbol (if (eq (car pair) 'function) 'procedure (car pair)))
		 (data-list (cdr pair))
		 (tab-info (some (lambda (tab)
				   (when (eq (sqlplus-tab-symbol tab) symbol)
				     tab))
				 sqlplus-side-view-tabset)))
	    (when tab-info
	      (setf (sqlplus-tab-refresh-in-progress tab-info) nil)
	      (setf (sqlplus-tab-data tab-info)
		    (if (and (eq symbol 'procedure)
			     was-proc)
			(append (sqlplus-tab-data tab-info) (copy-list data-list))
		    data-list))
	      (when (eq symbol 'procedure)
		(setq was-proc t))
	      (setf (sqlplus-tab-errors-count tab-info)
		    (count t (mapcar 'cdr data-list)))
	      (when (eql sqlplus-side-view-active-tab (sqlplus-tab-id tab-info))
		(sqlplus-side-view-redraw (current-buffer))))))
	(sqlplus-side-view-buffer-mode-line)
	(force-mode-line-update)))))

(defun sqlplus-side-view-window-function (side event &rest list)
  (when (and (eq side 'right)
	     (symbol-value 'ide-skel-current-right-view-window)
	     (with-current-buffer (symbol-value 'ide-skel-current-editor-buffer)
	       sqlplus-connect-string))
    (cond ((memq event '(show editor-buffer-changed))
	   (let ((sql-view-buffer (sqlplus-get-side-view-buffer (with-current-buffer (symbol-value 'ide-skel-current-editor-buffer)
							     sqlplus-connect-string))))
	     (when sql-view-buffer
	       (with-current-buffer sql-view-buffer
		 (set 'ide-skel-tabbar-enabled t)
		 (funcall 'ide-skel-side-window-switch-to-buffer (symbol-value 'ide-skel-current-right-view-window) sql-view-buffer)))))))
  nil)

(add-hook 'ide-skel-side-view-window-functions 'sqlplus-side-view-window-function)


(provide 'sqlplus)

;;; sqlplus.el ends here

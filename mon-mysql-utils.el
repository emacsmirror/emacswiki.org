;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is mon-mysql-utils.el
;;; ================================================================
;;; DESCRIPTION:
;;; mon-mysql-utils provides an alist and completion function for 
;;; accessing MySQL's `mysql' client help facility e.g. 
;;; mysql> help contents
;;; mysql> help [<CATEGORY>|<TOPIC>]
;;;
;;; :SEE-ALSO :FILE naf-mode-sql-skeletons.el
;;; :SEE-ALSO :FILE ../lisp/progmodes/sql.el
;;;
;;; :SEE (URL `http://www.emacswiki.org/cgi-bin/emacs/download/sql-completion.el')
;;; :SEE (URL `http://www.emacswiki.org/emacs/mysql.el')
;;;
;;; FUNCTIONS:►►►
;;; `mon-help-mysql-complete', `mon-mysql-cln-pipes', `mon-mysql-get-field-col'
;;; `mon-csv-string-to-list', `mon-csv-string-map-list', `mon-csv-split-string', 
;;; `mon-csv-map-col-field-pairs'
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; METHODS:
;;;
;;; CLASSES:
;;;
;;; CONSTANTS:
;;;
;;; VARIABLES:
;;; `*regexp-clean-mysql*'
;;;
;;; ALIASED/ADVISED/SUBST'D:
;;; `mon-cln-pipes'                     -> `mon-mysql-cln-pipes'
;;; `mon-mysql-csv-to-list'             -> `mon-csv-string-to-list'
;;; `mon-mysql-csv-split-string'        -> `mon-csv-split-string'
;;; `mon-mysql-csv-map-list'            -> `mon-csv-string-map-list'
;;; `mon-mysql-csv-map-col-field'       -> `mon-csv-map-col-field-pairs'
;;;
;;; DEPRECATED:
;;;
;;; RENAMED:
;;;
;;; MOVED:
;;;
;;; TODO:
;;;
;;; NOTES:
;;;
;;; SNIPPETS:
;;;
;;; REQUIRES:
;;; `mon-cln-pipes' <- `mon-string-from-sequence' -> :FILE mon-utils.el
;;; `mon-cln-pipes' <- `mon-string-to-sequence'   -> :FILE mon-utils.el
;;; :SEE (URL `http://www.emacswiki.org/emacs/mon-utils.el')
;;;
;;; THIRD PARTY CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/mon-mysql-utils.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2009-12-10T14:59:16-05:00Z}#{09504} - by MON>
;;;
;;; FILE-CREATED:
;;; <Timestamp: #{2009-12-10T11:11:41-05:00Z}#{09504} - by MON>
;;; ================================================================
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;;; Floor, Boston, MA 02110-1301, USA.
;;; ================================================================
;;; Permission is granted to copy, distribute and/or modify this
;;; document under the terms of the GNU Free Documentation License,
;;; Version 1.3 or any later version published by the Free Software
;;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;;; and no Back-Cover Texts. A copy of the license is included in
;;; the section entitled "GNU Free Documentation License".
;;; A copy of the license is also available from the Free Software
;;; Foundation Web site at:
;;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ================================================================
;;; Copyright © 2009 MON KEY 
;;; ==============================
;;; CODE:

(eval-when-compile (require 'cl))

;;; ==============================
;;; :NOTE "mysql> help contents"
;;; :CREATED <Timestamp: #{2009-12-10T10:52:19-05:00Z}#{09504} - by MON>
(defvar *regexp-clean-mysql* nil
  "*An alist of association key value pairs for use with `mysql help' command.\n
:EXAMPLE\n (cdr (assoc :DATA-TYPES *regexp-clean-mysql*))\n
 \(mapcar 'car *regexp-clean-mysql*\)\n
Alist keys correpsond to the following `mysql help' categories:\n
  Account Management - Administration - Compound Statements - Data Definition
  Data Manipulation - Data Types - Functions - Geographic Features
  Language Structure - Plugins - Table Maintenance - Transactions
  User-Defined Functions - Utility
  Functions and Modifiers for Use with GROUP BY\n
;; :MYSQL-HELP-KEYS\n
:ACCOUNT-MANAGEMENT \n:ADMINISTRATION \n:COMPOUND-STATEMENTS
:DATA-DEFINITION \n:DATA-MANIPULATION \n:DATA-TYPES
:BIT-FUNCTIONS \n:COMPARISON-OPERATORS \n:CONTROL-FLOW-FUNCTIONS
:DATE-AND-TIME-FUNCTIONS \n:ENCRYPTION-FUNCTIONS \n:INFORMATION-FUNCTIONS
:LOGICAL-OPERATORS \n:MISCELLANEOUS-FUNCTIONS \n:NUMERIC-FUNCTIONS
:STRING-FUNCTIONS \n:GROUP-BY-FUNCTIONS-MODIFIERS \n:GEOGRAPHIC-FEATURES
:GEOMETRY-CONSTRUCTORS \n:GEOMETRY-PROPERTIES \n:GEOMETRY-RELATIONS
:LINESTRING-PROPERTIES \n:MBR \n:POINT-PROPERTIES \n:POLYGON-PROPERTIES
:WKB \n:WKT \n:PLUGINS \n:TABLE-MAINTENANCE \n:TRANSACTIONS
:USER-DEFINED-FUNCTIONS \n:UTILITY \n
:SEE-ALSO `mon-help-mysql-complete', `mon-help-mysql-commands'.\n►►►.")
;;
(eval-when-compile
(unless  (bound-and-true-p *regexp-clean-mysql* )
  (setq *regexp-clean-mysql*
        '((:ACCOUNT-MANAGEMENT . ("create user" "drop user" "grant" 
                                  "rename user" "revoke" "set password"))
          (:ADMINISTRATION . ("binlog" "cache index" "change master to"
                              "deallocate prepare" "execute statement" "flush"
                              "flush query cache" "help command" "kill" 
                              "load data from master" "load index" 
                              "load table from master" "prepare" "purge binary logs"
                              "reset" "reset master" "reset slave" "set" 
                              "set global sql_slave_skip_counter" 
                              "set sql_log_bin" "show"
                              "show authors" "show binary logs" 
                              "show binlog events" "show character set" 
                              "show collation" "show columns" "show contributors"
                              "show create database" "show create event" 
                              "show create function" "show create procedure" 
                              "show create table" "show create trigger" 
                              "show create view" "show databases" "show engine" 
                              "show engines" "show errors" "show events" 
                              "show function code" "show function status" "show grants" 
                              "show index" "show innodb status" "show master status" 
                              "show open tables" "show plugins" "show privileges"
                              "show procedure code" "show procedure status"
                              "show processlist" "show profile" "show profiles"
                              "show slave hosts" "show slave status" 
                              "show status" "show table status" "show tables" 
                              "show triggers" "show variables" "show warnings" 
                              "start slave" "stop slave"))
          (:COMPOUND-STATEMENTS . ("begin end" "case statement" "close" 
                                   "declare condition" "declare cursor" 
                                   "declare handler" "declare variable" 
                                   "fetch" "if statement" "iterate" 
                                   "leave" "loop" "open" "repeat loop"
                                   "return" "select into" "set variable" "while"))
          (:DATA-DEFINITION . ("alter database" "alter event" "alter function"
                               "alter logfile group" "alter procedure" 
                               "alter server" "alter table" "alter tablespace" 
                               "alter view" "constraint" "create database" 
                               "create event" "create function" "create index" 
                               "create procedure" "create server" "create table" 
                               "create tablespace" "create trigger" "create view"
                               "drop database" "drop event" "drop function" 
                               "drop index" "drop procedure" "drop server" 
                               "droptable" "drop tablespace" "drop trigger" 
                               "drop view" "merge" "rename table"))
          (:DATA-MANIPULATION . ("call" "delete" "do" "dual" "handler" "insert"
                                 "insert delayed" "insert select" "join" 
                                 "load data" "replace" "select" "truncate table"
                                 "union" "update"))
          (:DATA-TYPES . ("auto_increment" "bigint" "binary" "bit" "blob" 
                          "blob data type" "boolean" "char" "char byte" "date"
                          "datetime" "dec" "decimal" "double" "double precision"
                          "enum" "float" "int" "integer" "longblob" "longtext"
                          "mediumblob" "mediumint" "mediumtext" "set data type"
                          "smallint" "text" "time" "timestamp" "tinyblob"
                          "tinyint" "tinytext" "varbinary" "varchar" 
                          "year data type"))
          (:BIT-FUNCTIONS . ("&" "<<" ">>" "BIT_COUNT" "^" "|" "~"))
          (:COMPARISON-OPERATORS . ("!=" "<" "<=" "<=>" "=" ">" ">=" 
                                    "between and" "coalesce" "greatest" "in" 
                                    "interval" "is" "is not" "is not null" 
                                    "is null" "isnull" "least" "not between" 
                                    "not in"))
          (:CONTROL-FLOW-FUNCTIONS . ("case operator" "if function"
                                      "ifnull" "nullif"))
          (:DATE-AND-TIME-FUNCTIONS . ("adddate" "addtime" "convert_tz"
                                       "curdate" "current_date" "current_time"
                                       "current_timestamp" "curtime" 
                                       "date function" "datediff" "date_add"
                                       "date_format" "date_sub" "day" "dayname"
                                       "dayofmonth" "dayofweek" "dayofyear"
                                       "extract" "from_days" "from_unixtime"
                                       "get_format" "hour" "last_day"
                                       "localtime" "localtimestamp" "makedate"
                                       "maketime" "microsecond" "minute" "month"
                                       "monthname" "now" "period_add"
                                       "period_diff" "quarter" "second"
                                       "sec_to_time" "str_to_date" "subdate"
                                       "subtime" "sysdate" "time function"
                                       "timediff" "timestamp function"
                                       "timestampadd" "timestampdiff"
                                       "time_format" "time_to_sec" "to_days"
                                       "unix_timestamp" "utc_date" "utc_time"
                                       "utc_timestamp" "week" "weekday"
                                       "weekofyear" "year" "yearweek"))
          (:ENCRYPTION-FUNCTIONS . ("aes_decrypt" "aes_encrypt" "compress"
                                    "decode" "des_decrypt" "des_encrypt"
                                    "encode" "encrypt" "md5" "old_password"
                                    "password" "sha1" "uncompress"
                                    "uncompressed_length"))
          (:INFORMATION-FUNCTIONS . ("benchmark" "charset" "coercibility"
                                     "collation" "connection_id" "current_user"
                                     "database" "found_rows" "last_insert_id"
                                     "row_count" "schema" "session_user"
                                     "system_user" "user" "version"))
          (:LOGICAL-OPERATORS . ("!" "&&" "xor" "||"))
          (:MISCELLANEOUS-FUNCTIONS . ("procedure analyse" "default" "get_lock"
                                       "inet_aton" "inet_ntoa" "is_free_lock"
                                       "is_used_lock" "master_pos_wait"
                                       "name_const" "release_lock" "sleep"
                                       "uuid" "uuid_short" "values"))
          (:NUMERIC-FUNCTIONS . ("%" "*" "+" "- binary" "- unary" "/" "abs"
                                 "acos" "asin" "atan" "atan2" "ceil" "ceiling"
                                 "conv" "cos" "cot" "crc32" "degrees" "div"
                                 "exp" "floor" "ln" "log" "log10" "log2" "mod"
                                 "oct" "pi" "pow" "power" "radians" "rand"
                                 "round" "sign" "sin" "sqrt" "tan" "truncate"))
          (:STRING-FUNCTIONS . ("ascii" "bin" "binary operator" "bit_length"
                                "cast" "char function" "character_length"
                                "char_length" "concat" "concat_ws" "convert"
                                "elt" "export_set" "extractvalue" "field"
                                "find_in_set" "format" "hex" "insert function"
                                "instr" "lcase" "left" "length" "like"
                                "load_file" "locate" "lower" "lpad" "ltrim"
                                "make_set" "match against" "mid" "not like" 
                                "not regexp" "octet_length" "ord" "position" 
                                "quote" "regexp" "repeat function" 
                                "replace function" "reverse" "right" "rpad"
                                "rtrim" "soundex" "sounds like" "space" "strcmp"
                                "substr" "substring" "substring_index" "trim" 
                                "ucase" "unhex" "updatexml" "upper"))
          (:GROUP-BY-FUNCTIONS-MODIFIERS . ("avg" "bit_and" "bit_or" "bit_xor"
                                            "count" "count distinct"
                                            "group_concat" "max" "min" "std"
                                            "stddev" "stddev_pop" "stddev_samp"
                                            "sum" "variance" "var_pop" "var_samp"))
          (:GEOGRAPHIC-FEATURES . ("geometry" "geometry hierarchy" "spatial"))
          (:GEOMETRY-CONSTRUCTORS .  ("geometrycollection" "linestring"
                                      "multilinestring" "multipoint"
                                      "multipolygon" "point" "polygon"))
          (:GEOMETRY-PROPERTIES . ("boundary" "dimension" "envelope"
                                   "geometrytype" "isempty" "issimple" "srid"))
          (:GEOMETRY-RELATIONS . ("contains" "crosses" "disjoint" "equals"
                                  "intersects" "overlaps" "touches" "within"))
          (:LINESTRING-PROPERTIES . ("endpoint" "glength" "numpoints" "pointn"
                                     "startpoint"))
          (:MBR . ("mbr definition" "mbrcontains" "mbrdisjoint" "mbrequal"
                   "mbrintersects" "mbroverlaps" "mbrtouches" "mbrwithin"))
          (:POINT-PROPERTIES   . ("x" "y"))
          (:POLYGON-PROPERTIES . ("area" "exteriorring" "interiorringn"
                                  "numinteriorrings"))
          (:WKB . ("asbinary" "geomcollfromwkb" "geomfromwkb" "linefromwkb"
                   "mlinefromwkb" "mpointfromwkb" "mpolyfromwkb" "pointfromwkb"
                   "polyfromwkb"))
          (:WKT . ("astext" "geomcollfromtext" "geomfromtext" "linefromtext"
                   "mlinefromtext" "mpointfromtext" "mpolyfromtext"
                   "pointfromtext" "polyfromtext" "wkt definition"))
          ;; (:LANGUAGE-STRUCTURE . ( ))
          (:PLUGINS . ("show plugins"))
          (:TABLE-MAINTENANCE . ("analyze table" "backup table" "check table"
                                 "checksum table" "optimize table" 
                                 "repair table" "restore table"))
          (:TRANSACTIONS . ("isolation" "lock" "savepoint" "start transaction"))
          (:USER-DEFINED-FUNCTIONS . ("create function udf"
                                      "drop function udf"))
          (:UTILITY . ("describe" "explain" "help statement" "use"))))))
;;
;;; :TEST-ME (cdr (assoc :STRING-FUNCTIONS *regexp-clean-mysql*))
;;; :TEST-ME (mapcar 'car *regexp-clean-mysql*)
;;
;;; (progn (makunbound '*regexp-clean-mysql*)(unintern '*regexp-clean-mysql*))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-09T17:43:40-05:00Z}#{09503} - by MON>
(defun mon-help-mysql-complete (&optional mysql-key)
  "Return a symbol for the alist keys of `*regexp-clean-mysql*'.
When MYSQL-KEY (a string or :symbol) is non-nil try completing it instead.\n
:EXAMPLE\n\(mon-help-mysql-complete\)
\(mon-help-mysql-complete :ADMINISTR\)
\(mon-help-mysql-complete \":ADMINISTR\"\)
\(cdr \(assoc \(mon-help-mysql-complete\) *regexp-clean-mysql*\)\)\n
To access/send to a running mysql system (sub)process :SEE `mon-get-process'.
:SEE-ALSO `mon-help-mysql-commands'.\n►►►"
  (let (a b)
    (setq a (mapcar 'car *regexp-clean-mysql*))
    (setq b (mapcar #'(lambda (s) (format "%s" s)) a))
    (if mysql-key
        (read (try-completion  (format "%s" mysql-key) b))
        (read (completing-read "Which key \(tab completes\): " b nil t ":")))))
;;
;;; :TEST-ME (mon-help-mysql-complete :ADMINISTR)
;;; :TEST-ME (mon-help-mysql-complete ":ADMINISTR")
;;; :TEST-ME (mon-help-mysql-complete :AD)
;;; :TEST-ME (mon-help-mysql-complete ":AC")
;;; :TEST-ME (mon-help-mysql-complete)

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-11T19:22:39-05:00Z}#{09506} - by MON KEY>
(defun mon-help-mysql-commands (&optional insrtp intrp)
  "Return a MySQL help 'topic' by with completions by category.
Useful for passing to CLI e.g. `mysql> help <SOME-TOPIC>'.
:EXAMPLE (mon-help-mysql-commands)\n
:SEE-ALSO: `mon-help-mysql-complete',`*regexp-clean-mysql*'.\n►►►"
  (interactive "i\np")
  (let ((help-for 
         (completing-read "MySQL help for \(tab completes\): "
                        (cdr (assoc (mon-help-mysql-complete) *regexp-clean-mysql*)))))
    (if (or insrtp intrp)
        (princ (concat "help " help-for) (current-buffer))
        help-for)))
;;
;;; :TEST-ME (mon-help-mysql-commands)
;;; :TEST-ME (call-interactively 'mon-help-mysql-commands)

;;; ==============================
;;; :NOTE For use to get help for a specific MySQL help topic e.g.
;;; "mysql> help contents", "mysql> help [<CATEGORY>|<TOPIC>]"
;;; :TODO needs to test if there is an existing mysql processs running; system|subproccess.
;;; :SEE `mon-get-process'
;;; (let ((help-for 
;;;        (completing-read "Get MySQL help for :"
;;;                         (cdr (assoc (mon-help-mysql-complete) *regexp-clean-mysql*)))))
;;;    (if (processp (get-process "shell"))
;;;        (process-send-string 
;;;         (get-process "shell")
;;;         (concat "help " help-for "\n"))))

;;; ==============================
;;; :NOTE Process related functions. 
;;; :SEE (describe-function 'mon-help-process-functions)

;;; (process-plist (get-process "shell"))
;;; (process-status (get-process "shell"))
;;;  =>run
;;; (process-type (get-process "shell"))
;;;  =>real
;;; (process-send-string process string)
;;; (list-processes)
;;; (process-buffer
;;; (process-id 
;;; ==============================


;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-10T16:39:52-05:00Z}#{09504} - by MON>
(defun mon-mysql-get-field-col (start end &optional field-1 intrp)
  "Return list of extracted rows in region of MySQL table's with FIELD-1 as key.
List has the form \(FIELD-1 (ROW1-VAL ROW2-VAL ROW3-VAL\)\)
When called-interactively or INTRP is non-nil put return value on the kill ring.\n
When called-interactively with prefix arg or FIELD-1 is non-nil match FIELD1 
as first field in table header row. Default is \"Field\".\n
Use to extract fields from mysql command:\nmysql> SHOW COLUMNS FROM THE-DB.TABLE\n
:EXAMPLE 
\(let \(\(r-eg `\(,\(1+ \(search-forward-regexp \"^►\" nil t\)\) . 
                   ,\(- \(search-forward-regexp \"◄$\" nil t\) 3\)\)\)\)
  \(mon-mysql-get-field-col \(car r-eg\) \(cdr r-eg\) \"Tables_in_mysql\" t\)
      \(momentary-string-display \(current-kill 0\) \(point\)\)\)
►\n +---------------------------+ 
 | Tables_in_mysql           | \n +---------------------------+ 
 | columns_priv              | \n | db                        | 
 | event                     | \n | func                      | 
 | general_log               | \n | help_category             | 
 | help_keyword              | \n | help_relation             | 
 | help_topic                | \n | host                      | 
 | ndb_binlog_index          | \n | plugin                    | 
 | proc                      | \n | procs_priv                | 
 | servers                   | \n | slow_log                  | 
 | tables_priv               | \n | time_zone                 | 
 | time_zone_leap_second     | \n | time_zone_name            | 
 | time_zone_transition      | \n | time_zone_transition_type | 
 | user                      | \n +---------------------------+ \n◄\n
:SEE-ALSO `mon-mysql-cln-pipes',`mon-mysql-csv-split-string',
`mon-mysql-csv-to-list', `mon-cln-csv-fields'
`mon-mysql-csv-map-list', `mon-mysql-csv-map-col-field',
`mon-string-csv-rotate'.\n►►►"
  (interactive "r\nsFirst field's value: \np")
  (let (flds
        (the-field (if (or intrp field-1) field-1 "Field")))
    (setq flds (buffer-substring-no-properties start end))
    (setq flds
          (with-temp-buffer
            (insert flds)        
            (goto-char (buffer-end 0))
            ;; Remove whitespace at BOL & EOL.
            (whitespace-cleanup)
            (goto-char (buffer-end 0))
            (while (search-forward-regexp "^[\\[:blank:]]" nil t )(replace-match ""))            
            (goto-char (buffer-end 0))
            ;; Remove all "+----+----+...." lines.
            (while (search-forward-regexp "^\\([+-]+\\)$" nil t) 
              ;; :WAS ;;...1....................2........3.......................
              ;;      "^\\([\\[:blank:]]?\\)\\([+-]+\\)\\([\\[:blank:]]?\\)" nil t)
              (replace-match ""))
            (goto-char (buffer-end 0))
            (progn
              ;; Remove table header.
              (search-forward-regexp (concat "\\(\| " the-field " .* |\\)") nil t)
              (replace-match ""))
            (goto-char (buffer-end 0))
            (while (search-forward-regexp 
                    ;;..1...2........3..............4.....................
                    "^\\(\\(\| \\)\\([A-z0-9_]+\\)\\([\\[:space:]].*\\)\\)$" nil t)
              (replace-match "\\3"))
            (whitespace-cleanup)
            (buffer-substring-no-properties (buffer-end 0)(buffer-end 1))))
    (setq flds (concat "(" flds ")"))
    (setq flds `(,(car (read-from-string field-1)) ,(car (read-from-string flds))))
    (if intrp
        (progn
          (kill-new (format "%S" flds))
          flds)
        flds)))

;;; ==============================
;;; :NOTE character: ␠ (9248, #o22040, #x2420) code point: 0x2420
;;; (ucs-insert 9248) (princ (char-to-string 9248)(current-buffer))
;;; :TEST-ME
;;; (let ((rnd-trip '(83 89 77 66 79 76 32 70 79 82 32 83 80 65 67 69))
;;;       (catch-trip))
;;;   (push rnd-trip catch-trip)
;;;   (setq rnd-trip (subst 9248 32 rnd-trip))
;;;   (push rnd-trip catch-trip)
;;;   (setq catch-trip 
;;;         (mapcar #'(lambda (rnd)
;;;                     (mon-string-from-sequence rnd))
;;;                 catch-trip)))
;;; `mon-string-from-sequence', `mon-string-to-sequence'in :FILE mon-utils.el
;;; :CREATED <Timestamp: #{2009-12-11T17:10:27-05:00Z}#{09505} - by MON>
(defun mon-mysql-cln-pipes (start end &optional to-kill)
  "Return MySQL query table rows in region without the table.
When called-interactively or TO-KILL is non-nil put retun value on kill-ring.\n
:NOTE Replaces empty valued cell ` | | ' with `?NULL?'.
      Converts row values containing whitespace to strings.
      Attempts handling quoted strings in table rows, but may be unreliable.
      May return incorrect results for wrapped lines. Use toggle-truncate-lines.\n
:EXAMPLE\n \(let \(\(r-eg `\(,\(search-forward-regexp \"^►\" nil t\) . 
                ,\(- \(search-forward-regexp \"◄◄\" nil t\) 2\)\)\)\)
   \(mon-mysql-cln-pipes \(car r-eg\) \(cdr r-eg\) t\)
   \(momentary-string-display \(current-kill 0\) \(+ \(point\) 5\)\)\)\n\n
► | \"12000\" | bubba | 1200 | T | \"bubba\" | | 2008-10-25 16:54:04 | 
| 2008-10-25 16:54:04 | 1200 | 0 | \"Quoted string \\\"bubba\\\"\" | | 2008-10-25 16:54:04 | 
| \"2008-05-25 16:54:04\" | 1200 | t | \"bubba\" | | 2008-10-25 16:54:04 |
| 2008-10-25 16:54:04 | 1200 | F | \"bubba\" | | 2008-10-25 16:54:04 |◄◄           \n
:SEE-ALSO `mon-mysql-get-field-col',`mon-mysql-csv-split-string', 
`mon-mysql-csv-to-list', `mon-cln-csv-fields', 
`mon-mysql-csv-map-list', `mon-mysql-csv-map-col-field',
`mon-string-csv-rotate'.\n►►►"
  (interactive "r\np")
  (let (tb mtb)
    (setq tb (buffer-substring-no-properties start end))
    (setq tb (with-temp-buffer
               (toggle-truncate-lines -1) ;; <- Paranoia.
               (insert tb)              
               (goto-char (buffer-end 0))
               ;; Remove all "+----+----+...." lines.
               (while (search-forward-regexp "^\\([+-]+\\)$" nil t)
                 (replace-match ""))
               (goto-char (buffer-end 0))
               ;; pipe at BOL.
               (while (search-forward-regexp "^|[\\[:blank:]]+" nil t)
                 ;; :WAS (replace-match "| "))
                 (replace-match " | "))
               (goto-char (buffer-end 0))
               ;; Remove pipe at EOL.
               (while (search-forward-regexp "[\\[:blank:]]+|$" nil t)
                 ;; :WAS (replace-match " |"))
                 (replace-match " | "))
               ;; Attempt to find empty pairs of pipes ` | | '.
               ;; These may or may not be column vals. replace with ?NULL?
               (goto-char (buffer-end 0))
               (while (search-forward-regexp 
                       "\\([\\[:graph:]]?\\)\\([\\[:blank:]]|[\\[:blank:]]+|[\\[:blank:]]\\)" nil t)
                 (replace-match "\\1 | ?NULL? | ")
                 (search-backward "?NULL?" nil t))
               (goto-char (buffer-end 0))
               ;; Replace the single char cell ` | T | '
               (while (search-forward-regexp 
                       (concat "\\([\\[:blank:]]|[\\[:blank:]]\\)" ;; <- grp1
                               "\\([A-Z]\\{1,1\\}\\)"              ;; <- grp2
                               "\\([\\[:blank:]]+|\\)")            ;; <- grp3 
                       nil t)
                 (replace-match " | \\2 |")
                 (skip-chars-backward " |"))
               (goto-char (buffer-end 0))
               ;; Replace leading whitespace - should only happend on digits?
               (while (search-forward-regexp
                       (concat
                        ;;..1..............................
                        "\\([\\[:blank:]]|[\\[:blank:]]+\\)"
                        ;;..2..3.................
                        "\\(\\([\\[:digit:]]+\\)"
                        ;;..4.............................
                        "\\([\\[:blank:]]|[\\[:blank:]]\\)  \\)") nil t)
                 (replace-match " | \\3 |")
                 (skip-chars-backward " |"))
               (goto-char (buffer-end 0))
               ;; Replace remaning trailing whitespace on digits and UPPERS.
               (while (search-forward-regexp
                       (concat
                        "\\(|[\\[:blank:]]" ;; <-grp1 leading piped w/ space.
                        "\\([A-Z]\\{1,1\\}\\|[0-9]+\\)\\)" ;; <-grp2 A Single UPPER or Digit
                        "\\([\\[:blank:]]+|\\)" ;;<- grp3 trailing space
                        ) nil t)
                 (replace-match "| \\2 |")
                 (skip-chars-backward " |"))
               (goto-char (buffer-end 0))
               ;; remaining _Big_ whitespace is after field data.
               (while (search-forward-regexp  "\\([\\[:blank:]]+|\\)[\\[:blank:]]" nil t)
                 (replace-match " | "))
               (goto-char (buffer-end 0))
               ;; Replace the field data.
               (while (search-forward-regexp  
                       (concat
                        ;;..1.
                        "\\("
                        ;;..2.........................................3....................
                        "\\([^| \n][\\[:graph:]\\[:blank:]][^|]+\\)\\([\\[:blank:]]|\\)\\)"
                        ;;.....4....5...............
                        "\\|\\(| \\([\\[:alpha:]]\\) |\\)") nil t)
                 (let* ((rep-mtch
                         (if (match-string-no-properties 2)
                             (match-string-no-properties 2)
                             (match-string-no-properties 5)))
                        (rep-seq (mon-string-to-sequence  ;; in :FILE `mon-utils.el'
                                  rep-mtch)))
                   (cond ( ;; First char is `"'. 
                          (eq (car rep-seq) 34)
                          ;; Test for whitespace.
                          (if (member 32 rep-seq)
                              (setq rep-seq (mon-string-from-sequence  ;; in :FILE `mon-utils.el'
                                             (subst 9248 32 rep-seq)))
                              ;;(format "%S | " rep-seq)
                              (replace-match (concat rep-seq " |")  nil t)
                              (replace-match (concat rep-mtch " |") nil t)))
                         ( ;; Not string _but_ contains whitespace and should be.
                          (member 32 rep-seq)                                          
                          (setq rep-seq (subst 9248 32 rep-seq))
                          (setq rep-seq (mon-string-from-sequence rep-seq))
                          (replace-match  (format "%S | " rep-seq) nil t))
                         ( ;; Got one char - in case we want to change logic.
                          (eq (length rep-seq) 1)
                          (replace-match (concat rep-mtch " |")))
                         ( ;; Symbol without whitespace
                          t (replace-match (concat rep-mtch " |"))))))
               ;; Remove remaining inter-field pipes per row.
               (goto-char (buffer-end 0))
               (while (search-forward-regexp  "\\(| \\|  + | \\| +| +| \\)" nil t)
                 (replace-match " "))
               (goto-char (buffer-end 0))
               ;; Try to catch any remaining cruft at EOL.
               (whitespace-cleanup)
               (goto-char (buffer-end 0))
               (while (search-forward-regexp  "[\\[:blank:]]?|[\\[:blank:]]?$" nil t)
                 (replace-match ""))
               ;; Did we miss any straggling whitespace at BOL?
               (while (search-forward-regexp "^ " nil t) 
                 (replace-match ""))
               (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
    (setq tb (split-string tb "\n" ))
    (setq mtb (mapcar #'(lambda (r) (split-string r nil t)) tb))
    (setq mtb (remq nil mtb)) ;; Any nil's are from empty lines. Kill them now!
    (setq tb nil)
    (let ((map-tb mtb)
          (tst-tb))
      (while map-tb
        (setq tst-tb 
              (mapcar #'(lambda (rnd)
                          (let ((tst-rnd rnd))
                            (if (stringp tst-rnd)
                                (progn
                                  (setq tst-rnd (mon-string-to-sequence tst-rnd))
                                  (if (member 9248 tst-rnd)
                                      (setq tst-rnd (mon-string-from-sequence (subst 32 9248 tst-rnd)))
                                      (setq tst-rnd (mon-string-from-sequence tst-rnd))))
                                tst-rnd)))
                      (pop map-tb)))
        (push tst-tb tb)))
    (setq mtb (format "%s" tb))
    (if to-kill
        (progn
          (kill-new mtb)
          mtb)
        mtb)))
;;
(defalias 'mon-cln-pipes 'mon-mysql-cln-pipes)
;;
;;; :TEST-ME:
;;; (let ((r-eg `(,(search-forward-regexp "^►" nil t) . 
;;;                 ,(- (search-forward-regexp "◄◄" nil t) 2))))
;;;    (mon-mysql-cln-pipes (car r-eg) (cdr r-eg) t)
;;;    (current-kill 0))
;;
;;,---- :UNCOMMENT-BELOW-TO-TEST
;;| ► | "12000" | bubba | 1200 | T | "bubba" | | 2008-10-25 16:54:04 | 
;;| | 2008-10-25 16:54:04 | 1200 | 0 | "Quoted string \"bubba\"" | | 2008-10-25 16:54:04 | 
;;| | "2008-05-25 16:54:04" | 1200 | t | "bubba" | | 2008-10-25 16:54:04 |
;;| | 2008-10-25 16:54:04 | 1200 | F | "bubba" | | 2008-10-25 16:54:04 |◄◄
;;`----

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-10T19:27:04-05:00Z}#{09505} - by MON>
(defun mon-csv-string-to-list (csv-str)
  "Return a converted csv string (a paren wrapped list) as a lisp list.
Assumes the following:
 o Initial value of is formated as \"\('<INITIAL-VALUE>',
 o A null values has the format ,''
 o Numeric values are not quoted: ,0  instead of ,'0'\n
Primarily useful for tearing down SQL dumps on the way to a pairlis.\n
:EXAMPLE
\(mon-csv-string-to-list
 \"('VALUE','VALUE','VALUE-3','Y','N','T','F','escaped\\\\\\'quote','','',0,1,2\)\"\)\n
\(pairlis
 '\(KEY-0 KEY-1 KEY-3 wrkng pretty will-do gv-a-sht PITA has-val nth first second\)
 \(mon-csv-string-to-list
  \"('VALUE-0','VALUE-1','VALUE-3','Y','N','T','F','escaped\\\\\\'quote','','',0,1,2\)\")\)\n
:SEE-ALSO `mon-mysql-csv-split-string', `mon-mysql-get-field-col',
`mon-mysql-csv-to-list', `mon-cln-csv-fields',
`mon-csv-string-map-list', `mon-mysql-csv-map-col-field',
`mon-string-csv-rotate'.\n►►►"
  (let ((normalize csv-str))
    (setq normalize (replace-regexp-in-string ",\\([0-9]+\\)" ",'\\1'" normalize nil)) ;; ,DIGIT -> ,'DIGIT'
    (setq normalize (replace-regexp-in-string
                     ;;.1.......2..................3.....  
                     "\\((\\)\\([\\[:digit:]]+\\)\\(,\\)" "('\\2'," normalize)) ;; (DIGIT, -> ('DIGIT',
    (setq normalize (replace-regexp-in-string ",''" ",'nil'" normalize)) ;; Replace empty value with 'nil'
    (setq normalize (replace-regexp-in-string "('\\|')" "" normalize)) ;; Remove lead/trail parens.
    (setq normalize (replace-regexp-in-string "','" " " normalize)) ;; Remove the value separators
    (setq normalize (car (read-from-string (concat "(" normalize ")")))) ;; Turn string into a list we can `read'.
    ))
;;
(defalias 'mon-mysql-csv-to-list 'mon-csv-string-to-list)
;;
;;; :TEST-ME 
;;; (mon-csv-string-to-list
;;;   "('VALUE','VALUE','VALUE-3','Y','N','T','F','escaped\\'quote','','',0,1,2)")

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-15T16:10:18-05:00Z}#{09512} - by MON KEY>
(defun mon-csv-string-map-list (csv-list)
  "Map CSV-LIST a of strings containing csv separated values.
Return pretty printed list with each string of CSV-LIST in a list.\n
:EXAMPLE\n\(mon-csv-string-map-list
  '\(\"\('VALUE-A0','VALUE-A1','VALUE-A2','Y','N','T','F','escaped\\\\\'quote','','',0,1,2\)\"
    \"\('VALUE-B0','VALUE-B1','VALUE-A2','N','N','T','T','bubba','','',2,1,0\)\"
    \"\('VALUE-C0','VALUE-C1','VALUE-C2','N','Y','F','T','bubbette','','',1,0,2\)\"\)\)\n
:SEE-ALSO `mon-mysql-csv-split-string', `mon-mysql-get-field-col',
`mon-mysql-csv-to-list', `mon-cln-csv-fields',
`mon-csv-string-map-list', `mon-mysql-csv-map-col-field'
`mon-string-csv-rotate'.\n►►►"
(let (ii)
    (dolist (i csv-list (setq ii (nreverse ii)))
      (push (mon-csv-string-to-list i) ii))))
;;
(defalias 'mon-mysql-csv-map-list 'mon-csv-string-map-list)
;;
;;; :TEST-ME 
;;; (mon-csv-string-map-list
;;;  '("('VALUE-A0','VALUE-A1','VALUE-A2','Y','N','T','F','escaped\\'quote','','',0,1,2)"
;;;    "('VALUE-B0','VALUE-B1','VALUE-A2','N','N','T','T','bubba','','',2,1,0)"
;;;    "('VALUE-C0','VALUE-C1','VALUE-C2','N','Y','F','T','bubbette','','',1,0,2)"))


;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-15T16:54:49-05:00Z}#{09512} - by MON KEY>
(defun mon-csv-map-col-field-pairs (col-val-list csv-field-vals &optional insrtp)
  "Map the pair of key values in COL-VAL-LIST with each val in the list of strings 
CSV-FIELD-VALS. Return a list of lists of of key-val -> field-val for each string.
:EXAMPLE\n\(mon-csv-map-col-field-pairs\n '\(KEY-0 KEY-1 KEY-2\)
 '\(\"\(1,'VAL-A1','VAL-A2'\)\" \"\('VAL-B0',1,''\)\" \"\('VAL-C0','VAL-C1',2\)\"\)\)\n
:SEE-ALSO `mon-mysql-csv-split-string', `mon-mysql-get-field-col',
`mon-mysql-csv-to-list', `mon-cln-csv-fields',
`mon-mysql-csv-map-list', `mon-mysql-csv-map-col-field'
`mon-string-csv-rotate'.\n►►►"
  (let (kvrt)
    (dolist (kv (mon-mysql-csv-map-list csv-field-vals) (setq kvrt (nreverse kvrt)))
      (push (pairlis col-val-list kv) kvrt))
    (if insrtp
        (save-excursion 
          (newline)
          (princ (pp kvrt) (current-buffer)))
        kvrt)))
;;
(defalias 'mon-mysql-csv-map-col-field 'mon-csv-map-col-field-pairs)
;; 
;;; :TEST-ME 
;;; (mon-csv-map-col-field-pairs 
;;;  '(KEY-0 KEY-1 KEY-2)
;;;  '("(1,'VAL-A1','VAL-A2')" "('VAL-B0',1,'')" "('VAL-C0','VAL-C1',2)"))

;;; ==============================
;;; :TODO Decide how we want to handle blocks of csv lists starting from strings.
;;; :CREATED <Timestamp: #{2009-12-15T16:31:58-05:00Z}#{09512} - by MON KEY>
;;; (defun mon- ( &optional insertp intrp) 
;;; (interactive " \ni\np") 
;;; (SOME XXX {...}
;;; (save-excursion 
;;;   (newline) 
;;;   (DO XXX (current-buffer))))

;;; ==============================
;;; :COURTESY Francis J. Wright :HIS csv-mode.el :WAS `csv-split-string'
;;; :CREATED <Timestamp: #{2009-12-10T19:03:25-05:00Z}#{09505} - by MON>
(defun mon-csv-split-string (string &optional separators subexp allowbeg allowend)
  "Split STRING into substrings where there are matches for SEPARATORS.
Return list of substrings between split points.
When SEPARATORS is nil default is \"[ \\f\\t\\n\\r\\v]+\".
SUBEXP specifies a subexpression of SEPARATORS as split point; defaults is 0.\n
If ALLOWBEG is non-nil when there is a match for SEPARATORS at the beginning of
STRING include a null substring for that.
If ALLOWEND is non-nil when match is at the end of STRING include a null
substring for that.\n
:NOTE Modifies the match data; use `save-match-data' if necessary.\n
:SEE-ALSO `mon-mysql-get-field-col', `mon-mysql-csv-to-list', 
`mon-csv-string-map-list', `mon-cln-csv-fields', `mon-mysql-csv-map-col-field'
`mon-string-csv-rotate'.\n►►►"
  (or subexp (setq subexp 0))
  (let ((rexp (or separators "[ \f\t\n\r\v]+"))
	(start 0)
	notfirst
	(list nil))
    (while (and (string-match rexp string
			      (if (and notfirst
				       (= start (match-beginning subexp))
				       (< start (length string)))
				  (1+ start) start))
		(< (match-beginning subexp) (length string)))
      (setq notfirst t)
      (or (and (not allowbeg) (eq (match-beginning subexp) 0))
	  (and (eq (match-beginning subexp) (match-end subexp))
	       (eq (match-beginning subexp) start))
	  (push (substring string start (match-beginning subexp)) list))
      (setq start (match-end subexp)))
    (or (and (not allowend) (eq start (length string)))
	(push (substring string start) list))
    (nreverse list)))
;;
(defalias 'mon-mysql-csv-split-string 'mon-csv-split-string)


;;; ==============================
(provide 'mon-mysql-utils)
;;; ==============================

;;; ================================================================
;;; mon-mysql-utils.el ends here
;;; EOF

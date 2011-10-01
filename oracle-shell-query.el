;;; oracle-shell-query.el --- execute a select sql using sqlplus . -*- coding:utf-8 -*-

;; Copyright (C) 2011 孤峰独秀

;; Author: 孤峰独秀  jixiuf@gmail.com
;; Keywords: oracle sqlplus script shell

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;  execute a select sql sentence .and return the value as a list .
;;  the main function:(oracle-shell-query sql)
;;  for example
;;  (oracle-shell-query "select empno,ename from emp where empno<=7499")
;;  return: (("7369" "SMITH") ("7499" "ALLEN"))

;; http://stackoverflow.com/questions/1639704/sqlplus-statement-from-command-line
;; echo "select 1 from dual;" | sqlplus -s username/password@host:1521/service
;; echo "select 1 from dual;" | sqlplus -s scott/tiger@localhost:1521/orcl
;; echo "set pagesize 100 \n select 1 from dual;" | sqlplus -s scott/tiger@localhost:1521/orcl

;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `osq-username'
;;    oracle user name.
;;    default = "scott"
;;  `osq-password'
;;    oracle user password.
;;    default = "tiger"
;;  `osq-server'
;;    Default server or host.
;;    default = "localhost"
;;  `osq-dbname'
;;    database name .
;;    default = "orcl"
;;  `osq-port'
;;    Default port.
;;    default = 1521
;;  `osq-as-sysdba'
;;    login as sysdba.
;;    default = nil

;;; Code:


(require 'sql)
(defcustom osq-username "scott"
  "oracle user name."
  :group 'sqlparse
  :type 'string)
(defcustom osq-password "tiger"
  "oracle user password."
  :group 'sqlparse
  :type 'string)
(defcustom osq-server "localhost"
  "Default server or host."
  :type 'string
  :group 'SQL
  :safe 'stringp)
(defcustom osq-dbname "orcl"
  "database name ."
  :type 'string
  :group 'SQL
  :safe 'stringp)

(defcustom osq-port 1521
  "Default port."
  :type 'number
  :group 'SQL
  :safe 'numberp)

(defcustom osq-as-sysdba nil
  "login as sysdba."
  :type 'boolean
  :group 'SQL
  :safe 'booleanp)

(defvar osq-linesize 2000
  "Default linesize for sqlplus")

(defun oracle-shell-query(sql)
  "query `sql',and return as list"
  (let ((raw-result  (osq-shell-querry-raw sql)) table)
    (when raw-result
      (when (string-match "\\bERROR\\b" raw-result) (error raw-result))
      (if (string-match "rows will be truncated" raw-result)
          (progn
            (setq osq-linesize (+ osq-linesize 500))
            (setq table (oracle-shell-query sql)))
        (setq table (osq-parse-result-as-list raw-result))))
    table))

;;TEST: (osq-parse-result-as-list (osq-shell-querry-raw "select * from emp"))
(defun osq-parse-result-as-list (raw-result)
  (let  (result row)
    (with-temp-buffer
      (insert raw-result)
      (goto-char (point-min))
      (while (re-search-forward "[ \t\n]* [ \t\n]*" nil t)
        (replace-match " " nil nil))
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]+" nil t)
        (replace-match "" nil nil))
      (goto-char  (point-min))
          (while (not (= (point-at-eol) (point-max)))
            (setq row (split-string (buffer-substring-no-properties (point-at-bol) (point-at-eol)) " " t))
            (setq result (append result (list row)))
;;            (add-to-list 'result row t)
            (forward-line) (beginning-of-line))
          )result ))


(defun osq-shell-querry-raw (sql)
  (let ( (cmd   (format "echo \"%s\" |%s" (osq-generate-sql-script sql) (osq-conn-str))))
;;    (message cmd)
    (shell-command-to-string cmd)))

(defun osq-conn-str()
  " default:sqlplus -s scott/tiger@localhost:1521/orcl"
  (if osq-as-sysdba
      (format "sqlplus -s %s/%s@%s:%s/%s as sysdba"
              osq-username osq-password osq-server osq-port osq-dbname)
    (format "sqlplus -s %s/%s@%s:%s/%s"
            osq-username osq-password osq-server osq-port osq-dbname)
      ))

(defun osq-generate-sql-script(sql)
(when (string-match "\\(.*\\);[ \t]*" sql)
  (setq sql (match-string 1 sql)))
  (with-temp-buffer
    (insert "set heading off;\n")
    (insert (format  "set linesize %d;\n" osq-linesize))
    (insert "set colsep ' ';\n");;column separater
    (insert "set null 'NULL';\n");;
    (insert "set wrap off;\n")
    (insert "set pagesize 0;\n")
    (insert "set feedback off;\n")
    (insert "set serveroutput on;\n")
    ;;      (insert (concat  "spool " tmp-script-file-result " ;\n"))
    (insert  (concat sql " ;\n"))
    ;;      (insert "spool off; \n")
    ;;      (append-to-file (point-min) (point-max) tmp-script-file)
    (buffer-string)
    )
  )
(provide 'oracle-shell-query)
;;; oracle-shell-query.el ends here

;;; mysql-table2entity-4java.el --- mysql table2entity for java   -*- coding:utf-8 -*-

;; Description:mysql table2entity for java
;; Last Updated: 纪秀峰 2013-02-17 20:53:07 星期日
;; Created: 2011-09-18 21:44
;; Author: 纪秀峰(Joseph)  jixiuf@gmail.com
;; Keywords: mysql java entity
;; URL: http://www.emacswiki.org/emacs/mysql-table2entity-4java.el
;; https://github.com/jixiuf/sqlparser
;;  call command : (mysql-table2entity-4java-interactively)

;; Copyright (C) 2011~2012 纪秀峰(Joseph) all rights reserved.

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

;; require mysql-query.el
;;  call command :  mysql-table2entity-4java-interactively
;; it will connect to a mysql intance ,and export all the tables to csharp entities

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `mtej-mysql-java-type-alist'
;;    key must be upcase.
;;    default = (quote ((TINYINT . "int") (SMALLINT . "int") (MEDIUMINT . "int") (INT . "int") (INTEGER . "int") ...))

;;; Code:

(require 'mysql-query)
(require   'cc-mode nil t)

(defcustom mtej-mysql-java-type-alist
  '((TINYINT . "int")
    (SMALLINT . "int")
    (MEDIUMINT . "int")
    (INT . "int")
    (INTEGER . "int")
    (BIGINT . "double")
    (FLOAT . "float")
    (DOUBLE . "double")
    (DECIMAL . "java.math.BigDecimal")
    (CHAR . "String")
    (VARCHAR . "String")
    (TINYBLOB . "byte[]")
    (TINYTEXT . "String")
    (BLOB . "byte[]")
    (TEXT . "String")
    (MEDIUMBLOB . "byte[]")
    (MEDIUMTEXT . "String")
    (LOGNGBLOB . "byte[]")
    (LONGTEXT . "String")
    (DATE . "java.sql.Date")
    (TIME . "java.sql.Time")
    (YEAR . "String")
    (DATETIME . "java.util.Date")
    (TIMESTAMP . "java.sql.Timestamp")
    (ENUM . "String")
    )
  "key must be upcase."
  :group 'SQL
  )

(defun mtej-tablename2classname(tablename)
  "use tablename as the class name."
  tablename)

(defun mtej-columnname2fieldname(columnname)
  "use mysql column name as the field name of generated java entity."
  columnname)

;; (camelize "hello_world") =="Hello_World"
;; (camelize "hello_world" "_") =="HelloWorld"
;; (camelize "HELLO_WORLD" "_") =="HelloWorld"
;; (camelize "helloworld") =="Helloworld"
(defun camelize (s &optional separator )
  "Convert under_score string S to CamelCase string."
  (mapconcat 'identity (mapcar
                        #'(lambda (word) (capitalize (downcase word)))
                        (if separator (split-string s "_") (list s))
                        ) ""))

;; (camelize-method "hello_world_everyone") =="hello_world_everyone"
;; (camelize-method "HELLO_WORLD_EVERYONE") =="hello_world_everyone"
;; (camelize-method "hello_world_everyone" "_") =="helloWorldEveryone"
;; (camelize-method "HELLO_WORLD_EVERYONE" "_")== "helloWorldEveryone"
(defun camelize-method (s &optional separator)
  "Convert under_score string S to camelCase string."
  (mapconcat 'identity (mapcar-head
                        '(lambda (word) (downcase word))
                        '(lambda (word) (capitalize (downcase word)))
                        (if separator (split-string s "_") (list s))) ""))

;; (upcase-first-char "hello") "Hello"
;; (upcase-first-char "HELLO") "HELLO"
;; (upcase-first-char "helloWorld") "HelloWorld"
;; (upcase-first-char "hello_world") "Hello_world"
;; (capitalize "hello_world") "Hello_World"
;; (upcase-first-char "helloWorld" "set") "setHelloWorld"
(defun upcase-first-char (s &optional prefix)
  "make the first char `upcase' and return (concat prefix upcasedstring)"
  (when  (>  (length s) 0)
    (let ( (first-char (substring s 0 1 ))
           (rest  (substring s  1 )))
      (concat (or prefix "") (upcase first-char) rest))))

;; (un-camelcase-string "helloWorld") == "hello_world"
;; (un-camelcase-string "helloWorld" "") == "helloworld"
(defun un-camelcase-string (s &optional sep start)
  "Convert CamelCase string S to lower case with word separator SEP.
    Default for SEP is a hyphen \"-\".

    If third argument START is non-nil, convert words after that
    index in STRING."
  (let ((case-fold-search nil))
    (while (string-match "[A-Z]" s (or start 1))
      (setq s (replace-match (concat (or sep "-")
                                     (downcase (match-string 0 s)))
                             t nil s)))
    (downcase s)))

(defun mapcar-head (fn-head fn-rest list)
  "Like MAPCAR, but applies a different function to the first element."
  (if list
      (cons (funcall fn-head (car list)) (mapcar fn-rest (cdr list)))))
 
(defvar mysql-connection-4-mysql nil)

;; (mtej-get-java-type "INT")
(defun mtej-get-java-type(db-type)
  "find out java type depend on de-type from `mtej-mysql-java-type-alist'
db-type is string."
  (cdr (assoc (intern (upcase db-type))  mtej-mysql-java-type-alist)))

;; (mtej-query-all-tablename-in-db mysql-connection-4-mysql)
(defun mtej-query-all-tablename-in-db( mysql-connection-4-mysql)
  "query all table name from connected mysql `mysql-query.el'"
  (mapcar 'car (mysql-query
                (format "select table_name from information_schema.tables where table_schema='%s'"
                        (cdr (assoc 'dbname mysql-connection-4-mysql)))
                mysql-connection-4-mysql)))

 ;; (mtej-query-table "user" mysql-connection-4-mysql)
(defun mtej-query-table (tablename mysql-connection-4-mysql)
  "query all column name and data type ."
  (mysql-query
   (format "select column_name,data_type from information_schema.columns where table_schema ='%s' and  table_name='%s' "
           (cdr (assoc 'dbname mysql-connection-4-mysql)) tablename)
   mysql-connection-4-mysql)
)

;; (mtej-setter-getter "String" "NAME")
(defun mtej-setter-getter(java-type field)
  (with-temp-buffer
    (insert (concat "private " java-type " " field ";\n"))

    (insert (format "public %s(%s %s){\n"  (upcase-first-char field "set") java-type field))
    (insert (format "this.%s=%s;\n" field field))
    (insert "}\n")

    (insert (format "public %s(%s %s){\n"  (upcase-first-char field "get") java-type field))
    (insert (format "return %s;\n" field ))
    (insert "}\n")
    (buffer-string))
  )

;; (mtej-generate-all-setter-getter "user" mysql-connection-4-mysql)
(defun mtej-generate-all-setter-getter(tablename mysql-connection-4-mysql)
  "generate all setter getter of `tablename'"
  (let (
        (col-type-alist (mtej-query-table tablename mysql-connection-4-mysql))
        field-type field-name)
    (with-temp-buffer
      (dolist (colomnname-type-item col-type-alist)
        (setq field-type (mtej-get-java-type (cadr colomnname-type-item)))
        (setq field-name (mtej-columnname2fieldname (car colomnname-type-item)))
        (insert (mtej-setter-getter field-type field-name))
        (insert "\n"))
      (buffer-string)
      )
    )
  )

(defun mtej-generate-class (setter-getters packagename classname savepath)
  "`setter-getters' are the return type of function `mtej-generate-all-setter-gette'"
  (with-current-buffer (find-file-noselect
                        (expand-file-name
                         (concat classname ".java") savepath))
    (when (boundp 'flymake-mode)(flymake-mode -1) )
    (erase-buffer)
    (insert (format "package %s;\n" packagename))
    (insert "\n")
    (insert (format "public class %s{\n\n" classname))
    (insert setter-getters)
    (insert "}\n")
    (java-mode)
    (indent-region (point-min) (point-max))
    (save-buffer)
    (kill-this-buffer)
    )
  )

(defun mtej-generate-all-classes(package savepath)
  (let ((mysql-connection-4-mysql (call-interactively 'mysql-query-create-connection)))
    (dolist (tablename  (mtej-query-all-tablename-in-db mysql-connection-4-mysql))
      (let  ((classname  (mtej-tablename2classname tablename) )
             (setter-getters (mtej-generate-all-setter-getter tablename mysql-connection-4-mysql)))
        (mtej-generate-class setter-getters package classname savepath)))))

;;;###autoload
(defun mysql-table2entity-4java-interactively()
  (interactive)
  (let ((package (read-string  "java package name:" "" nil ""))
        (savepath (read-directory-name  "save generated class to directory:"  )))
    (when (not (file-directory-p savepath)) (make-directory savepath))
    (mtej-generate-all-classes package savepath)
    (dired savepath)))

(provide 'mysql-table2entity-4java)
;;; mysql-table2entity-4java ends here

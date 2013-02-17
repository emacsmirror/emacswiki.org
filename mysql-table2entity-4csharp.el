;;; mysql-table2entity-4csharp.el --- mysql table2entity for csharp   -*- coding:utf-8 -*-

;; Description:mysql table2entity for csharp
;; Last Updated: 纪秀峰 2013-02-17 20:53:56 星期日
;; Created: 2012-01-15 23:55
;; Author: 纪秀峰(Joseph)  jixiuf@gmail.com
;; Keywords: mysql csharp entity
;; URL: http://www.emacswiki.org/emacs/mysql-table2entity-4csharp.el
;; https://github.com/jixiuf/sqlparser
;;  call command : (mysql-table2entity-4csharp-interactively)

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
;;  call command :  mysql-table2entity-4csharp-interactively
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
;;  `mtec-mysql-csharp-type-alist'
;;    key must be upcase.
;;    default = (quote ((TINYINT . "int") (SMALLINT . "int") (MEDIUMINT . "int") (INT . "int") (INTEGER . "int") ...))

;;; Code:

(require 'mysql-query)
(require   'cc-mode nil t)

(defcustom mtec-mysql-csharp-type-alist
  '((TINYINT . "int")
    (SMALLINT . "int")
    (MEDIUMINT . "int")
    (INT . "int")
    (INTEGER . "int")
    (BIGINT . "double")
    (FLOAT . "float")
    (DOUBLE . "double")
    (DECIMAL . "decimal")
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
    (DATE . "DateTime")
    (TIME . "DateTime")
    (YEAR . "String")
    (DATETIME . "DateTime")
    (TIMESTAMP . "DateTime")
    (ENUM . "String")
    )
  "key must be upcase."
  :group 'SQL
  )

(defun mtec-tablename2classname(tablename)
  "use tablename as the class name."
  tablename)

(defun mtec-columnname2fieldname(columnname)
  "use mysql column name as the field name of generated csharp entity."
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

;; (mtec-get-csharp-type "INT")
(defun mtec-get-csharp-type(db-type)
  "find out csharp type depend on de-type from `mtec-mysql-csharp-type-alist'
db-type is string."
  (cdr (assoc (intern (upcase db-type))  mtec-mysql-csharp-type-alist)))

;; (mtec-query-all-tablename-in-db mysql-connection-4-mysql)
(defun mtec-query-all-tablename-in-db( mysql-connection-4-mysql)
  "query all table name from connected mysql `mysql-query.el'"
  (mapcar 'car (mysql-query
                (format "select table_name from information_schema.tables where table_schema='%s'"
                        (cdr (assoc 'dbname mysql-connection-4-mysql)))
                mysql-connection-4-mysql)))

 ;; (mtec-query-table "user" mysql-connection-4-mysql)
(defun mtec-query-table (tablename mysql-connection-4-mysql)
  "query all column name and data type ."
  (mysql-query
   (format "select column_name,data_type from information_schema.columns where table_schema ='%s' and  table_name='%s' "
           (cdr (assoc 'dbname mysql-connection-4-mysql)) tablename)
   mysql-connection-4-mysql)
)

;; (mtec-setter-getter "String" "NAME")
(defun mtec-setter-getter(csharp-type field-name)
  (let(field property)
    (if (string-match "[A-Z0-9_]" field-name)
        (progn (setq field (concat "_" field-name))
               (setq property field-name))
      (setq field field-name)
      (setq property (capitalize field-name)))
    (with-temp-buffer
      (insert (concat "private " csharp-type " " field ";\n"))
      (insert (concat "public  " csharp-type " " property "\n"))
      (insert "{\n")
      (insert (format "   set { %s = value ; }\n" field))
      (insert (format "   get { return %s  ; }\n" field))
      (insert "}\n")
      (buffer-string)))
  )

;; (mtec-generate-all-setter-getter "user" mysql-connection-4-mysql)
(defun mtec-generate-all-setter-getter(tablename mysql-connection-4-mysql)
  "generate all setter getter of `tablename'"
  (let ((col-type-alist (mtec-query-table tablename mysql-connection-4-mysql))
        field-type field-name)
    (with-temp-buffer
      (dolist (colomnname-type-item col-type-alist)
        (setq field-type (mtec-get-csharp-type (cadr colomnname-type-item)))
        (setq field-name (mtec-columnname2fieldname (car colomnname-type-item)))
        (insert (mtec-setter-getter field-type field-name))
        (insert "\n"))
      (buffer-string)
      )
    )
  )

(defun mtec-generate-class (setter-getters namespacename classname savepath)
  "利用setter-getters 生成一个以`classname'为类名，namespace 为
  `namespacename' 的csharp 实体保存到`savepath'路径下。
 `setter-getters' is generated by function `sstec-generate-all-setter-getter-4table'
and `namespace' is namespace of generated csharp entity ,and `classname' is the name of
generated csharp entity . and the entity is saved in `savepath'"
  (with-current-buffer (find-file-noselect
                        (expand-file-name
                         (concat classname ".cs") savepath))
    ;; disable flymake mode if it is present (I don't use flet ,because it cause error when flymake-mode is not loaded)
    (when (boundp 'flymake-mode)(flymake-mode -1) )
    (erase-buffer)
    (insert "using System;\n")
    (insert "using System.Text;\n\n")
    (insert (format "namespace %s\n" namespacename))
    (insert "{\n")
    (insert (format "public class %s\n" classname))
    (insert "{\n")
    (insert "\n  #region Properties\n")
    (insert setter-getters)
    (insert "  #endregion\n")
    (insert "}\n")
    (insert "}\n")
    (when (featurep 'csharp-mode)
      (csharp-mode)
      (indent-region (point-min) (point-max)))
    (save-buffer)
    (kill-this-buffer)))

;;;###autoload
(defun mtec-generate-all-classes(namespace savepath)
  (let ((mysql-connection-4-mysql (call-interactively 'mysql-query-create-connection)))
    (dolist (tablename  (mtec-query-all-tablename-in-db mysql-connection-4-mysql))
      (let  ((classname  (mtec-tablename2classname tablename) )
             (setter-getters (mtec-generate-all-setter-getter tablename mysql-connection-4-mysql)))
        (mtec-generate-class setter-getters namespace classname savepath)))))


;;;###autoload
(defun mysql-table2entity-4csharp-interactively()
  (interactive)
  (let ((namespace (read-string  "csharp namespace for generate entities:" "" nil ""))
        (savepath (read-directory-name  "save generated class to directory:"  )))
    (when (not (file-directory-p savepath)) (make-directory savepath))
    (mtec-generate-all-classes namespace savepath)
    (dired savepath)))

(provide 'mysql-table2entity-4csharp)
;;; mysql-table2entity-4csharp ends here

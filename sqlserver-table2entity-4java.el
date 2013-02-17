;;; sqlserver-table2entity-4java.el --- sqlserver table2entity for java   -*- coding:utf-8 -*-

;; Description:sqlserver table2entity for java
;; Last Updated: 纪秀峰 2013-02-17 20:54:54 星期日
;; Created: 2011-09-18 21:44
;; Author: 纪秀峰(Joseph)  jixiuf@gmail.com
;; Keywords: sqlserver java entity
;; URL: http://www.emacswiki.org/emacs/sqlserver-table2entity-4java.el
;; https://github.com/jixiuf/sqlparser
;; screencast:
;; http://screencast-repos.googlecode.com/files/emacs-sqlserver-sqlserver-table2entity.mp4.bz2
;;  call command : (sqlserver-table2entity-4java-interactively)

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

;; require sqlserver-query.el
;;  call command :  sqlserver-table2entity-4java-interactively
;; it will connect to a sqlserver intance ,and export all the tables to csharp entities

;; 会提示以输入连接sqlserver 的连接字符串，然后，会查数据字典中的数据,根据当前连接的数据库，
;; 将其中所有的表导出为java Entity.
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
;;  `stej-sqlserver-type-java-type-alist'
;;    key must be upcase.
;;    default = (quote ((BIT . "boolean") (INT . "int") (SMALLINT . "int") (TINYINT . "int") (NUMERIC . "BigDecimal") ...))

;;; Code:

(require 'sqlserver-query)
(require   'cc-mode nil t)

(defcustom stej-sqlserver-type-java-type-alist
  '((BIT . "boolean")
    (INT . "int")
    (SMALLINT . "int")
    (TINYINT . "int")
    (NUMERIC . "BigDecimal")
    (DECIMAL . "BigDecimal")
    (MONEY . "BigDecimal")
    (SMALLMONEY . "BigDecimal")
    (FLOAT . "BigDecimal")
    (REAL . "BigDecimal")
    (DATETIME . "java.sql.Date")
    (SMALLDATETIME . "java.sql.Date")
    (TIMESTAMP . "String")
    (UNIQUEIDENTIFIER . "String")
    (CHAR . "String")
    (VARCHAR . "String")
    (TEXT . "String")
    (NCHAR . "String")
    (NVARCHAR . "String")
    (NTEXT . "String")
    (BINARY . "byte[]")
    (VARBINARY . "byte[]")
    (IMAGE . "byte[]")
    )
  "key must be upcase.
key 是db类型，value 是java 中对应类型.要求key大写"
  :group 'convenience
  )

(defun stej-tablename2classname(tablename)
  "从tablename 生成classname，目录直接以tablename作为类名"
  tablename
  )
(defun stej-columnname2fieldname(columnname)
  "由列名生成java Property名.目前直接用`columnname'作为生成的变量名。"
  columnname
  )

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
 
(defvar sqlserver-connection-4-sqlserver nil)
(defun stej-get-java-type(db-type)
  "find out java type depend on de-type from `stej-sqlserver-type-java-type-alist'"
  (cdr (assoc (intern (upcase db-type))  stej-sqlserver-type-java-type-alist)))
;; (stej-get-java-type "d")

(defun stej-query-all-tablename-in-db()
  "从sqlserver 中查询出当前连接的数据库中所有的表名,用到了`sqlserver-query.el'"
  (mapcar 'car (sqlserver-query "select name from sys.tables"  sqlserver-connection-4-sqlserver))
  )
;; (stej-query-all-tablename-in-db)

;; (stej-query-table "EMP")
;; (stej-query-table "DEPT")
(defun stej-query-table (tablename)
  (sqlserver-query
   (format " select c.name ,t.name from sys.columns c ,sys.types t, sys.objects o where c.user_type_id=t.user_type_id and o.object_id = c.object_id and o.name='%s'" tablename)
   sqlserver-connection-4-sqlserver
   )
  )


;; (stej-setter-getter "String" "name")
;; (stej-setter-getter "String" "NAME")
;; (stej-setter-getter "String" "NAME")
(defun stej-setter-getter(java-type field)
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

;; (stej-generate-all-setter-getter-4table "EMP")
(defun stej-generate-all-setter-getter-4table(tablename)
  "为这张表生成所有的setter-getter ,以字符串形式返回."
  (let (
        (col-type-alist (stej-query-table tablename))
        field-type field-name)
    (with-temp-buffer
      (dolist (colomnname-type-item col-type-alist)
        (setq field-type (stej-get-java-type (cadr colomnname-type-item)))
        (setq field-name (stej-columnname2fieldname (car colomnname-type-item)))
        (insert (stej-setter-getter field-type field-name))
        (insert "\n"))
      (buffer-string)
      )
    )
  )

(defun stej-generate-class (setter-getters packagename classname savepath)
  "利用setter-getters 生成一个以`classname'为类名，package 为
  `packagename' 的的java 实体保存到`savepath'路径下。"
  (with-current-buffer (find-file-noselect
                        (expand-file-name
                         (concat classname ".java") savepath))
    (when (boundp 'flymake-mode)(flymake-mode -1) )
    (erase-buffer)
    (insert (format "package %s;\n" packagename))
    (insert "\n")
    (insert "import java.util.*;\n")
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

;;;###autoload
(defun stej-generate-all-classes(package savepath)
  (unless  ( sqlserver-query-connection-alive-p  sqlserver-connection-4-sqlserver)
    (setq sqlserver-connection-4-sqlserver (call-interactively 'sqlserver-query-create-connection)))
  (dolist (tablename  (stej-query-all-tablename-in-db))
    (let  ((classname  (stej-tablename2classname tablename) )
           (setter-getters (stej-generate-all-setter-getter-4table tablename)))
      (stej-generate-class setter-getters package classname savepath)))
  (sqlserver-query-close-connection sqlserver-connection-4-sqlserver)
  )

;;;###autoload
(defun sqlserver-table2entity-4java-interactively()
  (interactive)
  (let ((package (read-string  "java package name:" "" nil ""))
        (savepath (read-directory-name  "save generated class to directory:"  )))
    (when (not (file-directory-p savepath)) (make-directory savepath))
    (stej-generate-all-classes package savepath)
    (dired savepath)))
;; (sqlserver-table2entity-4java-interactively)

(provide 'sqlserver-table2entity-4java)
;;; sqlserver-table2entity-4java ends here

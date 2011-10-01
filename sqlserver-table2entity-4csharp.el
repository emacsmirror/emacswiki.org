;;; sqlserver-table2entity-4csharp.el --- sqlserver table2entity for csharp   -*- coding:utf-8 -*-

;; Description:sqlserver table2entity for csharp
;; Time-stamp: <Joseph 2011-09-27 14:22:00 星期二>
;; Created: 2011-09-18 21:44
;; Author: 孤峰独秀  jixiuf@gmail.com
;; Maintainer:  孤峰独秀  jixiuf@gmail.com
;; Keywords: sqlserver csharp entity
;; URL: http://www.emacswiki.org/emacs/sqlserver-table2entity-4csharp.el
;; https://github.com/jixiuf/sqlparser

;; Copyright (C) 2011, 孤峰独秀, all rights reserved.

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

;; screencast:
;; http://screencast-repos.googlecode.com/files/emacs-sqlserver-oracle-table2entity.mp4.bz2
;;  call command : (sqlserver-table2entity-4csharp-interactively)
;; 会提示以输入连接sqlserver 的连接字符串，然后，会查数据字典中的数据,根据当前连接的数据库，
;; 将其中所有的表导出为csharp Entity.
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
;;  `sstec-sqlserver-type-csharp-type-alist'
;;    key must be upcase.
;;    default = (quote ((BIT . "bool") (INT . "int") (SMALLINT . "int") (TINYINT . "int") (NUMERIC . "decimal") ...))

;;; Code:
(require 'sqlserver-query)
(require   'csharp-mode nil t)
(defcustom sstec-sqlserver-type-csharp-type-alist
  '((BIT . "bool")
    (INT . "int")
    (SMALLINT . "int")
    (TINYINT . "int")
    (NUMERIC . "decimal")
    (DECIMAL . "decimal")
    (MONEY . "decimal")
    (SMALLMONEY . "decimal")
    (FLOAT . "decimal")
    (REAL . "decimal")
    (DATETIME . "DateTime")
    (SMALLDATETIME . "DateTime")
    (TIMESTAMP . "string")
    (UNIQUEIDENTIFIER . "string")
    (CHAR . "string")
    (VARCHAR . "string")
    (TEXT . "string")
    (NCHAR . "string")
    (NVARCHAR . "string")
    (NTEXT . "string")
    (BINARY . "byte[]")
    (VARBINARY . "byte[]")
    (IMAGE . "byte[]")
    )
  "key must be upcase.
key 是db类型，value 是csharp 中对应类型.要求key大写"
  :group 'convenience
  )

(defun sstec-tablename2classname(tablename)
  "从tablename 生成classname，目录直接以tablename作为类名"
  tablename
  )

(defun sstec-columnname2fieldname(columnname)
  "由列名生成csharp Property名.目前直接用`columnname'作为生成的变量名。"
  columnname
  )

;; (camelize "hello_world") =="Hello_World"
;; (camelize "hello_world" "_") =="HelloWorld"
;; (camelize "HELLO_WORLD" "_") =="HelloWorld"
;; (camelize "helloworld") =="Helloworld"
(defun camelize (s &optional separator )
  "Convert under_score string S to CamelCase string."
  (mapconcat 'identity (mapcar
                        '(lambda (word) (capitalize (downcase word)))
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

(defvar sqlplus-connection-4-sqlserver nil)


(defun sstec-get-csharp-type(db-type)
  "find out csharp type depend on de-type from `sstec-sqlserver-type-csharp-type-alist'"
  (cdr (assoc (intern (upcase db-type))  sstec-sqlserver-type-csharp-type-alist))
  )
;; (sstec-get-csharp-type "d")

(defun sstec-query-all-tablename-in-db()
  "从sqlserver 中查询出当前连接的数据库中所有的表名,用到了`sqlserver-query.el'"
  (mapcar 'car (sqlserver-query "select name from sys.tables"  sqlplus-connection-4-sqlserver)))
;; (sstec-query-all-tablename-in-db)

(defun sstec-query-table (tablename)
  (sqlserver-query
   (format " select c.name ,t.name from sys.columns c ,sys.types t, sys.objects o where c.user_type_id=t.user_type_id and o.object_id = c.object_id and o.name='%s'" tablename)
   sqlplus-connection-4-sqlserver
   )
  )
;; (sstec-query-table "EMP")
;; (sstec-query-table "DEPT")
;; (sstec-setter-getter "string" "nameAge")
;; (sstec-setter-getter "string" "FIRST_NAME")
(defun sstec-setter-getter(csharp-type filed-name)
  (let(field property)
    (if (string-match "[A-Z0-9_]" filed-name)
        (progn (setq field (concat "_" filed-name))
               (setq property filed-name))
      (setq field filed-name)
      (setq property (capitalize filed-name)))
    (with-temp-buffer
      (insert (concat "private " csharp-type " " field ";\n"))
      (insert (concat "public  " csharp-type " " property "\n"))
      (insert "{\n")
      (insert (format "   set { %s = value ; }\n" field))
      (insert (format "   get { return %s  ; }\n" field))
      (insert "}\n")
      (buffer-string))))

;; (sstec-generate-all-setter-getter-4table "EMP")
(defun sstec-generate-all-setter-getter-4table(tablename)
  "为这张表生成所有的setter-getter ,以字符串形式返回."
  (let (
        (col-type-alist (sstec-query-table tablename))
        field-type field-name)
    (with-temp-buffer
      (dolist (colomnname-type-item col-type-alist)
        (setq field-type (sstec-get-csharp-type (cadr colomnname-type-item)))
        (setq field-name (sstec-columnname2fieldname (car colomnname-type-item)))
        (insert (sstec-setter-getter field-type field-name))
        (insert "\n"))
      (buffer-string)
      )
    )
  )

(defun sstec-generate-class (setter-getters namespacename classname savepath)
  "利用setter-getters 生成一个以`classname'为类名，namespace 为
  `namespacename' 的的csharp 实体保存到`savepath'路径下。"
  (with-current-buffer (find-file-noselect
                        (expand-file-name
                         (concat classname ".cs") savepath))
    (erase-buffer)
    (insert "using System;\n")
    (insert "using System.Text;\n")
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
      (indent-region (point-min) (point-max))
      )
    (save-buffer)
    (kill-this-buffer)
    )
  )

;;;###autoload
(defun sstec-generate-all-classes(namespace savepath)
  (unless (and sqlplus-connection-4-sqlserver
               (equal (process-status (nth 0  sqlplus-connection-4-sqlserver)) 'run))
    (setq sqlplus-connection-4-sqlserver (call-interactively 'sqlserver-query-create-connection)))

  (dolist (tablename  (sstec-query-all-tablename-in-db))
    (let  ((classname  (sstec-tablename2classname tablename) )
           (setter-getters (sstec-generate-all-setter-getter-4table tablename)))
      (sstec-generate-class setter-getters namespace classname savepath)))
  (sqlserver-query-close-connection sqlplus-connection-4-sqlserver)
  )

;;;###autoload
(defun sqlserver-table2entity-4csharp-interactively()
  (interactive)
  (let ((namespace (read-string  "csharp namespace name:" "" nil ""))
        (savepath (read-directory-name  "save generated class to directory:"  )))
    (sstec-generate-all-classes namespace savepath)
    )
  )
(provide 'sqlserver-table2entity-4csharp)
;;; oracle-table2entity-4csharp ends here

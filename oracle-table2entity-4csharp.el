;;; oracle-table2entity-4csharp.el --- oracle table2entity for csharp   -*- coding:utf-8 -*-

;; Description:oracle table2entity for csharp
;; Last Updated: 纪秀峰 2013-02-17 20:59:19 星期日
;; Created: 2011-09-18 21:44
;; Author: 孤峰独秀  jixiuf@gmail.com
;; Maintainer:  孤峰独秀  jixiuf@gmail.com
;; Keywords: oracle csharp entity
;; URL: http://www.emacswiki.org/emacs/oracle-table2entity-4csharp.el
;; https://github.com/jixiuf/sqlparser

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

;; screencast:
;; http://screencast-repos.googlecode.com/files/emacs-sqlserver-oracle-table2entity.mp4.bz2
;;  call command : (oracle-table2entity-4csharp-interactively)
;; 会提示以输入连接oracle 的连接字符串，然后，会查数据字典中的数据,根据当前连接的数据库，
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
;;  `otec-oracle-type-csharp-type-alist'
;;    key must be upcase.
;;    default = (quote (("CHAR" . "string") ("VARCHAR2" . "string") ("NCHAR" . "string") ("NVARCHAR2" . "string") ("DATE" . "string") ...))

;;; Code:
(require 'oracle-query)
(require   'csharp-mode nil t)
(defcustom otec-oracle-type-csharp-type-alist
  '(("CHAR" . "string")
    ("VARCHAR2" . "string")
    ("NCHAR" . "string")
    ("NVARCHAR2" . "string")
    ("DATE" . "string")
    ("LONG" . "decimal")
    ("RAW" . "byte[]")
    ("BLOB" . "byte[]" )
    ("NCLOB" . "string")
    ("BFILE" . "byte[]")
    ("NUMBER" . "decimal")
    ("DECIMAL" . "decimal")
    ("INTEGER" . "decimal")
    ("REAL" . "decimal")
    )
  "key must be upcase.
key 是db类型，value 是csharp 中对应类型.要求key大写"
  :group 'convenience
  )

(defvar sqlplus-connection nil)


(defun otec-get-csharp-type(db-type)
  "find out csharp type depend on de-type from `otec-oracle-type-csharp-type-alist'"
  (cdr (assoc (upcase db-type)  otec-oracle-type-csharp-type-alist)))
;; (otec-get-csharp-type "d")

(defun otec-query-all-tablename-in-db()
  "从oracle 中查询出当前连接的数据库中所有的表名,用到了`oracle-query.el'"
  (mapcar 'car (oracle-query "select table_name from user_tables"  sqlplus-connection)))
;; (otec-query-all-tablename-in-db)

(defun otec-query-table (tablename)
  (oracle-query
   (format "select column_name ,data_type from user_tab_columns where table_name  ='%s'" tablename)
   sqlplus-connection
   )
  )
;; (otec-query-table "EMP")
;; (otec-query-table "DEPT")
;; (otec-setter-getter "string" "nameAge")
;; (otec-setter-getter "string" "FIRST_NAME")
(defun otec-setter-getter(csharp-type filed-name)
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



(defun otec-tablename2classname(tablename)
  "从tablename 生成classname，目录直接以tablename作为类名"
  tablename
  )
(defun otec-columnname2fieldname(columnname)
  "由列名生成csharp Property名.目前直接用`columnname'作为生成的变量名。"
  columnname
  )


;; (otec-generate-all-setter-getter-4table "EMP")
(defun otec-generate-all-setter-getter-4table(tablename)
  "为这张表生成所有的setter-getter ,以字符串形式返回."
  (let (
        (col-type-alist (otec-query-table tablename))
        field-type field-name)
    (with-temp-buffer
      (dolist (colomnname-type-item col-type-alist)
        (setq field-type (otec-get-csharp-type (cadr colomnname-type-item)))
        (setq field-name (otec-columnname2fieldname (car colomnname-type-item)))
        (insert (otec-setter-getter field-type field-name))
        (insert "\n"))
      (buffer-string)
      )
    )
  )

(defun otec-generate-class (setter-getters namespacename classname savepath)
  "利用setter-getters 生成一个以`classname'为类名，namespace 为
  `namespacename' 的的csharp 实体保存到`savepath'路径下。"
  (with-current-buffer (find-file-noselect
                        (expand-file-name
                         (concat classname ".cs") savepath))
    (when (boundp 'flymake-mode)(flymake-mode -1) )
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
    ))

;;;###autoload
(defun otec-generate-all-classes(namespace savepath)
  (unless (and sqlplus-connection
               (equal (process-status (nth 0  sqlplus-connection)) 'run))
    (setq sqlplus-connection (call-interactively 'oracle-query-create-connection)))
  (dolist (tablename  (otec-query-all-tablename-in-db))
    (let  ((classname  (otec-tablename2classname tablename) )
           (setter-getters (otec-generate-all-setter-getter-4table tablename)))
      (otec-generate-class setter-getters namespace classname savepath)))
  (oracle-query-close-connection sqlplus-connection)
  )

;;;###autoload
(defun oracle-table2entity-4csharp-interactively()
  (interactive)
  (let ((namespace (read-string  "csharp namespace name:" "" nil ""))
        (savepath (read-directory-name  "save generated class to directory:"  )))
    (when (not (file-directory-p savepath)) (make-directory savepath))
    (otec-generate-all-classes namespace savepath)
    (dired savepath)))

(provide 'oracle-table2entity-4csharp)
;;; oracle-table2entity-4csharp ends here

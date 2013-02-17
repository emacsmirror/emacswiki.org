;;; sqlparser-sqlserver-complete.el --- complete tablename and columnname when editing sql -*- coding:utf-8 -*-

;; Copyright (C) 2011~2012 纪秀峰(Joseph) all rights reserved.

;; Created: 2011年08月19日 星期五 00时38分17秒
;; Last Updated: Joseph 2012-03-16 01:25:57 星期五
;; Version: 0.1.2
;; Author: 纪秀峰(Joseph)  jixiuf@gmail.com
;; Keywords: sql complete sqlserver
;; URL:http://www.emacswiki.org/emacs/download/sqlparser-sqlserver-complete.el
;;     https://github.com/jixiuf/sqlparser
;; screencast: http://screencast-repos.googlecode.com/files/emacs-sqlparse-mysql-complete.mkv.bz2

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
;; it can parsing current sql sentence ,it is smart enough to choose to
;; completing table name or column name depending on your position.
;; 1 after keyword 'use'   :complete schema name (on mysql)
;; 2 after keyword 'select' 'set' 'where'    :complete  columnname.
;; 3 after keyword 'alter', 'from' 'update' 'desc'  'show' : complete tablename
;; 4 after keyword 'into' and and there isn't a
;; "\\(" between 'into' and current postion :complete tablename
;; 4.1 after keyword 'into' but there is a "(" between 'into' and current
;; position  :complete columnname
;; 5 after keyword 'values'  :complete nothing.

;;; Installation
;; 1 it required sqlserver-query.el you should download and add it to you load-path.
;; http://www.emacswiki.org/emacs/download/sqlserver-query.el

;;  there is a minor mode defined here
;;       (sqlserver-complete-minor-mode)
;; you can add it to sql-mode-hook
;; (add-hook  'sql-mode-hook 'sqlserver-complete-minor-mode)
;; or call M-x sqlserver-complete-minor-mode

;;  and complete command is binded on `TAB' .
;;
;; 1. you should custom these variable
;; `sqlserver-connection-info'
;;  `sqlserver-cmd' ;sqlcmd or osql
;; for example
;; (setq sqlserver-connection-info
;;       '((username . "sa")
;;         (password . "sa")
;;         (server-instance . "localhost\\SQLEXPRESS")
;;         (dbname . "master"))
;;       )
;; or sometimes
;; (setq sqlserver-connection-info
;;       '((username . "sa")
;;         (password . "sa")
;;         (server-instance . "localhost")
;;         (dbname . "master")))

;; (setq sqlserver-cmd' 'sqlcmd) or (setq sqlserver-cmd' 'osql)
;;
;;
;; my config file about sqlserver-complete-minor-mode looks like this .
;; (require 'sql)
;; (require 'sqlparser-sqlserver-complete)
;; (add-hook  'sql-mode-hook 'sqlserver-complete-minor-mode)
;; (add-hook  'sqlserver-complete-minor-mode-hook 'sqlserver-complete-minor-mode-setup)
;; (defun sqlserver-complete-minor-mode-setup()
;;   (setq sqlserver-connection-info
;;     '((username . "sa")
;;       (password . "sa")
;;       (server-instance . "localhost")
;;       (dbname . "HAIHUA"))
;;     )
;;   (setq sqlserver-cmd 'sqlcmd)
;;   )


;;; Commands:
;;
;; Below are complete command list:
;;
;;  `sqlserver-complete-minor-mode'
;;    mode for editing sqlserver script
;;  `sqlparser-sqlserver-complete'
;;    complete tablename or column name depending on current point
;;  `helm-sqlserver-complete'
;;    call `helm' to complete tablename and column name for sqlserver.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:


(require 'sqlserver-query)
(require 'thingatpt)
(require 'helm nil t)

(defvar sqlparser-sqlserver-connection nil)
(make-variable-buffer-local 'sqlparser-sqlserver-connection)

(defvar sqlparser-sqlserver-opened-connections nil)

(defvar sqlserver-complete-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (if (featurep 'helm)
        (define-key map  (quote [tab]) 'helm-sqlserver-complete)
      (define-key map  (quote [tab]) 'sqlparser-sqlserver-complete)
      )
    map))
(defvar  sqlserver-complete-minor-mode-hook nil)

;;;###autoload
(define-minor-mode sqlserver-complete-minor-mode
  "mode for editing sqlserver script"
  :lighter " MSSqlC"
  :keymap sqlserver-complete-minor-mode-map
  :group 'SQL
  (if sqlserver-complete-minor-mode
      (run-hooks 'sqlserver-complete-minor-mode-hook)))

;;;###autoload
(defun sqlparser-sqlserver-complete()
  "complete tablename or column name depending on current point
position ."
  (interactive)
  (let ((prefix  (sqlparser-word-before-point-4-sqlserver))
        (init-pos (point))
        last-mark)
    (insert (completing-read "complete:" ( sqlparser-sqlserver-context-candidates) nil t prefix))
    (setq last-mark (point-marker))
    (goto-char init-pos)
    (backward-delete-char (length prefix))
    (goto-char (marker-position last-mark))
   ))

(defvar helm-c-source-sqlserver-candidates nil)
(defvar helm-init-postion-4-sqlserver)
(defvar helm-c-source-sqlserver
  '((name . "SQL Object:")
    (init (lambda() (setq helm-init-postion-4-sqlserver (point))(setq helm-c-source-sqlserver-candidates (sqlparser-sqlserver-context-candidates))))
    (candidates . helm-c-source-sqlserver-candidates)
    (action . (("Complete" . (lambda(candidate) (goto-char helm-init-postion-4-sqlserver) (backward-delete-char (length (sqlparser-word-before-point-4-sqlserver))) (insert candidate)))))))

;;;###autoload
(defun helm-sqlserver-complete()
  "call `helm' to complete tablename and column name for sqlserver."
  (interactive)
  (let ((helm-execute-action-at-once-if-one t)
        (helm-quit-if-no-candidate
         (lambda () (message "complete failed."))))
    (helm '(helm-c-source-sqlserver)
              ;; Initialize input with current symbol
              (regexp-quote (sqlparser-word-before-point-4-sqlserver))  nil nil)))

(defun  sqlparser-sqlserver-context-candidates()
  "it will decide to complete tablename or columnname depend on
  current position."
  (unless (sqlserver-query-connection-alive-p  sqlparser-sqlserver-connection)
    (dolist (buf-conn-alist  sqlparser-sqlserver-opened-connections); close unused connections
      (unless (and (bufferp (car buf-conn-alist))
                   (buffer-live-p (car buf-conn-alist)))
        (sqlserver-query-close-connection (nth 1 buf-conn-alist))))
    (setq sqlparser-sqlserver-connection (call-interactively 'sqlserver-query-create-connection))
    (add-to-list 'sqlparser-sqlserver-opened-connections (list (current-buffer) sqlparser-sqlserver-connection)))

  (let ((context (sqlparser-parse-4-sqlserver)) candidats)
    (cond
     ((string= "database" context)
      (setq candidats (sqlparser-sqlserver-all-databasename-candidates))
      )
     ((string= "table" context)
      (setq candidats (sqlparser-sqlserver-tablename-schemaname-databasename-candidates))
      )
     ((string= "column" context)
      (setq candidats ( sqlparser-sqlserver-column-candidates))
      )
     ((string= "procedure" context)
      (setq candidats (sqlparser-sqlserver-all-procedures-candidates))
      )
     ((null context) nil))
    candidats))

;; Test
;; (sqlparser-sqlserver-tablename-schemaname-databasename-candidates "")
;; (sqlparser-sqlserver-tablename-schemaname-databasename-candidates "d")
;; (sqlparser-sqlserver-tablename-schemaname-databasename-candidates "[d")
;; (sqlparser-sqlserver-tablename-schemaname-databasename-candidates "m")

;; (sqlparser-sqlserver-tablename-schemaname-databasename-candidates "[master].[dbo")
;; (sqlparser-sqlserver-tablename-schemaname-databasename-candidates "[master].dbo")
;; (sqlparser-sqlserver-tablename-schemaname-databasename-candidates "master.db_o")
;; (sqlparser-sqlserver-tablename-schemaname-databasename-candidates "dbo.spt_f")
;; (sqlparser-sqlserver-tablename-schemaname-databasename-candidates "master.dbo.spt")
;; (sqlparser-sqlserver-tablename-schemaname-databasename-candidates "master.dbo.[spt")
;; (sqlparser-sqlserver-tablename-schemaname-databasename-candidates "[master].dbo.[spt")

(defun sqlparser-sqlserver-tablename-schemaname-databasename-candidates (&optional prefix)
  "complete table name or schemanmae or databasename ."
  (let* ((prefix (or prefix  (sqlparser-get-prefix-4-sqlserver))); maybe master.dbo.tablename ,dbo.tablename tablename [dbo].[tablename]
         (sub-prefix (split-string prefix "\\." nil))
         sql result)
    (cond ((= (length sub-prefix) 1)
            (let* ((tablefullname (sqlparser-sqlserver-split-three  (car sub-prefix)))
                    (name-prefix (car tablefullname))
                    (name-suffix   (if (equal "[" name-prefix) "]" ""))
                    (name  (nth 1 tablefullname)))
              (setq sql (format "select '%s'+  name  + '%s' from sys.tables where name like '%s%%' union all select '%s' + name + '%s.' from sys.databases where name like '%s%%' union all select '%s' + name + '%s.' from sys.schemas where name like '%s%%'"
                                name-prefix  name-suffix (replace-regexp-in-string "_" "[_]" name)
                                name-prefix  name-suffix (replace-regexp-in-string "_" "[_]" name)
                                name-prefix  name-suffix  (replace-regexp-in-string "_" "[_]" name)))))

          ((= (length sub-prefix) 2); maybe databasename.schename or schename.tablename
            (let* ((fullname1 (sqlparser-sqlserver-split-three (car sub-prefix)))
                   (name1-prefix (car fullname1))
                   (name1 (nth 1 fullname1))
                   (name1-suffix  (if (equal "[" name1-prefix) "]" ""))
                   (fullname2 (sqlparser-sqlserver-split-three (nth 1 sub-prefix)))
                   (name2-prefix (car fullname2))
                   (name2 (nth 1 fullname2))
                   (name2-suffix  (if (equal "[" name2-prefix) "]" ""))
                   (db-exists
                    (>  (string-to-number (caar (sqlserver-query
                                                 (format  "select count(name) from sys.databases where name='%s'" name1)
                                                 sqlparser-sqlserver-connection))) 0)))
              (if db-exists
                  (setq sql (format
                             "(select '%s' + name + '%s.' schemaname from [%s].[sys].[schemas] where name like '%s%%' and exists (select name from sys.databases where name='%s')) union all (select '%s' + name +'%s' tablename from sys.tables where schema_name(schema_id) ='%s' and name like '%s%%')"
                             name2-prefix name2-suffix name1  (replace-regexp-in-string "_" "[_]" name2) name1
                             name2-prefix name2-suffix name1 (replace-regexp-in-string "_" "[_]" name2
                            )))
                (setq sql (format
                           "select '%s' + name +'%s' tablename from sys.tables where schema_name(schema_id) ='%s' and name like '%s%%' union select '%s' + name +'%s' tablename from sys.all_views where schema_name(schema_id) ='%s' and name like '%s%%' "
                             name2-prefix name2-suffix name1  (replace-regexp-in-string "_" "[_]" name2)
                             name2-prefix name2-suffix name1  (replace-regexp-in-string "_" "[_]" name2)
                             ))
               )
             )
           )
          ((= (length sub-prefix) 3)
            (let* ((fullname1 (sqlparser-sqlserver-split-three (car sub-prefix)))
                   (name1-prefix (car fullname1))
                   (name1 (nth 1 fullname1))
                   (name1-suffix  (if (equal "[" name1-prefix) "]" ""))
                   (fullname2 (sqlparser-sqlserver-split-three (nth 1 sub-prefix)))
                   (name2-prefix (car fullname2))
                   (name2 (nth 1 fullname2))
                   (name2-suffix  (if (equal "[" name2-prefix) "]" ""))
                   (fullname3 (sqlparser-sqlserver-split-three (nth 2 sub-prefix)))
                   (name3-prefix (car fullname3))
                   (name3 (nth 1 fullname3))
                   (name3-suffix  (if (equal "[" name3-prefix) "]" ""))
                  )
              (setq sql (format " SELECT '%s' + name + '%s' tablename FROM sys.tables WHERE exists (select name from sys.databases where name = '%s') and schema_name(schema_id)= '%s' and name LIKE '%s%%' union  SELECT '%s' + name + '%s' tablename FROM sys.all_views WHERE exists (select name from sys.databases where name = '%s') and schema_name(schema_id)= '%s' and name LIKE '%s%%'"
                                name3-prefix name3-suffix name1 name2 (replace-regexp-in-string "_" "[_]" name3)
                                name3-prefix name3-suffix name1 name2 (replace-regexp-in-string "_" "[_]" name3)
                                )))))
    (mapcar 'car (sqlserver-query sql sqlparser-sqlserver-connection))))

;;(sqlparser-sqlserver-split-three "[abc]") return ("[" "abc" "]")
;;(sqlparser-sqlserver-split-three "[abc") return ("[" "abc" "")
;; (sqlparser-sqlserver-split-three "abc") return ("" "abc" "")
;; (sqlparser-sqlserver-split-three "") return ("" "" "")
(defun sqlparser-sqlserver-split-three(str)
  "when str=[abc] ,then return '(\"[\" \"abc\" \"]\")
   when str=[abc  ,then return '(\"[\" \"abc\" \"\")
   when str=abc   ,then return '(\"\" \"abc\" \"\")"
  (if  (or (null str) (string= str ""))
      (list "" "" "")
    (let ((prefix "") (suffix "") (middle str))
      (when (string-match "^\\[" middle)
        (setq prefix "[")
        (setq middle  (substring-no-properties middle 1)))
      (when (string-match "]" middle)
        (setq suffix "]")
        (setq middle (substring-no-properties str 1   (length middle))))
      (list prefix  middle suffix)
     )))

;; Test:
;; (sqlparser-sqlserver-get-matched-columns "t3" "s")
;; (sqlparser-sqlserver-get-matched-columns "[t3]" "[s")
;; (sqlparser-sqlserver-get-matched-columns "[t3]")
;; (sqlparser-sqlserver-get-matched-columns "dbo.t3" "[s")
;; (sqlparser-sqlserver-get-matched-columns "dbo.t3" "s")
;; (sqlparser-sqlserver-get-matched-columns "dbo.t3" "")
;; (sqlparser-sqlserver-get-matched-columns "master.dbo.dt" "t")
;; (sqlparser-sqlserver-get-matched-columns "master.[dbo].dt" "[t")
(defun sqlparser-sqlserver-get-matched-columns(tablename &optional column-prefix)
  "tablename can be [master].[dbo].test ,or test or [dbo].test"
  (let* (;;("[master]" "dbo" "test") or ("dbo" "test") or ("test")
         (tablename-string-list (split-string tablename "[ \t\\.]" t))
         sql
         (column-prefix-string (nth 1 (sqlparser-sqlserver-split-three  (or  column-prefix ""))))
         (column-prefix-prefix (car  (sqlparser-sqlserver-split-three  (or  column-prefix ""))))
         (column-prefix-sufix    (if (equal "[" column-prefix-prefix) "]" ""))
        )
    (cond ((= (length tablename-string-list) 1);tablename
            (setq sql (format "select '%s' + col.name + '%s' from sys.objects obj, sys.tables tab , sys.columns col where tab.[object_id]=obj.[object_id] and col.[object_id]=obj.[object_id] and tab.name ='%s'  and col.name like '%s%%'"
                              column-prefix-prefix column-prefix-sufix
                              (nth 1 (sqlparser-sqlserver-split-three  (car tablename-string-list)))
                              column-prefix-string))

           )
          ((= (length tablename-string-list) 2); schema.tablename
            (setq sql (format "select '%s' + col.name + '%s' from sys.objects obj, sys.tables tab , sys.columns col where tab.[object_id]=obj.[object_id] and col.[object_id]=obj.[object_id] and SCHEMA_NAME(tab.schema_id)='%s' and tab.name ='%s'  and col.name like '%s%%'"
                              column-prefix-prefix column-prefix-sufix
                              (nth 1 (sqlparser-sqlserver-split-three  (car tablename-string-list)))
                              (nth 1 (sqlparser-sqlserver-split-three  (nth 1  tablename-string-list)))
                              column-prefix-string))
           )
          ((= (length tablename-string-list) 3); dbname.schema.tablename
            (setq sql (format "select '%s' + col.name + '%s' from %s.sys.objects obj, %s.sys.tables tab , %s.sys.columns col where tab.[object_id]=obj.[object_id] and col.[object_id]=obj.[object_id] and SCHEMA_NAME(tab.schema_id)='%s' and tab.name ='%s'  and col.name like '%s%%'"
                              column-prefix-prefix column-prefix-sufix
                              (nth 1 (sqlparser-sqlserver-split-three  (car tablename-string-list)));db
                              (nth 1 (sqlparser-sqlserver-split-three  (car tablename-string-list)));db
                              (nth 1 (sqlparser-sqlserver-split-three  (car tablename-string-list)));db
                              (nth 1 (sqlparser-sqlserver-split-three  (nth 1 tablename-string-list)));schema
                              (nth 1 (sqlparser-sqlserver-split-three  (nth 2  tablename-string-list)));table
                              column-prefix-string))))
    (mapcar 'car (sqlserver-query sql sqlparser-sqlserver-connection))))

(defun  sqlparser-sqlserver-column-candidates ()
  "column name candidates of table in current sql "
  (let* ((prefix (sqlparser-get-prefix-4-sqlserver)) ;alias.columnname or columnname
         (sub-prefix (split-string prefix "\\." nil)) ;(alias columnname)
         result)
    (cond ((= (length sub-prefix) 1);;columnname
            (let ((table-names (sqlparser-fetch-tablename-from-sql-4-sqlserver
                                (sqlparser-sql-sentence-at-point-4-sqlserver)))
                 )
              (dolist (tablename table-names)
                (setq result (append result    (sqlparser-sqlserver-get-matched-columns tablename (car sub-prefix))))
               )))
          ((= (length sub-prefix) 2); alias.columnname
           (let ((tablename-string  (sqlparser-guess-table-name-4-sqlserver (car sub-prefix))) ;[master].dbo.test
                )
             (setq result (sqlparser-sqlserver-get-matched-columns tablename-string (nth 1 sub-prefix)))))
         )
    result))

(defun sqlparser-sqlserver-all-procedures-candidates()
  (let* ((prefix (sqlparser-word-before-point-4-sqlserver)) ;
         sql)
    (if (> (length prefix) 0)
        (setq sql (format "SELECT name FROM sys.all_objects WHERE ([type] = 'P' OR [type] = 'X' OR [type] = 'PC') and  name like '%s%%'" prefix))
      (setq sql "SELECT name FROM sys.all_objects WHERE ([type] = 'P' OR [type] = 'X' OR [type] = 'PC')"))
    (mapcar 'car (sqlserver-query sql))))

(defun sqlparser-sqlserver-all-tablename-candidates()
  (mapcar 'car (sqlserver-query "select name from sys.tables" sqlparser-sqlserver-connection)))

(defun sqlparser-sqlserver-all-databasename-candidates()
  "for example :return (master msdb tempdb)"
  (mapcar 'car (sqlserver-query "select name from sys.databases" sqlparser-sqlserver-connection)))

;; master.dbo.tablename  databasename.schemaname.tablename
(defun sqlparser-sqlserver-schemaname-candidates()
  "for example :return (dbo sys db_owner)."
  (mapcar 'car (sqlserver-query "  select name from sys.schemas" sqlparser-sqlserver-connection)))

(defun sqlparser-guess-table-name-4-sqlserver (alias &optional sql1)
  "find out the true table name depends on the alias.
suppose the sql is `select * from user u where u.age=11'
then the `u' is `alias' and `user' is the true table name."
  (let ((sql  (or sql1 (sqlparser-sql-sentence-at-point-4-sqlserver)))
        (regexp (concat  "\\(\\([a-zA-Z0-9_\\$\\. ]\\|\\[\\|]\\)+\\)[ \t\n\r]+\\(as[ \t]+\\)?" alias "[, \t\n\r]\\|$"))
        table-name)
    (setq sql (concat sql " "))
    (setq sql  (replace-regexp-in-string "\n" " " sql))
    (if (and  sql (string-match regexp sql))
        (progn
          (setq table-name (match-string 1 sql))
          (if (string-equal "from" table-name) alias table-name))
      alias)))


(defun sqlparser-fetch-tablename-from-sql-4-sqlserver (&optional sql1)
  "return a list of tablenames from a sql-sentence."
  (let ((sql (or sql1 (sqlparser-sql-sentence-at-point-4-sqlserver)))
        tablenames)
    (setq tablenames (sqlparser-fetch-tablename-from-select-sql-4-sqlserver sql))
    (unless tablenames
      (setq tablenames (append tablenames (list (sqlparser-fetch-tablename-from-insert-update-alter-sql-4-sqlserver sql)))))
    tablenames
   ))
(defun sqlparser-fetch-tablename-from-insert-update-alter-sql-4-sqlserver(&optional sql1)
  "fetch tablename ,or schema.tablename from a insert sentence or
update sentence or alter sentence."
  (let ((sql (or sql1 (sqlparser-sql-sentence-at-point-4-sqlserver)))
        tablename)
    (with-temp-buffer
      (insert sql)
      (goto-char (point-min))
      (when (search-forward-regexp "\\(\\binto\\|update\\|table\\)[ \t\n\r]+\\(\\([a-zA-Z0-9_\\$\\. ]\\|\\[\\|]\\)+\\)[ \t\n\r]*" (point-max) t)
        (setq tablename (match-string 2))))))

(defun sqlparser-fetch-tablename-from-select-sql-4-sqlserver (&optional sql1)
  "return a list of tablenames from a sql-sentence."
  (let* ((sql (or sql1 (sqlparser-sql-sentence-at-point-4-sqlserver)))
         (sql-stack (list sql)) ele pt result-stack tablename-stack)
    (while (> (length sql-stack) 0)
      (setq ele (pop sql-stack))
      (with-temp-buffer
        (insert ele)
        (goto-char (point-min))
        (while (search-forward-regexp "[ \t\n\r]*(" (point-max) t)
          (forward-char -1)
          (setq pt (point))
          (forward-sexp)
          (push (buffer-substring (1+ pt)  (1- (point))) sql-stack)
          (delete-region  pt (point))
          (insert "table "))
        (push (buffer-substring (point-min) (point-max))  result-stack)
        ))
    (while (> (length result-stack) 0)
      (setq ele (pop result-stack))
      (with-temp-buffer
        (insert ele)
        (goto-char (point-min))
        (when (search-forward-regexp "\\(?:\\([a-zA-Z0-9_ ]+\\)[ \t\n\r]+\\)?\\(?:\\(?:inner[ \t\r\n]+\\|\\(?:\\(?:left\\|right\\)[ \t\r\n]+\\(?:outer[ \t\r\n]+\\)?\\)\\)join[ \t\n\r]+\\)\\([a-zA-Z0-9_ ]+\\)[ \t\r\n]+" (point-max) t)
          (push  (match-string 1) tablename-stack)
          (push  (match-string 2) tablename-stack)
          )
        (goto-char (point-min))
        (when  (search-forward-regexp "[ \t\n\r]+from[ \t\n\r]+" (point-max) t)
          (delete-region (point-min) (point))
          (when (search-forward-regexp "[ \t\n\r]+where[ \t\n\r]+" (point-max) t)
            (backward-word)
            (delete-region (point) (point-max)))
          (goto-char (point-min))
          (while (search-forward-regexp "," (point-max) t)
            (push (buffer-substring 1 (1- (point))) tablename-stack)
            (delete-region  1 (point))
            )
          (push (buffer-substring (point-min) (point-max)) tablename-stack)
          )
        )
      )
    (while (> (length tablename-stack) 0)
      (setq ele (pop tablename-stack))
      (with-temp-buffer
        (insert ele)
        (goto-char (point-min))
        (while (re-search-forward "\n" nil t)
          (replace-match " " nil nil))

        (goto-char (point-min))
        (while (re-search-forward "[ \t]+as[ \t]+" nil t)
          (replace-match " " nil nil))

        (goto-char (point-min))
        (delete-horizontal-space)
        (goto-char (point-max))
        (delete-horizontal-space)

        (if (= 1  (count-matches   "\\([a-zA-Z0-9_\\$\\. ]\\|\\[\\|]\\)+" 1 (point-max)))
            (push (buffer-substring 1 (point-max)) result-stack)
          (goto-char 0)
          (when (search-forward-regexp "\\([a-zA-Z0-9_\\$\\. ]\\|\\[\\|]\\)+" (point-max) t)
            (push (match-string 0) result-stack)
            )
          )
        )
      )
    (delete "table " result-stack)
    ))

(defun sqlparser-parse-4-sqlserver()
  "judge now need complete tablename or column name or don't complete .
it will return 'table' ,or 'column' ,or nil.
"
  (let* ((cur-pos (point))
         (sql-pos-info (bounds-of-sql-at-point-4-sqlserver))
         (sql-start-pos (car sql-pos-info))
         (sql-end-pos (cdr sql-pos-info))
         map keyword returnVal)
    (when (search-backward-regexp "\\balter\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "alter") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\btable\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "table") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\bcolumn\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "column") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\buse\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "use") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\bfrom\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "from") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\bupdate\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "update") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\bselect\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "select") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\bset\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "set") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\bwhere\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "where") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\bon\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "on") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\bhaving\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "having") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\bvalues\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "values") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\binto\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "into") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\border\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "order") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\bgroup\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "group") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\bexec\\b" sql-start-pos t 1)
      (push (list (- cur-pos (point)) "exec") map))
    (goto-char cur-pos)
    (setq map   (sort map (lambda (a b) (when (< (car a) (car b)) t))))
    (setq keyword  (car (cdar map)))
    (cond
     ((null keyword)
       (setq returnVal nil)
      )
     ((string= "into" keyword)
      (progn
        ;; '(' between "into" and current position
        (if (search-backward-regexp (regexp-quote "(") (- cur-pos  (caar map)) t 1)
            (setq returnVal "column")
          (setq returnVal "table")
         )
       )
     )
     ((string-match "use" keyword)
      (setq returnVal "database")
     )
     ((string-match "exec" keyword)
      (setq returnVal "procedure")
      )
     ((string-match "table\\|from\\|alter\\|update" keyword)
      (setq returnVal "table")
     )
     ((string-match "column\\|select\\|set\\|where\\|on\\|having\\|group\\|order" keyword)
      (setq returnVal "column")
     )
     ((string-match "values" keyword)
      (setq returnVal nil)
     )
     (t
      (setq returnVal nil)
     )
    )
    (goto-char cur-pos)
    returnVal
   ))

(defun sqlparser-sql-sentence-at-point-4-sqlserver()
  "get current sql sentence. "
  (let* ((bounds (bounds-of-sql-at-point-4-sqlserver))
         (beg (car bounds))
         (end (cdr bounds)))
    (buffer-substring-no-properties  beg end)
   ))

(defun bounds-of-sql-at-point-4-sqlserver()
  "get start and end point of current sql."
  (let ((pt (point))begin end empty-line-p empty-line-p next-line-included tail-p)
    (when (and
           (looking-at "[ \t]*\\(\n\\|\\'\\)")
           (looking-back "[ \t]*;[ \t]*" (beginning-of-line))
          )
      (search-backward-regexp "[ \t]*;[ \t]*" (beginning-of-line) t)
     )
    (save-excursion
      (skip-chars-forward " \t\n\r")
      (re-search-backward ";[ \t\n\r]*\\|\\`\\|\n[\r\t ]*\n[^ \t]" nil t)
      (setq begin (match-end 0)))
    (save-excursion
      (skip-chars-forward " \t\n\r")
      (re-search-forward "\n[\r\t ]*\n[^ \t]\\|\\'\\|[ \t\n\r]*;" nil t)
      (unless (zerop (length (match-string 0)))
        (backward-char 1))
      (skip-syntax-backward "-")
      (setq end   (point)))
    (goto-char pt)
    (cons begin end)
   )
 )

(defun sqlparser-get-prefix-4-sqlserver()
  "for example `tablename.col' `table.' `str'"
  (with-syntax-table (copy-syntax-table (syntax-table))
    (modify-syntax-entry ?.  "w");treat . as part of word
    (modify-syntax-entry ?[  "w");treat [ as part of word
    (modify-syntax-entry ?]  "w");treat ] as part of word
    (or (thing-at-point 'word) "")))

(defun sqlparser-word-before-point-4-sqlserver()
  "get word before current point or empty string."
  (with-syntax-table (copy-syntax-table (syntax-table))
    (modify-syntax-entry ?.  ".");treat . as punctuation character
    (modify-syntax-entry ?[  "w");treat [ as part of word
    (modify-syntax-entry ?]  "w");treat ] as part of word
    (or (thing-at-point 'word) "")))

(provide 'sqlparser-sqlserver-complete)
;;; sqlparser-sqlserver-complete.el ends here

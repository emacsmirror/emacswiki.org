;;; sqlparser-mysql-complete.el --- completing tablename,column name for mysql. -*- coding:utf-8 -*-

;; Copyright (C) 2011~2012 纪秀峰(Joseph) all rights reserved.

;; Created: 2011年07月21日 星期四 20时03分40秒
;; Last Updated: Joseph 2012-02-04 12:17:18 星期六
;; Version: 0.2.0
;; Author: 纪秀峰(Joseph)  jixiuf@gmail.com
;; Keywords: sql  mysql complete
;; Description:  completing tablename column for mysql when editing
;; URL:http://www.emacswiki.org/emacs/download/sqlparser-mysql-complete.el
;; https://github.com/jixiuf/sqlparser
;; screencast :http://screencast-repos.googlecode.com/files/emacs-sqlparse-mysql-complete.mkv.bz2
;; Compatibility: Test on Windows Xp ,and Linux
;;
;; Features  that be required by this library
;; `mysql-query.el'
;; http://www.emacswiki.org/emacs/download/mysql-query.el
;;

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
;; 1 after keyword 'use'   :complete schema name
;; 2 after keyword 'select' 'set' 'where'    :complete  columnname.
;; 3 after keyword 'alter', 'from' 'update' 'desc'  'show' : complete tablename
;; 4 after keyword 'into' and and there isn't a
;; "\\(" between 'into' and current postion :complete tablename
;; 4.1 after keyword 'into' but there is a "(" between 'into' and current
;; position  :complete columnname
;; 5 after keyword 'values'  :complete nothing.
;;

;;; Installation:
;;
;; 1 it required mysql-query.el you should download and add it to you load-path.
;; http://www.emacswiki.org/emacs/download/mysql-query.el

;; 2 add sqlparser-mysql-complete.el to you load-path
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;;      (require 'sqlparser-mysql-complete)
;;      (define-key sql-mode-map (quote [M-return]) 'sqlparser-mysql-complete)
;;      (define-key sql-interactive-mode-map  (quote [M-return]) 'sqlparser-mysql-complete)
;;  or
;;     (defadvice sql-mysql (around start-mysql-complete-minor-mode activate)
;;        "enable `mysql-complete-minor-mode' minor mode."
;;         ad-do-it
;;        (mysql-complete-minor-mode))
;;    (define-derived-mode mysql-mode sql-mode "mysql"
;;        "mysql mode"
;;        (mysql-complete-minor-mode))

;; `sqlparser-mysql-complete' default  bind on `TAB' in
;;   with `C-u' you can change the dbname
;;   with `C-uC-u' you can use another new mysql connection
;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `mysql-complete-minor-mode'
;;    mode for editing mysql script
;;  `sqlparser-mysql-complete'
;;    complete tablename or column name depending on current point position .
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:

(require 'mysql-query)
(require 'thingatpt)
(require 'anything nil t)

(defgroup sqlparser nil
  "SQL-PARSE"
  :group 'SQL)

(defvar mysql-connection-4-complete nil)

(defvar mysql-complete-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map  (quote [tab]) 'sqlparser-mysql-complete)
    map))

(defvar mysql-complete-minor-mode-hook nil)

;;;###autoload
(define-minor-mode mysql-complete-minor-mode
  "mode for editing mysql script"
  :lighter " mysqlC"
  :keymap mysql-complete-minor-mode-map
  :group 'sqlparser
  (if mysql-complete-minor-mode
      (run-hooks 'mysql-complete-minor-mode-hook)))

;;;###autoload
(defun sqlparser-mysql-complete(&optional arg)
  "complete tablename or column name depending on current point position .
when you first call this command ,it will ask you for the dbname ,user ,password
host and port. the info will be stored in `mysql-connection-4-complete'. it can be
reused . with `C-u' you can change the dbname.
with `C-uC-u' you can use another new mysql connection"
  (interactive "P")
  (when (or (and arg (> (prefix-numeric-value arg) 4))
            (null mysql-connection-4-complete))
    (setq mysql-connection-4-complete (call-interactively 'mysql-query-create-connection)))
  (when (and arg (= (prefix-numeric-value arg) 4) )
    (setcdr  (assoc 'dbname mysql-connection-4-complete)
             (completing-read (format  "dbname(default:%s):"  (cdr (assoc 'dbname mysql-connection-4-complete)))
                              (sqlparser-mysql-schemaname-candidates) nil
                              nil nil nil  (cdr (assoc 'dbname mysql-connection-4-complete)))))
  (let ((prefix  (sqlparser-word-before-point-4-mysql) )
        (init-pos (point))
        (candidates (sqlparser-mysql-context-candidates))
        last-mark)
    (if (= 1 (length candidates))
        (insert (car candidates))
    (insert (completing-read "complete:" candidates nil t prefix )))
    (setq last-mark (point-marker))
    (goto-char init-pos)
    (backward-delete-char (length prefix))
    (goto-char (marker-position last-mark))
    ))

(defun  sqlparser-mysql-context-candidates()
  "it will decide to complete tablename or columnname depend on
  current position."
  (let ((context (sqlparser-parse-4-mysql))
        candidats)
    ;;  (print context)
    (cond
     ((string= "schema" context)
      (setq candidats (sqlparser-mysql-schemaname-candidates))
      )
     ((string= "table" context)
      (setq candidats (sqlparser-mysql-tablename-or-schemaname-candidates))
      )
     ((string= "column" context)
      (setq candidats (  sqlparser-mysql-column-candidates))
      )
     ((null context)))
    candidats))

;; (setq ac-ignore-case t)
;; (ac-define-source mysql-all
;;   '((candidates . ( sqlparser-mysql-context-candidates ))
;;     (cache)))
;; (define-key sql-mode-map "\C-o" 'ac-complete-mysql-all)
(defun sqlparser-mysql-tablename-or-schemaname-candidates ()
  "is used to complete tablenames ,but sometimes you may
type in `schema.tablename'. so schemaname is considered as
candidats"
  ;;-s means use TAB as separate char . -N means don't print column name.
  (let* ((prefix (sqlparser-get-prefix-4-mysql))
         (sub-prefix (split-string prefix "\\." nil))
         (sql )
         )
    (if (> (length sub-prefix) 1)
        (setq sql (format
                   "select table_name from information_schema.tables where table_schema='%s' and table_name like '%s%%'"
                   (car sub-prefix) (nth 1 sub-prefix)))
      (setq sql (format
                 "select table_name as tablename from information_schema.tables where table_schema='%s' and table_name like '%s%%' union select concat( schema_name, '.') as tablename from information_schema.schemata where schema_name like '%s%%' "
                 (cdr (assoc 'dbname  mysql-connection-4-complete))
                 prefix prefix )))
    (mapcar 'car (mysql-query sql  mysql-connection-4-complete))))

(defun sqlparser-mysql-schemaname-candidates ()
  "all schema-name in mysql database"
  (mapcar 'car (mysql-query "SELECT SCHEMA_NAME FROM INFORMATION_SCHEMA.SCHEMATA"  mysql-connection-4-complete)))

(defun  sqlparser-mysql-column-candidates ()
  "column name candidates of table in current sql "
  (let* ((sql "select column_name from information_schema.columns where 1=0")
         (table-names (sqlparser-fetch-tablename-from-sql-4-mysql
                       (sqlparser-sql-sentence-at-point-4-mysql)))
         (prefix (sqlparser-get-prefix-4-mysql))
         (sub-prefix (split-string prefix "\\." nil))
         tablename tablenamelist schemaname )
    (if (> (length sub-prefix) 1);;alias.columnsname
        (progn
          (setq tablename (sqlparser-guess-table-name-4-mysql (car sub-prefix)))
          (setq tablenamelist (split-string tablename "[ \t\\.]" t))
          (if (= 1 (length tablenamelist)) ;;just tablename ,not dbname.tablename
              (progn
                (setq tablename (car tablenamelist))
                (setq schemaname nil)
                (setq sql (format "select column_name from information_schema.columns where table_name='%s' and column_name like '%s%%' "
                                  tablename (nth 1 sub-prefix))))
            (setq schemaname (car tablenamelist))
            (setq tablename (cadr tablenamelist))
            (setq sql (format "select column_name from information_schema.columns where table_schema ='%s' and  table_name='%s' and column_name like '%s%%'"
                              schemaname tablename (nth 1 sub-prefix)))
            ))
      (while (> (length table-names) 0)
        (setq tablename (pop table-names))
        (setq tablenamelist (split-string tablename "[ \t\\.]" t))
        (if (= 1 (length tablenamelist))
            (progn
              (setq tablename (car tablenamelist))
              (setq schemaname nil)
              (setq sql (format "%s union select column_name from information_schema.columns where table_name='%s' and column_name like '%s%%' " sql tablename prefix )))
          (setq tablename (cadr tablenamelist))
          (setq schemaname (car tablenamelist))
          (setq sql (format "%s union select column_name from information_schema.columns where table_name='%s' and table_schema='%s' and column_name like '%s%%' "
                            sql tablename schemaname prefix)))))
    (mapcar 'car (mysql-query sql  mysql-connection-4-complete))))

;; TEST :
;; (sqlparser-fetch-tablename-from-sql "select * from (select id from mysql.emp a , mysql.abc ad) ,abcd  as acd  where name=''")
;; (sqlparser-fetch-tablename-from-sql "update user set age=11 ")
;; (sqlparser-fetch-tablename-from-sql "alter  user add (tim datetime)")

(defun sqlparser-fetch-tablename-from-sql-4-mysql ( &optional sql1)
  "return a list of tablenames from a sql-sentence."
  (let ((sql (or sql1 (sqlparser-sql-sentence-at-point-4-mysql)))
        tablenames)
    (setq tablenames (sqlparser-fetch-tablename-from-select-sql-4-mysql sql))
    (unless tablenames
      (setq tablenames (append tablenames (list (sqlparser-fetch-tablename-from-insert-update-alter-sql-4-mysql sql)))))
    tablenames))

(defun sqlparser-fetch-tablename-from-insert-update-alter-sql-4-mysql( &optional sql1)
  "fetch tablename ,or schema.tablename from a insert sentence or
update sentence or alter sentence."
  (let ((sql (or sql1 (sqlparser-sql-sentence-at-point-4-mysql)))
        tablename)
    (with-temp-buffer
      (insert sql)
      (goto-char (point-min))
      (when (search-forward-regexp "\\(\\binto\\|update\\|table\\)[ \t]+\\([a-zA-Z0-9\\._]+\\)\\b" (point-max ) t)
        (setq tablename (match-string 2))))))

(defun sqlparser-fetch-tablename-from-select-sql-4-mysql ( &optional sql1)
  "return a list of tablenames from a sql-sentence."
  (let* ((sql (or sql1 (sqlparser-sql-sentence-at-point-4-mysql)))
         (sql-stack (list sql)) ele pt result-stack tablename-stack )
    (while (> (length sql-stack) 0)
      (setq ele (pop sql-stack))
      (with-temp-buffer
        (insert ele)
        (goto-char (point-min))
        (while (search-forward-regexp "[ \t]*(" (point-max) t)
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
        (when  (search-forward-regexp "[ \t]+from[ \t]+" (point-max) t)
          (delete-region (point-min) (point))
          (when (search-forward-regexp "[ \t]+where[ \t]+" (point-max) t)
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
        (while (re-search-forward "[ \t]+as[ \t]+"  nil t)
          (replace-match " " nil nil))
        (goto-char (point-min))
        (delete-horizontal-space)
        (goto-char (point-max))
        (delete-horizontal-space)

        (if (= 1  (count-matches  "[a-zA-Z0-9_\\. ]+" 1 (point-max)))
            (push (buffer-substring 1 (point-max)) result-stack)
          (goto-char 0)
          (when (search-forward-regexp "[a-zA-Z0-9_\\. ]+" (point-max) t )
            (push (match-string 0) result-stack)
            )
          )
        )
      )
    (delete "table " result-stack)
    result-stack
    ))

;; TEST :
;; (sqlparser-fetch-tablename-from-select-sql-4-mysql "select * from (select id from mysql.emp a , mysql.abc ad) ,abcd  as acd  where name=''")
(defun sqlparser-guess-table-name-4-mysql (alias &optional sql1)
  "find out the true table name depends on the alias.
suppose the sql is `select * from user u where u.age=11'
then the `u' is `alias' and `user' is the true table name."
  (let ((sql  (or sql1 (sqlparser-sql-sentence-at-point-4-mysql)))
        (regexp (concat  "\\(\\([a-zA-Z0-9_\\$\\. ]\\|\\[\\|]\\)+\\)[ \t\n\r]+\\(as[ \t]+\\)?" alias "[, \t\n\r]\\|$"))
        table-name)
    (setq sql (concat sql " "))
    (setq sql  (replace-regexp-in-string "\n" " " sql))
    (if (and  sql (string-match regexp sql))
        (progn
          (setq table-name (match-string 1 sql))
          (if (string-equal "from" table-name) alias table-name))
      alias))
  )

;; TEST :
;; (sqlparser-guess-table-name-4-mysql "a"   "select * from (select id from mysql.emp a , mysql.abc ad) ,abcd  as acd  where name=''")


;; (defun sql-mode-hook-fun()
;;   "change the `sentence-end'"
;;   (make-local-variable 'sentence-end)
;;   (make-local-variable 'sentence-end-without-space)
;;   (setq sentence-end nil)
;;   (setq sentence-end-without-space ";")

;;   )
;; (add-hook 'sql-mode-hook 'sql-mode-hook-fun)

(defun sqlparser-sql-sentence-at-point-4-mysql()
  "get current sql sentence. "
  (let* ((bounds (bounds-of-sql-at-point-4-mysql))
         (beg (car bounds))
         (end (cdr bounds)))
    (buffer-substring-no-properties  beg end)
    ))

(defun bounds-of-sql-at-point-4-mysql()
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
      (setq begin (point)))
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
;; 1 after keyword 'use'   :complete schema name
;; 2 after keyword 'alter', 'from' 'update' 'desc'  'show' : complete tablename
;; 3 after keyword 'select' 'set' 'where'    :complete  columnname.
;; 4 after keyword 'into' and and there isn't a
;; "\\(" between 'into' and current postion :complete tablename
;; 4.1 after keyword 'into' but there is a "(" between 'into' and current
;; position  :complete columnname
;; 5 after keyword 'values'  :complete nothing.
(defun sqlparser-parse-4-mysql()
  "judge now need complete tablename or column name or don't complete .
it will return 'table' ,or 'column' ,or nil.
"
  (let* ((cur-pos (point))
         (sql-pos-info (bounds-of-sql-at-point-4-mysql))
         (sql-start-pos (car sql-pos-info ))
         (sql-end-pos (cdr sql-pos-info))
         map keyword returnVal)
    (goto-char cur-pos)
    (when (search-backward-regexp "\\buse\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "use") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\btable\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "table") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\bcolumn\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "column") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\bfrom\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "from") map))
    (when (search-backward-regexp "\\bshow\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "show") map))
    (when (search-backward-regexp "\\bdesc\\b\\|\\bdescribe\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "desc") map))
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
    (when (search-backward-regexp "\\bgroup\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "group") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\bon\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "on") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\bhaving\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "having") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\border\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "order") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\bvalues\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "values") map))
    (goto-char cur-pos)
    (when (search-backward-regexp "\\binto\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "into") map))
    (goto-char cur-pos)
    (setq map   (sort map (lambda (a b ) (when (< (car a ) (car b)) t))))
    (setq keyword  (car (cdar map)))
    (cond
     ( (null keyword)
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
      (setq returnVal "schema")
      )
     ((string-match "table\\|from\\|alter\\|update\\|desc\\|describe\\|show" keyword)
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

(defun sqlparser-get-prefix-4-mysql()
  "for example `tablename.col' `table.' `str'"
  (with-syntax-table (copy-syntax-table (syntax-table))
    (modify-syntax-entry ?_  "w");treat . as part of word
    (modify-syntax-entry ?.  "w");treat . as part of word
    (or (thing-at-point 'word) ""))
  )

(defun sqlparser-word-before-point-4-mysql()
  "get word before current point or empty string."
  (with-syntax-table (copy-syntax-table (syntax-table))
    (modify-syntax-entry ?_  "w");treat . as part of word
    (modify-syntax-entry ?.  ".");treat . as punctuation character
    (or (thing-at-point 'word) ""))
  )


(provide 'sqlparser-mysql-complete)
;;; sqlparser-mysql-complete.el ends here



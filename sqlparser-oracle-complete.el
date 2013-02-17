;;; sqlparser-oracle-complete.el --- completing tablename,column name for oracle. -*- coding:utf-8 -*-

;; Copyright (C) 2011~2012 纪秀峰(Joseph) all rights reserved.

;; Created: 2011年07月31日 星期日 20时37分31秒
;; Last Updated: Joseph 2012-03-16 01:27:43 星期五
;; Version: 0.1.5
;; Author: 纪秀峰(Joseph)  jixiuf@gmail.com
;; Keywords: sql parse oracle complete
;; Filename: sqlparser-oracle-complete.el
;; Description:  completing tablename column for oracle when editing
;; URL:http://www.emacswiki.org/emacs/download/sqlparser-oracle-complete.el
;;     https://github.com/jixiuf/sqlparser
;; screencast:http://screencast-repos.googlecode.com/files/sqlparser0oracle0complete.mkv
;; Compatibility: Test on Linux
;;
;; Features  that be required by this library
;; `oracle-query.el'
;; http://www.emacswiki.org/emacs/download/oracle-query.el
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
;; 1 after keyword 'use'   :complete schema name (on mysql)
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
;; 1 it required oracle-query.el you should download and add it to you load-path.
;; http://www.emacswiki.org/emacs/download/oracle-query.el
;; 2 run  lsnrctl start

;; 3 add sqlparser-oracle-complete.el to you load-path
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;; (require 'sqlparser-oracle-complete)
;; (add-hook  'sql-mode-hook 'oracle-complete-minor-mode)
;; (add-hook  'sqlplus-mode-hook 'oracle-complete-minor-mode)
;; or you can call M-x: oracle-complete-minor-mode
;;  and complete command is binded on `TAB' .
;;  with `C-u' you can complete with new connection string.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `oracle-complete-minor-mode'
;;    mode for editing oracle script
;;  `sqlparser-oracle-complete'
;;    complete tablename or column name depending on current point
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:

(require 'oracle-query)
(require 'thingatpt)

(defvar oracle-complete-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map  (quote [tab]) 'sqlparser-oracle-complete)
    map))
(defvar  oracle-complete-minor-mode-hook nil)

;;;###autoload
(define-minor-mode oracle-complete-minor-mode
  "mode for editing oracle script"
  :lighter " OracleC"
  :keymap oracle-complete-minor-mode-map
  :group 'SQL
  (if oracle-complete-minor-mode
      (run-hooks 'oracle-complete-minor-mode-hook)))

(defvar sqlparser-sqlplus-connection nil)
;; (make-variable-buffer-local 'sqlparser-sqlplus-connection)

;;;###autoload
(defun sqlparser-oracle-complete(&optional arg)
  "complete tablename or column name depending on current point
position . with `C-u',use a new connection string to complete."
  (interactive "P")
  (let ((prefix  (sqlparser-word-before-point-4-oracle) )
        (init-pos (point))
        (candidates  (sqlparser-oracle-context-candidates arg))
        last-mark)
    (if (= 1 (length candidates))
        (insert (car candidates))
      (insert (completing-read "complete:" candidates nil t prefix )))
    (setq last-mark (point-marker))
    (goto-char init-pos)
    (backward-delete-char (length prefix))
    (goto-char (marker-position last-mark))
    ))

(defun  sqlparser-oracle-context-candidates(&optional arg)
  "it will decide to complete tablename or columnname depend on
  current position. with `C-u' use a new connection string to complete."
  (if arg
      (when (oracle-query-connection-alive-p sqlparser-sqlplus-connection)
        (oracle-query-close-connection  sqlparser-sqlplus-connection)
        (setq sqlparser-sqlplus-connection (call-interactively 'oracle-query-create-connection)))
    (unless  (oracle-query-connection-alive-p sqlparser-sqlplus-connection)
      (setq sqlparser-sqlplus-connection (call-interactively 'oracle-query-create-connection))))
  (let ((context (sqlparser-parse-4-oracle)) candidats)
    (cond
     ((string= "schema" context)
      (setq candidats (sqlparser-oracle-schemaname-candidates))
      )
     ((string= "table" context)
      (setq candidats (sqlparser-oracle-tablename-or-schemaname-candidates))
      )
     ((string= "column" context)
      (setq candidats (  sqlparser-oracle-column-candidates))
      )
     ((null context)))
    candidats))

;; elect view_name from all_views where owner = 'U1'
;; union all
;; select table_name from all_tables where owner = 'U1'
;; union all
;; select table_name from all_tab_privs where grantee = 'U1' and privilege = 'SELECT'
(defun  sqlparser-oracle-tablename-or-schemaname-candidates ()
  "all tablename viewname i can select."
  (let* ((prefix (sqlparser-get-prefix-4-oracle))
         (tmp-str-array (split-string prefix "\\."))
         owner table_name sql)
    (if  (= 2 (length tmp-str-array))
        (progn (setq owner (car tmp-str-array))
               (setq table_name (nth 1 tmp-str-array)))
      (setq table_name  (car tmp-str-array)))

    (if owner
        (setq sql (format
                   (concat " select view_name from all_views where upper(owner)=upper('%s') and upper(view_name) like upper('%s%%') "
                           "union all select table_name from all_tables where upper(owner)=upper('%s') and upper(table_name) like upper('%s%%') "
                           "union all  select table_schema||'.'||table_name from all_tab_privs where lower(grantee) = lower('%s') and privilege = 'SELECT' and upper(table_schema)=upper('%s') and upper(table_name) like upper('%s%%')"
                           "union all select table_name from dict where upper(table_name) like upper('%s%%')")

                   (nth 2 sqlparser-sqlplus-connection) table_name
                   (nth 2 sqlparser-sqlplus-connection) table_name
                   (nth 2 sqlparser-sqlplus-connection) owner table_name
                   table_name
                   ))
      (setq sql (format
                 (concat " select view_name from all_views where upper(owner)=upper('%s') and upper(view_name) like upper('%s%%') "
                         "union all select table_name from all_tables where upper(owner)=upper('%s') and upper(table_name) like upper('%s%%') "
                         "union all  select table_schema||'.'||table_name from all_tab_privs where lower(grantee) = lower('%s') and privilege = 'SELECT' and upper(table_name) like upper('%s%%')"
                         "union all select table_name from dict where upper(table_name) like upper('%s%%')"
                         "union all select USERNAME from ALL_USERS where upper(USERNAME) like upper('%s%%')"
                         )

                 (nth 2 sqlparser-sqlplus-connection) table_name
                 (nth 2 sqlparser-sqlplus-connection) table_name
                 (nth 2 sqlparser-sqlplus-connection) owner table_name
                 table_name
                 table_name           ;actual ,maybe username
                 ))
      )
    (mapcar 'car
            (oracle-query
             sql
             sqlparser-sqlplus-connection
             ))
    )
  )

(defun sqlparser-oracle-schemaname-candidates ()
  "all schema-name in oracle database"
  (mapcar 'car
          (oracle-query
            (format "select table_schema from all_tab_privs where lower(grantee) = lower('%s') and privilege = 'SELECT'"
                   (nth 2 sqlparser-sqlplus-connection))
           sqlparser-sqlplus-connection
           )))

(defun sqlparser-oracle-column-candidates ()
  "column name candidates of table in current sql "
  (let* ((sql "select column_name from user_tab_columns where 1=0")
         (table-names (sqlparser-fetch-tablename-from-sql-4-oracle
                       (sqlparser-sql-sentence-at-point-4-oracle)))
         (prefix (sqlparser-get-prefix-4-oracle))
         (sub-prefix (split-string prefix "\\." nil))
         tablename tablenamelist schemaname )
    (if (> (length sub-prefix) 1);;alias.columnsname
        (progn
          (setq tablename (sqlparser-guess-table-name-4-oracle (car sub-prefix)))
          (setq tablenamelist (split-string tablename "[ \t\\.]" t))
          (if (= 1 (length tablenamelist)) ;;just tablename ,not dbname.tablename
              (progn
                (setq tablename (car tablenamelist))
                (setq schemaname nil)
                (setq sql (format " select column_name from all_tab_columns  where upper(table_name)=upper('%s') and upper(column_name) like upper('%s%%') "
                                  tablename (nth 1 sub-prefix) ) ))
            (setq schemaname (car tablenamelist))
            (setq tablename (cadr tablenamelist))
            (setq sql (format "select column_name from all_tab_columns where upper(owner)=upper('%s') and upper(table_name) =upper('%s') and upper(column_name) like upper('%s%%')"
                              schemaname tablename (nth 1 sub-prefix)))
            ))
      (while (> (length table-names) 0)
        (setq tablename (pop table-names))
        (setq tablenamelist (split-string tablename "[ \t\\.]" t))
        (if (= 1 (length tablenamelist))
            (progn
              (setq tablename (car tablenamelist))
              (setq schemaname nil)
              (setq sql (format "%s union select column_name from all_tab_columns where upper(table_name)=upper('%s') and upper(column_name) like upper('%s%%') " sql tablename prefix ))
              )
          (setq tablename (cadr tablenamelist))
          (setq schemaname (car tablenamelist))
          (setq sql (format "%s union select column_name from all_tab_columns where upper(table_name)=upper('%s') and upper(owner)=upper('%s') and upper(column_name) like upper('%s%%') "
                            sql tablename schemaname prefix)))))
    (mapcar 'car (oracle-query sql sqlparser-sqlplus-connection))))

;; TEST :
;; (sqlparser-fetch-tablename-from-sql-4-oracle "select * from (select id from oracle.emp a , oracle.abc ad) ,abcd  as acd  where name=''")
;; (sqlparser-fetch-tablename-from-sql-4-oracle "update user set age=11 ")
;; (sqlparser-fetch-tablename-from-sql-4-oracle "alter  user add (tim datetime)")

(defun sqlparser-fetch-tablename-from-sql-4-oracle ( &optional sql1)
  "return a list of tablenames from a sql-sentence."
  (let ((sql (or sql1 (sqlparser-sql-sentence-at-point-4-oracle)))
        tablenames)
    (setq tablenames (sqlparser-fetch-tablename-from-select-sql-4-oracle sql))
    (unless tablenames
      (setq tablenames (append tablenames (list (sqlparser-fetch-tablename-from-insert-update-alter-sql-4-oracle sql)))))
    tablenames
    ))

(defun sqlparser-fetch-tablename-from-insert-update-alter-sql-4-oracle( &optional sql1)
  "fetch tablename ,or schema.tablename from a insert sentence or
update sentence or alter sentence."
  (let ((sql (or sql1 (sqlparser-sql-sentence-at-point-4-oracle)))
        tablename)
    (with-temp-buffer
      (insert sql)
      (goto-char (point-min))
      (when (search-forward-regexp "\\(\\binto\\|update\\|table\\)[ \t]+\\([a-zA-Z0-9\\$\\._]+\\)\\b" (point-max ) t)
        (setq tablename (match-string 2))
        )
      )))

(defun sqlparser-fetch-tablename-from-select-sql-4-oracle ( &optional sql1)
  "return a list of tablenames from a sql-sentence."
  (let* ((sql (or sql1 (sqlparser-sql-sentence-at-point-4-oracle)))
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

        (if (= 1  (count-matches  "[a-zA-Z0-9_\\$\\. ]+" 1 (point-max)))
            (push (buffer-substring 1 (point-max)) result-stack)
          (goto-char 0)
          (when (search-forward-regexp "[a-zA-Z0-9_\\$\\. ]+" (point-max) t )
            (push (match-string 0) result-stack)
            )
          )
        )
      )
    (delete "table " result-stack)
    result-stack
    ))

;; TEST :
;; (sqlparser-fetch-tablename-from-select-sql-4-oracle "select * from (select id from oracle.emp a , oracle.abc ad) ,abcd  as acd  where name=''")


(defun sqlparser-guess-table-name-4-oracle (alias &optional sql1)
  "find out the true table name depends on the alias.
suppose the sql is `select * from user u where u.age=11'
then the `u' is `alias' and `user' is the true table name."
  (let ((sql  (or sql1 (sqlparser-sql-sentence-at-point-4-oracle)))
        (regexp (concat  "\\([a-zA-Z0-9_\\.\\$]+\\)[ \t]+\\(as[ \t]+\\)?" alias "[, \t\n\r]\\|$"))
        table-name)
    (setq sql (concat sql " "))
    (if (and  sql (string-match regexp sql))
        (progn
          (setq table-name (match-string 1 sql))
          (if (string-equal "from" table-name) alias table-name))
      alias)
    ))
;; TEST :
;; (sqlparser-guess-table-name "a"   "select * from (select id from oracle.emp a , oracle.abc ad) ,abcd  as acd  where name=''")

(defun sqlparser-sql-sentence-at-point-4-oracle()
  "get current sql sentence. "
  (let* ((bounds (bounds-of-sql-at-point-4-oracle))
         (beg (car bounds))
         (end (cdr bounds)))
    (buffer-substring-no-properties  beg end)
    ))

(defun bounds-of-sql-at-point-4-oracle()
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

;; 1 after keyword 'use'   :complete schema name
;; 2 after keyword 'alter', 'from' 'update' 'desc'  'show' : complete tablename
;; 3 after keyword 'select' 'set' 'where'    :complete  columnname.
;; 4 after keyword 'into' and and there isn't a
;; "\\(" between 'into' and current postion :complete tablename
;; 4.1 after keyword 'into' but there is a "(" between 'into' and current
;; position  :complete columnname
;; 5 after keyword 'values'  :complete nothing.
(defun sqlparser-parse-4-oracle()
  "judge now need complete tablename or column name or don't complete .
it will return 'table' ,or 'column' ,or nil.
"
  (let* ((cur-pos (point))
         (sql-pos-info (bounds-of-sql-at-point-4-oracle))
         (sql-start-pos (car sql-pos-info ))
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
    (when (search-backward-regexp "\\bfrom\\b" sql-start-pos t 1)
      (push   (list (- cur-pos (point)) "from") map))
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
     ((string-match "table\\|from\\|update\\|desc\\|describe" keyword)
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


(defun sqlparser-get-prefix-4-oracle()
  "for example `tablename.col' `table.' `str'"
  (with-syntax-table (copy-syntax-table (syntax-table))
    (modify-syntax-entry ?.  "w");treat . as part of word
    (or (thing-at-point 'word) ""))
  )

(defun sqlparser-word-before-point-4-oracle()
  "get word before current point or empty string."
  (with-syntax-table (copy-syntax-table (syntax-table))
    (modify-syntax-entry ?.  ".");treat . as punctuation character
    (or (thing-at-point 'word) ""))
  )


(provide 'sqlparser-oracle-complete)
;;; sqlparser-oracle-complete.el ends here

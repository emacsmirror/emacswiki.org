;;; sqlserver-query.el --- execute sql select using sqlcmd.exe or osql.exe on SQL SERVER. -*- coding:utf-8 -*-

;; Copyright (C) 2011~2012 纪秀峰(Joseph) all rights reserved.

;; Created: 2011年08月17日 星期三 22时11分54秒
;; Last Updated: 纪秀峰 2013-02-17 20:50:54 星期日
;; Version: 0.1.4
;; Author: 纪秀峰(Joseph) jixiuf@gmail.com
;; Keywords: sqlserver emacs sql sqlcmd.exe osql.exe
;; Filename: sqlserver-query.el
;; Description:  execute sql select using sqlcmd.exe or osql.exe on SQL SERVER
;; URL:http://www.emacswiki.org/emacs/down/sqlserver-query.el
;;     https://github.com/jixiuf/sqlparser

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

;;  execute sql using sqlcmd.exe or osql.exe and return as list .
;;  (sqlcmd.exe is recommended. ) (osql.exe is slow)

;; (sqlserver-query "select empno,ename from emp where empno<=7499")
;; got : (("7369" "SMITH") ("7499" "ALLEN"))
;; (sqlserver-query-with-heading "select empno,ename from emp where empno<=7499")
;; got : (("empno","ename")("7369" "SMITH") ("7499" "ALLEN"))
;; using default connection ,not recommended.

;;
;; 1. you should custom these variable
;; `sqlserver-connection-info'
;; `sqlserver-cmd' ;sqlcmd or osql
;; `sqlserver-command-path' (not needn't if `sqlserver-cmd' in under your PATH)
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

;; (defvar c (sqlserver-query-create-connection sqlserver-connection-info))
;; (sqlserver-query "select empno from emp" c)
;; (sqlserver-query-with-heading "select empno from emp" c)
;; recommended
;;
;; the normal way to use sqlserver-query.el is :
;; 1:
;; (defvar c nil)
;; (unless (sqlserver-query-connection-alive-p c)
;;   (setq c (call-interactively 'sqlserver-query-create-connection)))
;; 2:
;;   (sqlserver-query "select empno from emp" c)
;;   or
;;   (sqlserver-query-with-heading "select empno from emp" c)
;; 3:
;;   (sqlserver-query-close-connection c)

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `sqlserver-query-create-connection'
;;    open connection with sqlcmd.exe or osql.exe.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `sqlserver-connection-info'
;;    sqlserver connection info .
;;    default = (quote ((username . "sa") (password . "sa") (server-instance . "localhost\\SQLEXPRESS") (dbname . "master")))
;;  `sqlserver-cmd'
;;    sqlserver-cmd  now  support sqlcmd.exe and osql.exe
;;    default = (quote sqlcmd)
;;  `sqlserver-command-path'
;;    path (without filename) of the sqlcmd.exe or osql.exe .
;;    default = nil



;;; Code:


(defgroup sqlserver-query nil
  "SQL SERVER QUERY"
  :group 'SQL)

(defcustom sqlserver-connection-info
  '((username . "sa")
    (password . "sa")
    (server-instance . "localhost\\SQLEXPRESS")
    (dbname . "master"))
  "sqlserver connection info ."
  :group 'sqlserver-query
  :type 'alist)
;; (make-variable-buffer-local 'sqlserver-connection-info)

(defcustom sqlserver-cmd 'sqlcmd
  "sqlserver-cmd  now  support sqlcmd.exe and osql.exe
sqlserver 2005 add new cmd sqlcmd.exe. and osql.exe is not recommended."
  :type '(choice (const sqlcmd) (const osql))
  :group 'sqlserver-query)

(defcustom  sqlserver-command-path nil
  "path (without filename) of the sqlcmd.exe or osql.exe .
eg \"c:\\Program Files\\Microsoft SQL Server\\100\\Tools\\Binn\"
If you leave it nil, it will search the path for the executable."
  :type 'string
  :group 'sqlserver-query)

(defvar sqlserver-timeout-wait-for-result 300
  "waiting 300s for sql result returned.")
(defvar sqlserver-query-default-connection nil)

(defun sqlserver-parse-result-as-list-4-osql (raw-result)
  (let  (result row line-count line)
    (with-temp-buffer
      (insert raw-result)
      (goto-char  (point-min))
      (when (string-match  "(.*\\(行受影响\\|rows affected\\))"
                           (buffer-substring-no-properties
                            (line-beginning-position)(line-end-position)))
        (delete-region (line-beginning-position) (line-beginning-position 2)))
      (when (re-search-forward "(.*\\(行受影响\\|rows affected\\))")
        (delete-region (match-beginning 0)(point-max))
        )
      (while (re-search-forward "[ \t\n]*\^E[ \t\n]*" nil t)
        (replace-match "\^E" nil nil))
      (goto-char  (point-min))
      (while (re-search-forward "\\([ \t]+$\\|^[ \t]+\\)" nil t)
        (replace-match "" nil nil))
      (setq line-count (count-lines (point-min) (point-max)))
      (goto-char  (point-min))
      (while (< (line-number-at-pos)  line-count )
        (setq line  (buffer-substring-no-properties
                     (point-at-bol) (point-at-eol)))
        (unless (string-match "^[ \t]*$" line)
          (setq row (split-string line "\^E" t))
          (when row (setq result (append result (list row)))))
        (forward-line)))
    (when (and result (> (length result) 1)
               (not (string-match "^-+$" (caar result))))
      (setcdr result (cddr result))  )
    result ))

(defun sqlserver-parse-result-as-list-4-sqlcmd (raw-result)
  (let  (result row line-count)
    (with-temp-buffer
      (insert raw-result)
      (setq line-count (count-lines (point-min) (point-max)))
      (goto-char  (point-min))
      (while (<= (line-number-at-pos) line-count)
        (setq row (split-string (buffer-substring-no-properties
                                 (point-at-bol) (point-at-eol)) "\^E" t))
        (setq result (append result (list row)))
        (forward-line)))
    (when (and result (> (length result) 1))
      (setcdr result (cddr result))  )
    result))

(defun sqlserver-parse-result-as-list (raw-result)
  (if (equal sqlserver-cmd 'sqlcmd)
      (sqlserver-parse-result-as-list-4-sqlcmd raw-result)
    (sqlserver-parse-result-as-list-4-osql raw-result)))

(defun sqlserver-command-exepath ()
  (if (null sqlserver-command-path)
      (concat (symbol-name sqlserver-cmd) ".exe")
    (expand-file-name
     (concat (symbol-name sqlserver-cmd) ".exe")
     sqlserver-command-path)
    ))

(defun sqlserver-format-command-args(connection-info)
  "Returns a list of all the arguments for the sqlcmd.exe or osql.exe program.
  default: -S localhost\\SQLEXPRESS -U sa -P sa -d master -h-1 -w 65535   -s \"\^E\" -W
"
  (apply 'list "-S" (cdr (assoc 'server-instance connection-info))
         "-U" (cdr (assoc 'username connection-info))
         "-P" (cdr (assoc 'password connection-info))
         "-d" (cdr (assoc 'dbname connection-info))
         "-w" "65535"
         "-s"  "\^E"    ;; should be actual ^E, not ^ followed by E
         (if (equal sqlserver-cmd 'sqlcmd)
             (list "-W")
           (list "-n" "-r" "1")
           )
         ))

(defun sqlserver-query-read-connect-string()
  "set server dbname username password interactive"
  (let ((connection-info (copy-alist sqlserver-connection-info)))
    (setcdr  (assoc 'username connection-info)
             (read-string (format  "username(default:%s):"  (cdr (assoc 'username connection-info)))
                          "" nil   (cdr (assoc 'username connection-info))))
    (setcdr  (assoc 'password connection-info)
             (read-passwd "password:"  nil ))
    (setcdr  (assoc 'server-instance connection-info)
             (read-string (format  "server-instance(default:%s):"  (cdr (assoc 'server-instance connection-info)))
                          "" nil (cdr (assoc 'server-instance connection-info))))
    (setcdr  (assoc 'dbname connection-info)
             (read-string (format  "dbname(default:%s):"  (cdr (assoc 'dbname connection-info)))
                          "" nil (cdr (assoc 'dbname connection-info))))
    (setq-default sqlserver-connection-info connection-info) ;;update default info
    connection-info))

;; thanks dino chiesa <dpchiesa@hotmail.com> for
;; changed things - used start-process instead of start-process-shell-command
;; (sqlserver-query-create-connection sqlserver-connection-info)
(defun sqlserver-query-create-connection(connection-info)
  "open connection with sqlcmd.exe or osql.exe."
  (interactive (list (sqlserver-query-read-connect-string)))
  (let ((process (apply 'start-process ;; dino
                        (symbol-name sqlserver-cmd)
                        (concat " *sqlserver-query-" (number-to-string (random)) "*")
                        (sqlserver-command-exepath)
                        (sqlserver-format-command-args connection-info))))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel
     process
     (lambda (proc change)
       (when (string-match "\\(finished\\|exited\\|exited abnormally with code\\)" change)
         (kill-buffer (process-buffer proc) )
         (message  (concat  (process-name proc) " exited")))))
    (list process
          (process-buffer process)
          connection-info)))

(defun sqlserver-query-connection-alive-p(connection)
  "test whether the connection is alive."
  (and connection
       (listp connection)
       (processp (car connection))
       (bufferp (nth 1 connection))
       (buffer-live-p (nth 1 connection))
       (equal (process-status (car connection)) 'run)))

;;;###autoload
(defun sqlserver-query-close-connection(connection)
  "close connection.kill sqlserver process and buffer ."
  (when (sqlserver-query-connection-alive-p connection)
    (process-send-string (car connection)  "exit\n"))
  (sleep-for 0.1)
  (when (sqlserver-query-connection-alive-p connection)
    (kill-process (car connection))
    (kill-buffer (nth 1 connection)))
  )

;;(sqlserver-query "select empno from emp")
;;;###autoload
(defun sqlserver-query-with-heading (sql &optional sqlserver-query-connection)
  "execute sql using `sqlcmd' or `osql' ,and return the result of it.
the `car' of result is heading"
  (let(  process)
    (unless (sqlserver-query-connection-alive-p sqlserver-query-connection)
      (setq-default sqlserver-query-default-connection (call-interactively 'sqlserver-query-create-connection))
      (setq sqlserver-query-connection sqlserver-query-default-connection))
    (setq process (car sqlserver-query-connection))
    (when (string-match "\\(.*\\);[ \t]*" sql) (setq sql (match-string 1 sql)))
    (with-current-buffer (process-buffer process)
      (delete-region (point-min) (point-max))
      (let ((start (point-min))
            (end-flg-regexp (if (equal sqlserver-cmd 'osql) "1234!@#end-flg&%~`4321" "([0-9]+ \\(行受影响\\|rows? affected\\))" ))
            end)
        (goto-char (point-max))
        (process-send-string process (format "%s ;\n" sql))
        (process-send-string process  "go\n")
        (when (equal sqlserver-cmd 'osql)
          (process-send-string process "select '1234!@#end-flg&%~`4321' \n")
          (process-send-string process  "go\n")
          )
        (goto-char (point-min))
        (while (not (re-search-forward end-flg-regexp nil t 1))
          (when (accept-process-output process  sqlserver-timeout-wait-for-result 0 nil)
            (goto-char (point-min))))
        (setq end (max start  (1- (match-beginning 0))))
        (sqlserver-parse-result-as-list  (buffer-substring-no-properties start end))
        ))))

;;;###autoload
(defun sqlserver-query(sql &optional sqlserver-query-connection)
  "execute sql using `sqlcmd' or `osql' ,and return the result of it."
  (cdr (sqlserver-query-with-heading sql sqlserver-query-connection)))

(provide 'sqlserver-query)
;;; sqlserver-query.el ends here

;;; oracle-query.el --- execute sql select using sqlplus. -*- coding:utf-8 -*-

;; Copyright (C) 2011~2012 纪秀峰(Joseph)

;; Last Updated: Joseph 2012-01-15 21:33:39 星期日
;; Created: 2011-7-31
;; Version: 0.1.4
;; Author: 纪秀峰(Joseph)  jixiuf@gmail.com
;; Keywords: oracle sql emacs
;; Filename: oracle-query.el
;; Description:execute sql select using sqlplus.
;; URL:http://www.emacswiki.org/emacs/download/oracle-query.el
;; https://github.com/jixiuf/sqlparser
;; Compatibility: Test on Linux

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; execute sql using sqlplus and return as list .
;;
;;       (oracle-query "select empno,ename from emp where empno<=7499")
;; got :     (("7369" "SMITH") ("7499" "ALLEN"))
;; or    (oracle-query-with-heading)
;; got :     (("EMPNO" "ENAME") ("7369" "SMITH") ("7499" "ALLEN"))
;; using default connection ,not recommended.
;;
;; (defvar connection (oracle-query-create-connection "scott/tiger"))
;; (oracle-query "select empno from emp" connection)
;; recommended
;;
;; the normal way to use oracle-query.el is :
;; 1:
;; (defvar sqlplus-connection nil)
;; (unless (oracle-query-connection-alive-p c)
;;   (setq sqlplus-connection (call-interactively 'oracle-query-create-connection)))
;;
;; 2:
;;   (oracle-query "select empno from emp" sqlplus-connection)
;; 3:
;;   (oracle-query-close-connection sqlplus-connection)

;;
;;; Commands:
;;
;; Below are complete command list:
;;
;;  `oracle-query-create-connection'
;;    create a connection to oracle using sqlplus ,and return the
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:

(defvar oracle-query-default-connection nil)

(defvar oq-timeout-wait-for-result 300
  "waiting 300s for sql result returned.")

(defvar oq-linesize 20000 "Default linesize for sqlplus")
(defvar oq-pagesize 50000 "Default pagesize for sqlplus")

;; (oracle-query-fetch-username-from-connect-string "scott/tiger")
(defun oracle-query-fetch-username-from-connect-string(connect-string)
  (when (string-match "\\(.*\\)/.*" connect-string)
    (match-string 1 connect-string)))


(defun oq-parse-result-as-list (raw-result)
  (let (result row index-of-result oracle-query-heading)
    (with-temp-buffer
      (insert raw-result)
      (goto-char (point-min))
      (while (re-search-forward "[ \t\n]* [ \t\n]*" nil t)
        (replace-match " " nil nil))
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]+" nil t)
        (replace-match "" nil nil))
      (goto-char (point-min))
      (forward-line 1)
      (setq oracle-query-heading
            (split-string (buffer-substring-no-properties
                           (point-at-bol) (point-at-eol)) " " t))
      (forward-line 2) (setq index-of-result 3)
      (while (not (= (point-at-eol) (point-max)))
        (unless (or (= 1  (% index-of-result oq-pagesize))
                    (= 2  (% index-of-result oq-pagesize))
                    (= 0 (% index-of-result oq-pagesize)))
          (setq row (split-string (buffer-substring-no-properties
                                   (point-at-bol) (point-at-eol)) " " t))
          (setq result (append result (list row))))
        (forward-line) (beginning-of-line)
        (setq index-of-result (1+ index-of-result)))
      )(cons oracle-query-heading result)))

;; (defun oq-build-connection-string()
;;   " default:sqlplus scott/tiger@localhost:1521/orcl"
;;   (format "sqlplus %s/%s@%s:%s/%s %s"
;;           oq-username oq-password oq-server oq-port oq-dbname (if oq-as-sysdba "as sysdba" "")))
(defun oracle-query-read-connect-string ()
  (let( (connection-string  (read-string "SqlPlus Connect String:" "" nil)))
    (list connection-string)))


;; (oracle-query-create-connection "scott/tiger")
;; (oracle-query-create-connection "scott/tiger@localhost:1521/orcl")
;; (oracle-query-create-connection "system/root as sysdba")
;;;###autoload
(defun oracle-query-create-connection(connect-string)
  "create a connection to oracle using sqlplus ,and return the
created process"
  (interactive (oracle-query-read-connect-string))
  (let ((oracle-query-process (start-process
                               "sqlplus"
                               (concat " *oracle-query-"
                                       (oracle-query-fetch-username-from-connect-string connect-string) "-"
                                       (number-to-string (random)) "*")
                               "sqlplus" connect-string)))
    (process-send-string oracle-query-process "set heading on;\n")
    (process-send-string oracle-query-process (format "set linesize %d;\n" oq-linesize))
    (process-send-string oracle-query-process "set colsep ' ';\n");;column separater
    (process-send-string oracle-query-process "set null 'NULL';\n");;
    (process-send-string oracle-query-process "set wrap off;\n")
    (process-send-string oracle-query-process (format "set pagesize %s;\n" oq-pagesize))
    (process-send-string oracle-query-process "set feedback on;\n")
    (process-send-string oracle-query-process "set serveroutput on;\n")
    (set-process-query-on-exit-flag  oracle-query-process nil)
    (set-process-sentinel oracle-query-process
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\|exited abnormally with code\\)" change)
                              (message  (concat  (process-name proc) "closed"))
                              (kill-buffer  (process-buffer proc))
                              )))
    ;; (set-process-filter oracle-query-process 'oq-filter-fun)
    (list oracle-query-process
          (process-buffer oracle-query-process)
          (oracle-query-fetch-username-from-connect-string connect-string))
    ))

(defun oracle-query-connection-alive-p(connection)
  "test whether the connection is alive."
  (and connection
       (listp connection)
       (processp (car connection))
       (bufferp (nth 1 connection))
       (buffer-live-p (nth 1 connection))
       (equal (process-status (car connection)) 'run)))

;;;###autoload
(defun oracle-query-close-connection(connection)
  "close connection.kill oracle process and buffer ."
  (when (oracle-query-connection-alive-p connection)
    (process-send-string (car connection)  "exit\n"))
  (sleep-for 0.1)
  (when (oracle-query-connection-alive-p connection)
    (kill-process (car connection))
    (kill-buffer (nth 1 connection)))
  )


;;(oracle-query "select empno from emp")
;; (oracle-query "select empno from emp" (oracle-query-create-connection "scott/tiger"))
;;;###autoload
(defun oracle-query (sql &optional oracle-query-connection)
  "execute sql using `sqlplus' ,and return the result of it."
  (cdr (oracle-query-with-heading sql oracle-query-connection)))


(defun oracle-query-with-heading (sql &optional oracle-query-connection)
  "execute sql using `sqlplus' ,and return the result of it."
  (let( (connection oracle-query-connection) process)
    (unless connection
      (unless (and oracle-query-default-connection
                   (buffer-live-p (nth 1 oracle-query-default-connection))
                   (equal (process-status (nth 0  oracle-query-default-connection)) 'run)
                   )
        (setq oracle-query-default-connection (call-interactively 'oracle-query-create-connection)))
      (setq connection oracle-query-default-connection)
      )
    (setq process (car connection))
    (when (string-match "\\(.*\\);[ \t\n\r]*" sql)
      (setq sql (match-string 1 sql)))
    (setq sql (replace-regexp-in-string "[\n\r]" " " sql))
    (with-current-buffer (process-buffer process)
      (delete-region (point-min) (point-max))
      (let ((start (point-min)) end)
        (goto-char (point-max))
        (process-send-string process (format "%s ;\n" sql))
        (goto-char (point-min))
        (while (not (re-search-forward "^\\([0-9]+\\|no\\) rows? \\(selected\\|updated\\|deleted\\)" nil t 1))
          (when (accept-process-output process oq-timeout-wait-for-result 0 nil)
            (goto-char (point-min))))
        (if (not  (string=  (match-string 2) "selected"))
            (match-string 0)
          (setq end (1- (match-beginning 0)))
          (when (re-search-backward "\\bSQL> " nil t 1) (setq start (match-end 0)))
          (oq-parse-result-as-list (buffer-substring start end)))))))


(provide 'oracle-query)
;;; oracle-query.el ends here

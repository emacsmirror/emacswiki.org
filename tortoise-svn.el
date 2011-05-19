;;; tortoise-svn.el

;; SEE ALSO http://tortoisesvn.net/docs/release/TortoiseSVN_en/tsvn-automation.html
;;     AND  http://www.emacswiki.org/emacs/tortoise-svn.el
;;; License:
;; This is free software

;;; Commentary:
;; TortoiseSVN's front end.
;; USAGE:
;; (require 'tortoise-svn)
;; (add-to-list 'exec-path "D:/Soft/TortoiseSVN/bin/")
;;                          -------------------------
;;                          


(defun tortoise-command (command filename)
  (start-process-shell-command "tortoise-svn" nil
   (concat "TortoiseProc /command:" command " "
	   "/path:\"" (expand-file-name filename) "\"")))

(defun tortoise-svn-log ()
  (interactive)
  (tortoise-svn-log-select (or (buffer-file-name) default-directory)))

(defun tortoise-svn-log-select (filename &optional wildcards)
  (interactive (find-file-read-args "Find file: " t))
  (tortoise-command "log" filename))

(defun tortoise-svn-diff ()
  (interactive)
  (tortoise-command "diff" (or (buffer-file-name) default-directory)))

(defun tortoise-svn-blame ()
 (interactive)
 (tortoise-command "blame" (or (buffer-file-name) default-directory)))

(defun tortoise-svn-commit ()		; add log?
  (interactive)
  (tortoise-svn-commit-select (or (buffer-file-name) default-directory)))

(defun tortoise-svn-commit-select (filename &optional wildcards)
  (interactive (find-file-read-args "Find file: " t))
  (tortoise-command "commit" (or (buffer-file-name) default-directory)))

(defun tortoise-svn-repostatus ()
  (interactive)
  (tortoise-svn-repostatus-select (or (buffer-file-name) default-directory)))

(defun tortoise-svn-repostatus-select (filename &optional wildcards)
  (interactive (find-file-read-args "Find file: " t))
  (tortoise-command "repostatus" filename))

(defun tortoise-svn-repobrowser ()
  (interactive)
  (tortoise-command "repobrowser" (or (buffer-file-name) default-directory)))

(defun tortoise-svn-revert ()
  (interactive)
  (tortoise-svn-revert-select (or (buffer-file-name) default-directory)))

(defun tortoise-svn-revert-select (filename &optional wildcards)
  (interactive (find-file-read-args "Find file: " t))
  (tortoise-command "revert" filename))

(defun tortoise-svn-help ()
 (interactive)
 (start-process-shell-command "tortoise-svn" nil
  (concat "TortoiseProc /command:help")))

; add key bind
(global-set-key "\C-xvtl" 'tortoise-svn-log)
(global-set-key "\C-xvtL" 'tortoise-svn-log-select)

(global-set-key "\C-xvt=" 'tortoise-svn-diff)

(global-set-key "\C-xvtb" 'tortoise-svn-blame)
(global-set-key "\C-xvtB" 'tortoise-svn-repobrowser)

(global-set-key "\C-xvtc" 'tortoise-svn-commit)
(global-set-key "\C-xvtC" 'tortoise-svn-commit-select)

(global-set-key "\C-xvts" 'tortoise-svn-repostatus)
(global-set-key "\C-xvtS" 'tortoise-svn-repostatus-select)

(global-set-key "\C-xvtr" 'tortoise-svn-revert)
(global-set-key "\C-xvtR" 'tortoise-svn-revert-select)

(global-set-key "\C-xvth" 'tortoise-svn-help)

(provide 'tortoise-svn)

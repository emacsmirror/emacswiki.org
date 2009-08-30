;;; shell-completion.el --- provides tab completion for shell commands

;; Copyright (C) 2006 Ye Wenbin
;;
;; Author: Ye Wenbin <wenbinye@163.com>
;; Version: $Id: shell-completion.el,v 0.0 2006/08/14 15:08:48 ywb Exp $
;; Package-Version: 0.0
;; Keywords: completion, shell, emacs
;; X-URL: <http://www.emacswiki.org/cgi-bin/wiki/shell-completion.el>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Put this file into your load-path and the following into your ~/.emacs:
;; (require 'shell-completion)

;; If you want use with lftp, put this to .emacs
;; (defvar my-lftp-sites (shell-completion-get-file-column "~/.lftp/bookmarks" 0 "[ \t]+"))
;; (add-to-list 'shell-completion-options-alist
;;              '("lftp" my-lftp-sites))
;; (add-to-list 'shell-completion-prog-cmdopt-alist
;;              '("lftp" ("help" "open" "get" "mirror") ("open" my-lftp-sites)))

;;; Code:

(provide 'shell-completion)
(eval-when-compile
  (require 'comint)
  (require 'cl))

(defvar shell-completion-sudo-cmd "sudo")

(defvar shell-completion-options-alist
  '(
    ("apt-get" "remove" "install" "update" "dist-upgrade")
    ("dpkg" "--get-selections" "--set-selections")
    ("mplayer" "--help" "-vobsubid")
    ))

(defvar shell-completion-prog-cmd-alist
  '(
    ("lftp" "help" "open" "get" "mirror" "exit" "mget")
    ("mysql" "show" "desc" "create" "update")
    )
  "This is alist of command for specific programs."
  )

(defvar shell-completion-prog-cmdopt-alist
  `(
    ("mysql" ("show" "tables" "databases") ("create" "table" "database"))
    )
  "This is alist of options for command in specific programs.")

;;; Completion functions
(defun shell-completion-prog-command ()
  "Completion for command options in specific program"
  (let ((opt (current-word))
        (prompt (save-excursion
                  (re-search-backward comint-prompt-regexp nil t)
                  (match-string 0)))
        (progs shell-completion-prog-cmd-alist)
        item completions)
    (while (progn                       
             (and progs
                  (null
                   (setq item (car progs)
                         progs (cdr progs)
                         completions (if (string-match (car item) prompt)
                                         (cdr item)))))))
    (let ((success (let ((comint-completion-addsuffix nil))
                     (comint-dynamic-simple-complete opt completions))))
      (if  (and (memq success '(sole shortest)) comint-completion-addsuffix)
          (insert " "))
      success)))
  
(defun shell-completion-get-alist ()
  "When detect we are in some specific programs, use the alist in
`shell-completion-prog-cmdopt-alist'. Otherwise, use `shell-completion-options-alist'."
  (let ((prompt (save-excursion
                  (re-search-backward comint-prompt-regexp nil t)
                  (match-string 0)))
        (progs shell-completion-prog-cmdopt-alist)
        item alist)
    (while (progn                       
             (and progs
                  (null
                   (setq item (car progs)
                         progs (cdr progs)
                         alist (if (string-match (car item) prompt)
                                   (cdr item)))))))
    (or alist shell-completion-options-alist)))

(eval-after-load "shell"
  '(progn
(defun shell-completion-cmd-options ()
  "Completions for command options.

See `shell-completion-options-alist' and `shell-completion-prog-cmdopt-alist'."
  (let* ((opt (current-word))
         (alist (shell-completion-get-alist))
         (cmd
          (save-excursion
            (shell-backward-command 1)
            (when shell-completion-sudo-cmd
              (if (looking-at (format "\\s-*%s\\s-+" shell-completion-sudo-cmd))
                  (goto-char (match-end 0))))
            (current-word)))
         (completions (cdr (assoc cmd alist)))
         all item)
    (while completions
      (setq item (car completions)
            completions (cdr completions)
            all (append all
                        (cond ((stringp item) (list item))
                              ((fboundp item) (funcall item))
                              ((boundp item) (symbol-value item))
                              (t (error "Options for %s is not found!" cmd))))))
    (setq completions all)
    (let ((success (let ((comint-completion-addsuffix nil))
                     (comint-dynamic-simple-complete opt completions))))
      (if (and (memq success '(sole shortest)) comint-completion-addsuffix)
          (insert " "))
      success)))
     (add-to-list 'shell-dynamic-complete-functions 'shell-completion-cmd-options t)
     (add-to-list 'shell-dynamic-complete-functions 'shell-completion-prog-command)
     (when shell-completion-sudo-cmd
       (defun shell-backward-command (&optional arg)
         "Move backward across ARG shell command(s).  Does not cross lines.
See `shell-command-regexp'."
         (interactive "p")
         (let ((limit (save-excursion (comint-bol nil) (point))))
           (when (> limit (point))
             (setq limit (line-beginning-position)))
           (skip-syntax-backward " " limit)
           (if (re-search-backward
                (format "[;&|]+[\t ]*\\(%s\\)" shell-command-regexp) limit 'move arg)
               (progn (goto-char (match-beginning 1))
                      (skip-chars-forward ";&|")))
           (if (looking-at (format "\\s-*%s\\s-+" shell-completion-sudo-cmd))
               (goto-char (match-end 0))))))))

;;; Some funtions may be useful
;;;###autoload
(defun shell-completion-get-column (start end col &optional sep)
  (let (val)
    (or sep (setq sep "\t"))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (setq val
              (append val
                      (list (nth col (split-string
                             (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))
                             sep)))))
        (forward-line 1))
      val)))

;;;###autoload
(defun shell-completion-get-file-column (file col &optional sep)
  (with-temp-buffer
    (insert-file-contents file)
    (shell-completion-get-column (point-min) (point-max) col sep)))

;;; shell-completion.el ends here

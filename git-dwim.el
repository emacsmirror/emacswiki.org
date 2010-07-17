;;;; git-dwim.el --- Context-aware git commands such as branch handling
;; Time-stamp: <2010-07-17 15:14:12 rubikitch>

;; Copyright (C) 2010  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: git, tools, convenience
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/git-dwim.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This file provides context-aware git commands.
;; Currently only `git-branch-next-action'.
;; 
;; `git-branch-next-action' does typical branch handling.
;;    * If current branch is master: switch to other or new branch.
;;    * If current branch is not master: switch to other branch or merge this branch to master.
;;    * If merge is failed: continue merging (You have to resolve conflict merker)

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `git-branch-next-action'
;;    Do appropriate action for branch.
;;  `git-create-new-branch'
;;    Create new BRANCH and switch to it.
;;  `git-switch-to-other-branch'
;;    Switch to existing BRANCH.
;;  `git-merge-to'
;;    Merge this branch to master.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Installation:
;;
;; Put git-dwim.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'git-dwim)
;; (global-set-key "\C-xvB" 'git-branch-next-action)

;; No need more.

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET git-dwim RET
;;


;;; History:

;; See http://www.rubyist.net/~rubikitch/gitlog/emacs-git-dwim.txt

;;; Code:

(eval-when-compile (require 'cl))
(defgroup git-dwim nil
  "git-dwim"
  :group 'vc)

(defun gd-shell-command (command)
  (shell-command command " *git-dwim*"))
(defun git-current-branch ()
  (substring (shell-command-to-string "git branch | grep '\*'") 2 -1))
(defun git-get-branches ()
  (split-string (shell-command-to-string "git branch | cut -b3-") "\n" t))
(defun git-unmerged-p ()
  (string-match "^# Unmerged paths:" (shell-command-to-string "git status")))
(defun gd-display-string (output buffer)
  (with-current-buffer (get-buffer-create buffer)
    (buffer-disable-undo)
    (insert output "\n===========================================================\n")
    (display-buffer (current-buffer))))
(defun gd-menu-select (menu-alist)
  "Menu selection. MENU-ALIST is a list of (KEY DISPLAY FUNCTION).
KEY is a character such as ?a.
DISPLAY is display string.
FUNCTION has no argument and is called when the KEY is pressed.

Example:
 (gd-menu-select '((?a \"A\" (lambda () (message \"[a]\")))
                   (?b \"B\" (lambda () (error \"err\")))))
"
  (condition-case err
      (funcall (caddr (assq (read-event (mapconcat 'identity (mapcar 'cadr menu-alist) " "))
                            menu-alist)))
    (void-function (error "invalid key"))))

;; (gd-menu-select '((?a "A" (lambda () (message "[a]")))(?b "B" (lambda () (error "err")))))
(defun git-branch-next-action ()
  "Do appropriate action for branch.

* If current branch is master: switch to other or new branch.
* If current branch is not master: switch to other branch or merge this branch to master.
* If merge is failed: continue merging (You have to resolve conflict merker)
"
  (interactive)
  (cond ((equal (git-current-branch) "master")
         (gd-menu-select '((?s "[s]witch-to-other-branch" git-switch-to-other-branch)
                           (?c "[c]reate-new-branch"      git-create-new-branch))))
        ((git-unmerged-p)
         (git-merge-to "master" t))
        (t
         (gd-menu-select '((?s "[s]witch-to-other-branch" git-switch-to-other-branch)
                           (?m "[m]erge-to-master"        git-merge-to))))))

(defun git-create-new-branch (&optional branch)
  "Create new BRANCH and switch to it."
  (interactive)
  (setq branch (or branch (read-string "Create and switch to new branch: ")))
  (gd-shell-command (format "git checkout -b %s" branch)))
(defun git-switch-to-other-branch (&optional branch)
  "Switch to existing BRANCH."
  (interactive)
  (setq branch (or branch (completing-read "Switch to new branch: "
                                           (git-get-branches) nil t)))
  (gd-shell-command (format "git checkout %s" branch)))
(defun git-merge-to (&optional branch continue)
  "Merge this branch to master."
  (interactive)
  (setq branch (or branch "master"))
  (when continue
    (gd-shell-command (format "git add %s" (shell-quote-argument buffer-file-name))))
  (let ((output (shell-command-to-string
                 (format "git rebase %s %s" (if continue "--continue" "") branch))))
    (if (string-match "^CONFLICT\\|^You must edit all merge conflicts" output)
        (gd-display-string output "*git rebase conflict*")
      (let ((cur (git-current-branch)))
        (ignore-errors (kill-buffer "*git rebase conflict*"))
        (gd-shell-command (format "git checkout %s; git merge %s; git branch -d %s"
                               branch cur cur))))))
(provide 'git-dwim)

;; How to save (DO NOT REMOVE!!)
;; (progn (git-log-upload) (emacswiki-post "git-dwim.el"))
;;; git-dwim.el ends here

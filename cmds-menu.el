;;; cmds-menu.el --- `Recent Commands' submenu for the menu-bar `Tools' menu.
;;
;; Filename: cmds-menu.el
;; Description: `Recent Commands' submenu for the menu-bar `Tools' menu.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2013-2018, Drew Adams, all rights reserved.
;; Created: Sat Oct 19 12:56:51 2013 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Mon Jan  1 10:14:48 2018 (-0800)
;;           By: dradams
;;     Update #: 87
;; URL: https://www.emacswiki.org/emacs/download/cmds-menu.el
;; Doc URL: https://www.emacswiki.org/emacs/RecentCommandsMenu
;; Keywords: convenience, command, menu
;; Compatibility: GNU Emacs: 21.x, 22.x, 23.x, 24.x, 25.x, 26.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    `Recent Commands' submenu for the menu-bar `Tools' menu.
;;
;;  Global minor mode `recent-cmds-menu-mode' adds a `Recent Commands'
;;  submenu to the menu-bar `Tools' menu and updates
;;  `menu-bar-update-hook' so that this menu is automatically updated.
;;  Menu `Recent Commands' holds the most recent commands you have
;;  invoked using `M-x'.
;;
;;  Put this in your init file (~/.emacs):
;;
;;   (require 'cmds-menu)
;;
;;  If you want to turn on the mode from the outset, add this:
;;
;;   (recent-cmds-menu-mode 1)
;;
;;
;;  User options defined here:
;;
;;    `recent-cmds-menu-max-size', `recent-cmds-menu-mode',
;;    `recent-cmds-name-length'.
;;
;;  Commands defined here:
;;
;;    `recent-cmds-menu-mode'.
;;
;;  Non-interactive functions defined here:
;;
;;    `recent-cmds-menu-bar-update'.
;;
;;  Internal variables defined here:
;;
;;    `recent-cmds-menu'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2017/01/24 dadams
;;     recent-cmds-menu-bar-update: caddr -> car of cddr.
;; 2016/08/30 dadams
;;     Added: recent-cmds-menu-mode.  Removed Emacs 20 support.
;; 2013/11/02 dadams
;;     Removed autoload cookie.
;; 2013/10/19 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom recent-cmds-menu-max-size 10
  "Maximum number of commands for menu `Recent Commands'.
If this is an integer N then show only the N most recent commands.
If this is nil then show all commands invoked.
A large number or nil slows down menu responsiveness."
  :type '(choice
          (integer :tag "Include at most" :value 10)
          (const   :tag "Include all" nil))
  :group 'menu)

(defcustom recent-cmds-name-length 30
  "Maximum length of a command name for menu `Recent Commands'.
If this is an integer N then truncate names to N characters.
If this is nil then show command names in full."
  :type '(choice
          (integer :tag "Truncate name" :value 30)
          (const :tag "Full name" nil))
  :group 'menu)

(defvar recent-cmds-menu (make-sparse-keymap "Recent Commands")
  "`Recent Commands' submenu of `Tools' menu-bar menu.")
(defalias 'recent-cmds-menu (symbol-value 'recent-cmds-menu))

(defun recent-cmds-menu-bar-update ()
  "Update menu `Recent Commands', `recent-cmds-menu'."
  (and (lookup-key menu-bar-tools-menu [recent-cmds])
       (let ((cmds  (mapcar #'car command-history))
	     recent-menu)
	 (if (and (integerp recent-cmds-menu-max-size)
		  (> recent-cmds-menu-max-size 1))
	     (if (> (length cmds) recent-cmds-menu-max-size)
		 (setcdr (nthcdr recent-cmds-menu-max-size cmds) nil)))
	 (setq recent-menu
	       (let (alist)
		 (dolist (cmd  cmds)
		   (let ((name  (format "%s" cmd)))
                     (unless (eq ?\   (aref name 0))
                       (let ((cmd-info  (list (if (and (integerp recent-cmds-name-length)
                                                       (> (length name) recent-cmds-name-length))
                                                  (concat (substring name 0 (/ recent-cmds-name-length 2))
                                                          "..."
                                                          (substring name (- (/ recent-cmds-name-length 2))))
                                                name)
                                              cmd
                                              (where-is-internal cmd nil 'nonascii))))
                         (unless (member cmd-info alist) (push cmd-info alist))))))
                 (let ((menu ()))
                   (dolist (cmd-info  alist)
                     (push (nconc (list (intern (concat "recent-" (car cmd-info))) 'menu-item (car cmd-info)
                                        `(lambda () (interactive) (call-interactively ',(cadr cmd-info)))
                                        (cons nil nil))
                                  (and (car (cddr cmd-info))
                                       (list :keys (key-description (car (cddr cmd-info))))))
                           menu))
                   menu)))
	 (setcdr recent-cmds-menu recent-menu))))

(define-minor-mode recent-cmds-menu-mode
    "Add `Recent Commands' submenu to menu-bar `Tools' menu.
Update `menu-bar-update-hook' so this menu is automatically updated."
  :init-value nil :global t :require 'cmds-menu
  (cond (recent-cmds-menu-mode
         (add-hook 'menu-bar-update-hook 'recent-cmds-menu-bar-update)
         (recent-cmds-menu-bar-update))
        (t
         (recent-cmds-menu-bar-update)
         (remove-hook 'menu-bar-update-hook 'recent-cmds-menu-bar-update)))
  (when (interactive-p)
    (message "`Recent Commands' submenu of menu-bar `Tools' menu is %s"
             (if recent-cmds-menu-mode "AVAILABLE" "REMOVED"))))

(define-key menu-bar-tools-menu [recent-cmds]
    '(menu-item "Recent Commands" recent-cmds-menu :visible recent-cmds-menu-mode
      :help "Invoke recent commands"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'cmds-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cmds-menu.el ends here

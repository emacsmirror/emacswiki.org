;;; w32-exec-predef.el --- execute Start Menu shortcuts from inside Emacs

;; Copyright (C) 2005 Mathias Dahl

;; Version: 0.2
;; Keywords: w32, vbscript, shortcuts, start menu
;; Author: Mathias Dahl <mathias.removethis.dahl@gmail.com>
;; Maintainer: Mathias Dahl
;; URL: http://www.emacswiki.org/cgi-bin/wiki/WThirtyTwoExecPredef

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provide the function `w32-exec-predef' which can be
;; used to execute shortcuts from the Start Menu in Windows, using
;; iswitchb completion. This is yet another step in avoiding to leave
;; the safe environment living inside Emacs... :)
;;
;; The user does not need to keep this list updated himself as there
;; is a function, `w32-exec-predef-build', that does it for him. This,
;; however, means that the first time in each session that the
;; function is run, it will take a bit longer because it will populate
;; all information from the Start Menu.

;;; Prerequisites:

;; * requires findr.el for doing the recursive scan for .lnk files and
;;   w32-utl.el for some utility functions.

;;; History:

;; Version 0.2, 2005-03-30

;; * Added read and write of command list from file and made
;;   `w32-exec-predef' smarter.

;; * Made all functions that deals with the command list interactive
;;   so that the user can control it.

;; Version 0.1, 2005-03-26

;; * First release.

;;; Bugs:

;;; Todo:

;; - Customize option to tell from where to populate the shortcuts;
;;   StartMenu, AllUsersStartMenu, Desktop, or all.

;;; Code:

(require 'findr)
(require 'w32-utl)

(defvar w32-exec-predef-command-list nil
  "Keeps the list of avaible commands. Populated by
`w32-exec-predef-build'")

(defun w32-exec-predef-build ()
  (interactive)
  "Build the command list by recursively look for .lnk files"
  (let ((files (findr "\.lnk$" (w32-utl-special-folder "StartMenu")))
        link-name)
    (message "Shortcuts found. Building program list...")
    (setq w32-exec-predef-command-list
          (mapcar 
           (lambda (file)
             (setq link-name (file-name-nondirectory file))
             (setq link-name (substring
                              link-name 0 (- (length link-name) 4)))
             (cons link-name (w32-utl-lnk-get-target-and-args file)))
           files))))

(defvar w32-exec-predef-command-list-file-name
  "~/.w32-exec-predef-commands"
  "File to store the command list")

(defun w32-exec-predef-save-command-list ()
  (interactive)
  (let ((buf (get-buffer-create "*w32-exec-predef-save-command-list*")))
    (set-buffer buf)
    (erase-buffer)
    (insert "(setq w32-exec-predef-command-list '")
    (goto-char (point-max))
    (print w32-exec-predef-command-list buf)
    (goto-char (point-max))
    (insert ")")
    (write-region (point-min) (point-max) 
                  w32-exec-predef-command-list-file-name)))

(defun w32-exec-predef-load-command-list ()
  (interactive)
  (if (file-exists-p w32-exec-predef-command-list-file-name)
      (load-file w32-exec-predef-command-list-file-name)
    nil))

;; The code for w32-exec-predef-read-completing was ripped from the
;; function my-icompleting-read in the Commentary of
;; iswitchb.el. Original code by Kin Cho.

(defun w32-exec-predef-read-completing (prompt choices)
  "Use iswitch as a completing-read replacement to choose from
choices.  PROMPT is a string to prompt with.  CHOICES is a list of
strings to choose from."
  (let ((iswitchb-make-buflist-hook
         (lambda ()
           (setq iswitchb-temp-buflist choices))))
    (iswitchb-read-buffer prompt)))

(defun w32-exec-predef ()
  "Using iswitchb completion, execute a program from a list of
programs collected from the user's Start Menu."
  (interactive)
  ;; If command list is empty, try reading it from file. If the file
  ;; could not be read, the last option is to build the file from
  ;; scratch.
  (if (and (eq 0 (length w32-exec-predef-command-list))
           (not (w32-exec-predef-load-command-list)))
      (w32-exec-predef-build))
  (let ((program (cdr
                  (assoc
                   (w32-exec-predef-read-completing
                    ": " 
                    (mapcar (lambda (x) 
                              (car x))
                            w32-exec-predef-command-list)) 
                   w32-exec-predef-command-list))))
    (w32-shell-execute "Open" (car program) (cadr program))))

(provide 'w32-exec-predef)

;;; w32-exec-predef.el ends here

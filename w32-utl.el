;;; w32-utl.el --- small utility functions implemented using vbscript

;; Copyright (C) 2005 Mathias Dahl

;; Version: 0.1
;; Keywords: w32, vbscript, utilities
;; Author: Mathias Dahl <mathias.removethis.dahl@gmail.com>
;; Maintainer: Mathias Dahl
;; URL: http://www.emacswiki.org/cgi-bin/wiki/WThirtyTwoUtl

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

;; This package contains a couple of utility functions implemented
;; partly in elisp and partly in VBScript:

;; - `w32-utl-special-folder'
;;     Get the path to a "special folder" (Desktop, StartMenu, Recent, etc.)
;; - `w32-utl-lnk-get-target-and-args'
;;     Get values from Windows shortcuts (.lnk files).

;; I needed these functions primarily for `w32-exec-predef' in
;; w32-exec-predef.el but as they could be useful in other situations
;; I decided to put them in a separate file.

;; The vbscript files needed will be created by this script,
;; minimizing the dependancies on another files at installation time.

;;; Prerequisites:

;; * It needs cscript.exe to be able to execute the vbscript scripts.

;;; History:

;; Version 0.1, 2005-03-26
;; * First release.

;;; Bugs:

;;; Todo:

;;; Code:

(defvar w32-utl-lnk-script "~/w32-utl-lnk.vbs"
  "VBScript file that does the actual work in
`w32-utl-lnk-get-target-and-args'")

(defun w32-utl-lnk-create-script ()
  "Create the script file needed by `w32-utl-lnk-get-target-and-args'"
  (let ((buf (get-buffer-create "*w32-utl-lnk-create-script*")))
    (set-buffer buf)
    (insert "
             Set oShell = CreateObject(\"WScript.Shell\")
             Set oLnk = oShell.CreateShortcut(WScript.Arguments(0))
             WScript.Echo oLnk.TargetPath & \";\" & oLnk.Arguments
             Set oShell = Nothing
             Set oLnk = Nothing
             ")
    (goto-char (point-min))
    (write-region (point-min) (point-max) w32-utl-lnk-script nil 'quiet)))

(defun w32-utl-lnk-get-target-and-args (lnk-file)
  "Return as a list, the target and arguments (if any) for the
Windows shortcut lnk-file."
  (if (not (file-exists-p w32-utl-lnk-script))
      (w32-utl-lnk-create-script))
  (let ((buf (get-buffer-create "*w32-utl-lnk*"))
        target-and-args
        target
        args)
    (set-buffer buf)
    (erase-buffer)
    (call-process "cscript" nil buf nil "//nologo"
                  (expand-file-name w32-utl-lnk-script) lnk-file)
    (goto-char (point-min))
    (end-of-line)
    (setq target-and-args (buffer-substring (point-min) (point)))
    (split-string target-and-args ";")))

(defvar w32-utl-special-folder-script "~/w32-utl-special-folder.vbs"
    "VBScript file that does the actual work in
`w32-utl-special-folder'")

(defun w32-utl-special-folder-create-script ()
  "Create the script file needed by `w32-utl-special-folder'"
  (let ((buf (get-buffer-create "*w32-utl-special-folder-create-script*")))
    (set-buffer buf)
    (insert "
             Set oShell = CreateObject(\"WScript.Shell\")
             WScript.Echo oShell.SpecialFolders(WScript.Arguments(0))
             Set oShell = Nothing
             ")
    (goto-char (point-min))
    (write-region (point-min) (point-max) w32-utl-special-folder-script nil 'quiet)))

(defun w32-utl-special-folder (folder-name)
"Get special shell folder. One of the following works:

    * AllUsersDesktop
    * AllUsersStartMenu
    * AllUsersPrograms
    * AllUsersStartup
    * Desktop
    * Favorites
    * Fonts
    * MyDocuments
    * NetHood
    * PrintHood
    * Programs
    * Recent
    * SendTo
    * StartMenu
    * Startup
    * Templates"
  (if (not (file-exists-p w32-utl-special-folder-script))
      (w32-utl-special-folder-create-script))
  (let ((buf (get-buffer-create "*w32-utl-special-folder*"))
        path)
    (set-buffer buf)
    (erase-buffer)
    (call-process "cscript" nil buf nil "//nologo"
                  (expand-file-name w32-utl-special-folder-script) folder-name)
    (goto-char (point-min))
    (end-of-line)
    (setq path (expand-file-name (buffer-substring (point-min) (point))))))

(provide 'w32-utl)

;;; w32-utl.el ends here

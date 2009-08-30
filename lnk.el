;;; lnk.el --- Follow Microsoft Explorer shortcuts

;; Author:  Peter Breton
;; Created: Tue Sep 03 2002
;; Version: $Id: lnk.el,v 1.1 2002/09/06 11:18:39 pbreton Exp $

;; This is free software. This file is NOT part of GNU Emacs, but is
;; distributed in the same spirit.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;;; Commentary:
;;
;; Installation instructions:
;;
;; 1. Put lnk.el somewhere in your load-path.
;; 2. Put shortcut.vbs in an appropriate location (with lnk.el,
;; with other VBS scripts, etc).
;; 3. Add the following lines to your .emacs file:
;;
;; (require 'lnk)
;; (setq lnk-script FILENAME-OF-SHORTCUT)
;;
;; To dereference shortcuts without asking permission do:
;;
;; (setq lnk-always-follow-shortcuts t)

;;; Code:

(defvar lnk-cscript-program "cscript.exe"
  "Interpreter program for VBS scripts.")

(defvar lnk-script  "shortcut.vbs"
  "VBS script to dereference a Windows shortcut.")

(defvar lnk-always-follow-shortcuts  nil
  "If true, always dereference shortcuts without asking permission.")

(defun lnk-dereference-shortcut  (file)
  "Print the name of the file that a shortcut is mapped to."
  (interactive "fDereference shortcut: ")
  (let (run-command-display-output-buffer
 (buf      " *Shortcut*"))
    (run-command
     buf
     lnk-cscript-program
     "//nologo"
     lnk-script
     (expand-file-name file))
    (with-current-buffer (get-buffer buf)
      (goto-char (point-min))
      (end-of-line)
      (buffer-substring-no-properties (point-min) (point))
      )))

(defun lnk-follow-shortcut  ()
  "Dereference a Windows shortcut."
  (interactive)
  (and (buffer-file-name)
       (string-match "\\.lnk\\'" (buffer-file-name))
       (let ((shortcut (lnk-dereference-shortcut (buffer-file-name))))
  (and shortcut
       (or
        lnk-always-follow-shortcuts
        (yes-or-no-p "Follow shortcut? "))
       (progn
  (kill-buffer (get-file-buffer (buffer-file-name)))
  (find-file shortcut)
  )))))

(add-hook 'find-file-hooks 'lnk-follow-shortcut)

(provide 'lnk)

;;; lnk.el ends here

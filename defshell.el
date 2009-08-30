;;; defshell.el --- Define inferior shells -*-unibyte: t; coding: iso-8859-1;-*-

;; Copyright ? 2000,2001,2003 Kevin Rodgers

;; Author: Kevin Rodgers <ihs_4664@yahoo.com>
;; Created: 13 Dec 2000
;; Version: $Revision: 1.4 $
;; Keywords: convenience, shell
;; RCS $Id: defshell.el,v 1.4 2003/08/14 20:26:05 kevinr Exp kevinr $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; defshell.el makes it easy to run different inferior shells at the
;; same time, by defining new commands for the Bourne (M-x sh), C (M-x
;; csh), Korn (M-x ksh), and Bourne Again (M-x bash) shells.  And
;; whereas the `M-x shell' command defined in Emacs' shell.el always
;; reuses the *shell* buffer, defshell.el can be customized via the
;; defshell-reuse-buffer and defshell-rename-buffer-uniquely options
;; (e.g. for `M-x sh') to reuse the *sh* buffer or create a *sh*<2> (or
;; *sh*<N>) buffer.
;; 
;; defshell.el also provides the `defshell' macro to define additional
;; shell commands.  E.g. to define the Foo shell (M-x fsh):
;; 	(defshell "/bin/fsh" "Foo")
;; or to define a DNS lookup shell (M-x nslookup):
;; 	(defshell "/usr/sbin/nslookup" "DNS lookup")
;; and since `nslookup` doesn't accept the -i option:
;; 	(setq explicit-nslookup-args '())

;;; Code:
(provide 'defshell)

(require 'custom)			; defgroup, defcustom
(require 'comint)			; comint-check-proc
(require 'shell)			; shell

(defgroup defshell nil
  "Define inferior shells."
  :group 'shell)

(defcustom defshell-reuse-buffer t
  "*If non-nil, reuse `defshell-shell' buffers." ; i.e. pop-to-buffer
  :group 'defshell
  :type '(boolean))

(defcustom defshell-rename-buffer-uniquely nil
  "*If non-nil, rename `defshell-shell' buffers uniquely."
  :group 'defshell
  :type '(boolean))

(defun defshell-shell (defshell-file-name)
  "*Run an inferior DEFSHELL-FILE-NAME shell."
  (interactive "sShell: ")
  (let* ((explicit-shell-file-name defshell-file-name)
	 (defshell-buffer-name (concat "*"
				       (file-name-sans-extension
					(file-name-nondirectory
					 explicit-shell-file-name))
				       "*")))
    (if (and defshell-reuse-buffer
	     (comint-check-proc defshell-buffer-name)) ; see shell.el
	(pop-to-buffer (get-buffer defshell-buffer-name))
      (progn
	(shell)
	(rename-buffer defshell-buffer-name
		       (or defshell-rename-buffer-uniquely
			   (comint-check-proc defshell-buffer-name)))))))

(defmacro defshell (shell-file-name &optional shell-name)
  (let ((command (intern (file-name-sans-extension (file-name-nondirectory
						    shell-file-name)))))
    `(defun ,command ()
       ,(format "*Run an inferior %s shell." (or shell-name shell-file-name))
       (interactive)
       (defshell-shell ,shell-file-name))))

(defshell "/bin/sh" "Bourne")
(defshell "/bin/csh" "C")
(defshell "/bin/ksh" "Korn")
(defshell "/usr/local/bin/bash" "Bourne Again")

;;; defshell.el ends here

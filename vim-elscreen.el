;;; vim-elscreen.el ---

;; Copyright 2011 Yen-Chin,Lee
;;
;; Author: coldnew coldnew.tw@gmail.com
;; Keywords:
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/download/vim-elscreen.el
(defconst vim-elscreen-version "0.3")

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
;;
;;

;;; Usage:
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'vim-elscreen)

;;; Code:

(eval-when-compile (require 'cl))

(require 'vim)
(require 'elscreen)

;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################




;;;;;;;; Opening a new page

(vim:defcmd vim:cmd-tab-new ((argument:file file) nonrepeatable)
  "Open a new tab page and edit {file}, like with :edit."
  "If the {file} is nil, create a new tab page with *scratch* buffer, after the current tab page."
  (if file
      (elscreen-find-file file)
    (elscreen-create)))

(vim:defcmd vim:cmd-tab-find ((argument:file file) nonrepeatable)
  "Open a new tab page and edit {file} in 'path', like with :find. "
  (if file
      (elscreen-find-file file)
    (message "Error: need argument")
    ))

;; FIXME: need to add a function that return if command does not exist
;;        maybe fix vim:ex-execute-command will do the work
(vim:defcmd vim:cmd-tab-command ((argument:text text) nonrepeatable)
  "Execute {cmd} and when it opens a new window open a new tab
   page instead.  Doesn't work for :diffsplit, :diffpatch,:execute and :normal.
   Examples: >
	     :tab split      'opens current buffer in new tab page'"
  (if text
      (progn
	(elscreen-create)
	(vim:ex-execute-command text))
    ;; TODO: If command does not exist, do something
    ))

;; TODO:
;; CTRL-W gf       Open a new tab page and edit the file name under the cursor.
;;                 See CTRL-W_gf.

;; CTRL-W gF       Open a new tab page and edit the file name under the cursor
;;                 and jump to the line number following the file name.
;;                 See CTRL-W_gF.



;;;;;;;; Closing a tab page

;; FIXME: in vim, tabclose cant have {count}
(vim:defcmd vim:cmd-tab-close (nonrepeatable)
  "Close current tab page. However the buffer still alive."
  (elscreen-kill))

(vim:defcmd vim:cmd-tab-close-f (nonrepeatable)
  "Close current tab page. and also kill current buffer."
  (kill-buffer)
  (elscreen-kill))

(vim:defcmd vim:cmd-tab-close-other (nonrepeatable)
  "Close all other tabpages. Do not kill other buffers"
  (elscreen-kill-others))

(vim:defcmd vim:cmd-tab-close-other-f (nonrepeatable)
  "Close all other tabpages and alos kill other buffers"
  (elscreen-kill-others)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;;;;;;;; Switching to another tab page

(vim:defcmd vim:cmd-tab-next (count argument nonrepeatable)
  ""
  (if elscreen-display-screen-number
      (progn
	(if (string= real-last-command "vim:motion-beginning-of-line-or-digit-argument")
	    (setq count 0))
	(cond
	 (argument (elscreen-goto (string-to-number argument)))
	 (count  (elscreen-goto count))
	 (t (elscreen-next))))
    ;; If tab does not display number, do the same as vim's :tabnext
    (cond
     (argument (dotimes (dummy (string-to-number argument))
		 (elscreen-next)))
     (count (dotimes (dummy count)
	      (elscreen-next)))
     (t (elscreen-next)))))


(vim:defcmd vim:cmd-tab-previous (count argument nonrepeatable)
  ""
  (if elscreen-display-screen-number
      (progn
	(if (string= real-last-command "vim:motion-beginning-of-line-or-digit-argument")
	    (setq count 0))
	(cond
	 (argument (elscreen-goto (string-to-number argument)))
	 (count (elscreen-goto count))
	 (t (elscreen-previous))))
    ;; If tab does not display number, do the same as vim's :tabprevious
    (cond
     (argument (dotimes (dummy (string-to-number argument))
		 (elscreen-previous)))
     (count (dotimes (dummy count)
	      (elscreen-previous)))
     (t (elscreen-previous)))))


;;;;;;;; vim-mode keymaps
;; Normal map
(vim:nmap (kbd "gt") 'vim:cmd-tab-next)
(vim:nmap (kbd "C-<next>") 'vim:cmd-tab-next)
(vim:nmap (kbd "gT") 'vim:cmd-tab-previous)
(vim:nmap (kbd "C-<prior>") 'vim:cmd-tab-previous)

(vim:mmap (kbd "gt") 'vim:cmd-tab-next)
;; Insert map
(vim:imap (kbd "C-<next>") 'vim:cmd-tab-next)
(vim:imap (kbd "C-<prior>") 'vim:cmd-tab-previous)

;; ex command
(vim:emap "tabnew" 'vim:cmd-tab-new)
(vim:emap "tabedit" "tabnew")
(vim:emap "tabe" "tabedit")
(vim:emap "tabfind" 'vim:cmd-tab-find)
(vim:emap "tabf" "tabfind")
(vim:emap "tab" 'vim:cmd-tab-command)
(vim:emap "tabclose" 'vim:cmd-tab-close)
(vim:emap "tabc" "tabclose")
(vim:emap "tabclose!" 'vim:cmd-tab-close-f)
(vim:emap "tabc!" "tabclose!")
(vim:emap "tabonly" 'vim:cmd-tab-close-other)
(vim:emap "tabo" "tabonly")
(vim:emap "tabonly!" 'vim:cmd-tab-close-other-f)
(vim:emap "tabo!" "tabonly!")
(vim:emap "tabnext" 'vim:cmd-tab-next)
(vim:emap "tabn" "tabnext")
(vim:emap "tabprevious" 'vim:cmd-tab-previous)
(vim:emap "tabp" "tabprevious")
(vim:emap "tabNext" "tabprevious")
(vim:emap "tabN" "tabprevious")




(provide 'vim-elscreen)
;; vim-elscreen.el ends here.


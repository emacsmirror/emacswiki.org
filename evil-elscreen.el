;;; evil-elscreen.el --- evil wrpaaer to use elscreen as tab

;; Copyright 2013 Yen-Chin,Lee <coldnew>
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords:
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/download/evil-elscreen.el
(defconst evil-elscreen-version "0.1")

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
;;   (require 'evil-elscreen)

;;; Code:

(eval-when-compile (require 'cl))

(require 'evil)
(require 'elscreen)

;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

;;;;;;;; Opening a new page

(evil-define-command evil-elscreen/tab-new (file)
  "Open a new tab page and edit {file}, like with :edit.
If the {file} is nil, create a new tab page with *scratch* buffer, after the current tab page."
  :repeat nil
  (interactive "<f>")
  (if file
      (elscreen-find-file file)
    (elscreen-create)))

(evil-define-command evil-elscreen/tab-find (file)
  "Open a new tab page and edit {file} in 'path', like with :find. "
  :repear nil
  (interactive "<f>")
  (if file
      (elscreen-find-file file)
    (message "Error: need argument")
    ))

;; FIXME: need to add a function that return if command does not exist
;;        maybe fix vim:ex-execute-command will do the work
;; (vim:defcmd evil-elscreen/tab-command ((argument:text text) nonrepeatable)
;;   "Execute {cmd} and when it opens a new window open a new tab
;;    page instead.  Doesn't work for :diffsplit, :diffpatch,:execute and :normal.
;;    Examples: >
;; 	     :tab split      'opens current buffer in new tab page'"
;;   (if text
;;       (progn
;; 	(elscreen-create)
;; 	(vim:ex-execute-command text))
;;     ;; TODO: If command does not exist, do something
;;     ))

;; TODO:
;; CTRL-W gf       Open a new tab page and edit the file name under the cursor.
;;                 See CTRL-W_gf.

;; CTRL-W gF       Open a new tab page and edit the file name under the cursor
;;                 and jump to the line number following the file name.
;;                 See CTRL-W_gF.



;;;;;;;; Closing a tab page

;; FIXME: in vim, tabclose cant have {count}
(evil-define-command evil-elscreen/tab-close ()
  "Close current tab page. However the buffer still alive."
  :repeat nil
  (interactive)
  (elscreen-kill))

(evil-define-command evil-elscreen/tab-close-f ()
  "Close current tab page. and also kill current buffer."
  :repeat nil
  (interactive)
  (kill-buffer)
  (elscreen-kill))

(evil-define-command evil-elscreen/tab-close-other ()
  "Close all other tabpages. Do not kill other buffers"
  :repeat nil
  (interactive)
  (elscreen-kill-others))

(evil-define-command evil-elscreen/tab-close-other-f ()
  "Close all other tabpages and alos kill other buffers"
  :repeat nil
  (interactive)
  (elscreen-kill-others)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;;;;;;;; Switching to another tab page

(evil-define-command evil-elscreen/tab-next (&optional count argument)
  ""
  :repeat nil
  (interactive "p")
  (if elscreen-display-screen-number
      (progn
	(if (eq last-command #'evil-beginning-of-line-or-digit-argument)
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


(evil-define-command evil-elscreen/tab-previous (&optional count argument)
  ""
  :repeat nil
  (interactive "p")
  (if elscreen-display-screen-number
      (progn
	(if (eq last-command #'evil-beginning-of-line-or-digit-argument)
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


;;;;;;;; evil-mode keymaps
;; Normal map
(define-key evil-normal-state-map (kbd "gt") 'evil-elscreen/tab-next)
(define-key evil-normal-state-map (kbd "C-<next>") 'evil-elscreen/tab-next)
(define-key evil-normal-state-map (kbd "gT") 'evil-elscreen/tab-previous)
(define-key evil-normal-state-map (kbd "C-<prior>") 'evil-elscreen/tab-previous)

;; Insert map
(define-key evil-insert-state-map (kbd "C-<next>") 'evil-elscreen/tab-next)
(define-key evil-insert-state-map (kbd "C-<prior>") 'evil-elscreen/tab-previous)

;; Motion map
(define-key evil-motion-state-map (kbd "gt") 'evil-elscreen/tab-next)

;; ex command
(evil-ex-define-cmd "tabnew" 'evil-elscreen/tab-new)
(evil-ex-define-cmd "tabedit" "tabnew")
(evil-ex-define-cmd "tabe" "tabedit")
(evil-ex-define-cmd "tabfind" 'evil-elscreen/tab-find)
(evil-ex-define-cmd "tabf" "tabfind")
;;(evil-ex-define-cmd "tab" 'evil-elscreen/tab-command)
(evil-ex-define-cmd "tabclose" 'evil-elscreen/tab-close)
(evil-ex-define-cmd "tabc" "tabclose")
(evil-ex-define-cmd "tabclose!" 'evil-elscreen/tab-close-f)
(evil-ex-define-cmd "tabc!" "tabclose!")
(evil-ex-define-cmd "tabonly" 'evil-elscreen/tab-close-other)
(evil-ex-define-cmd "tabo" "tabonly")
(evil-ex-define-cmd "tabonly!" 'evil-elscreen/tab-close-other-f)
(evil-ex-define-cmd "tabo!" "tabonly!")
(evil-ex-define-cmd "tabnext" 'evil-elscreen/tab-next)
(evil-ex-define-cmd "tabn" "tabnext")
(evil-ex-define-cmd "tabprevious" 'evil-elscreen/tab-previous)
(evil-ex-define-cmd "tabp" "tabprevious")
(evil-ex-define-cmd "tabNext" "tabprevious")
(evil-ex-define-cmd "tabN" "tabprevious")


(provide 'evil-elscreen)
;; evil-elscreen.el ends here.

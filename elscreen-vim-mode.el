;;; elscreen-vim-mode.el ---

;; Copyright 2010 Yen-Chin,Lee
;;
;; Author: Yen-Chin,Lee <coldnew.tw@gmail.com>
;; Version: $Id: elscreen-vim-mode.el,v 0.0 2010/08/29 19:26:59 coldnew Exp $
;; Keywords: vim, elscreen
;; X-URL: http://www.emacswiki.org/emacs/elscreen-vim-mode.el

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

;; use elscreen as tab in vim-mode

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'elscreen-vim-mode)


;;; Code:

(provide 'elscreen-vim-mode)
(eval-when-compile
    (require 'cl))

(require 'vim)
(require 'elscreen)

;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

(vim:defcmd vim:cmd-tab-edit ((argument:file file) nonrepeatable)
 "Visit a file in new tab."
 (if file
     (elscreen-find-file file)
   (when (buffer-file-name)
     (elscreen-find-file (buffer-file-name)))
   (elscreen-create)))

(vim:defcmd vim:cmd-tab-close ((argument:char arg) (nonrepeatable)) ;
  "Close current tab."
  (elscreen-kill (string-to-number arg))
  (redraw-display))

(vim:defcmd vim:cmd-tab-close-others (nonrepeatable)
  "Close others tab."
  (elscreen-kill-others 1)
  (redraw-display))

(vim:defcmd vim:cmd-tab-next (nonrepeatable)
  "Visit next tab."
  (elscreen-next))

(vim:defcmd vim:cmd-tab-previous (nonrepeatable)
  "Visit previous tab."
  (elscreen-previous))

(vim:defcmd vim:cmd-tab-first (nonrepeatable)
  "Visit first one tab."
  (elscreen-goto 0))

;;FIXME: the last tab is 9?
(vim:defcmd vim:cmd-tab-last (nonrepeatable)
  "Visit first one tab."
  (elscreen-goto 9))

(vim:defcmd vim:cmd-tab-goto ((argument:char arg) (nonrepeatable))
  "Move to specific tab."
  (elscreen-goto (string-to-number arg)))

(vim:nmap "gt" 'vim:cmd-tab-next)
(vim:nmap "gT" 'vim:cmd-tab-previous)

(vim:emap "tabedit" 'vim:cmd-tab-edit)
(vim:emap "tabe" "tabedit")
(vim:emap "tabclose" 'vim:cmd-tab-close)
(vim:emap "tabc" "tabclose")
(vim:emap "tabonly" 'vim:cmd-tab-close-others)
(vim:emap "tabo" "tabonly")
(vim:emap "tabnext" 'vim:cmd-tab-next)
(vim:emap "tabn" "tabnext")
(vim:emap "tabprevious" 'vim:cmd-tab-previous)
(vim:emap "tabp" "tabprevious")
(vim:emap "tabrewind" 'vim:cmd-tab-first)
(vim:emap "tabr" "tabrewind")
(vim:emap "tablast" 'vim:cmd-tab-last)
(vim:emap "tabl" "tablast")
(vim:emap "tabmove" 'vim:cmd-tab-goto)
(vim:emap "tabm" "tabmove")

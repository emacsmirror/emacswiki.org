;;; minor-mode-hack.el --- Change priority of minor-mode keymaps

;; $Id: minor-mode-hack.el,v 1.6 2013/02/12 12:28:05 rubikitch Exp $

;; Copyright (C) 2010,2013  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: lisp
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/minor-mode-hack.el
;; Version: 1.6

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
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
;; A hack of minor-modes.
;;
;; `raise-minor-mode-map-alist' / `lower-minor-mode-map-alist' - resolve `minor-mode-map-alist' conflict
;;
;; Example: 
;; (raise-minor-mode-map-alist 'view-mode)
;; raises priority of `view-mode-map'.
;;
;;; Commands
;;
;; Below are complete command list:
;;
;;  `show-minor-mode-map-priority'
;;    Show priority of `minor-mode-map-alist'.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;
;;; Installation:
;;
;; Put minor-mode-hack.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'minor-mode-hack)
;;
;; No need more.


;;; History:

;; $Log: minor-mode-hack.el,v $
;; Revision 1.6  2013/02/12 12:28:05  rubikitch
;; Change short description (no code change)
;;
;; Revision 1.5  2013/02/07 02:54:39  rubikitch
;; Fix header
;;
;; Revision 1.4  2013/02/07 02:52:57  rubikitch
;; *** empty log message ***
;;
;; Revision 1.3  2013/02/07 00:16:30  rubikitch
;; autoload, copyright, more commentary
;;
;; Revision 1.2  2010/01/07 22:25:49  rubikitch
;; New command: `show-minor-mode-map-priority'
;;
;; Revision 1.1  2010/01/07 06:24:28  rubikitch
;; Initial revision
;;

;;; Code:

(defvar minor-mode-hack-version "$Id: minor-mode-hack.el,v 1.6 2013/02/12 12:28:05 rubikitch Exp $")

(defgroup minor-mode-hack nil
  "minor-mode-hack"
  :group 'emacs)

;;;###autoload
(defun raise-minor-mode-map-alist (mode-symbol)
  "Raise `minor-mode-map-alist' priority of MODE-SYMBOL."
  (let ((x (assq mode-symbol minor-mode-map-alist)))
    (and x (setq minor-mode-map-alist (cons x (delq x minor-mode-map-alist))))))

;;;###autoload
(defun lower-minor-mode-map-alist (mode-symbol)
  "Lower `minor-mode-map-alist' priority of MODE-SYMBOL."
  (let ((rel (assq mode-symbol minor-mode-map-alist)))
    (setq minor-mode-map-alist (append (delete rel minor-mode-map-alist) (list rel)))))

;;;###autoload
(defun show-minor-mode-map-priority ()
  "Show priority of `minor-mode-map-alist'."
  (interactive)
  (message "%S" (mapcar 'car minor-mode-map-alist)))

(provide 'minor-mode-hack)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "minor-mode-hack.el")
;;; minor-mode-hack.el ends here

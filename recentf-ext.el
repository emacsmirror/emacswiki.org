;;;; recentf-ext.el --- Recentf extensions
;; $Id: recentf-ext.el,v 1.1 2009/12/24 11:53:03 rubikitch Exp $

;; Copyright (C) 2009  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: convenience, files
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/recentf-ext.el

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
;; Extension of `recentf' package.
;;
;; * `dired' buffers can be handled.
;; * Switching to file buffer considers it as most recent file.

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Installation:
;;
;; Put recentf-ext.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'recentf-ext)
;;
;; No need more.

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET recentf-ext RET
;;


;;; History:

;; $Log: recentf-ext.el,v $
;; Revision 1.1  2009/12/24 11:53:03  rubikitch
;; Initial revision
;;

;;; Code:

(defvar recentf-ext-version "$Id: recentf-ext.el,v 1.1 2009/12/24 11:53:03 rubikitch Exp $")
(eval-when-compile (require 'cl))
(defgroup recentf-ext nil
  "recentf-ext"
  :group 'emacs)

(recentf-mode 1)

;;; [2009/03/01] (@* "`recentf' as most recently USED files")
(defun recentf-push-buffers-in-frame ()
  (walk-windows
   (lambda (win)
     (let ((bfn (buffer-local-value 'buffer-file-name (window-buffer win))))
       (and bfn (recentf-add-file bfn))))))
(add-to-list 'window-configuration-change-hook 'recentf-push-buffers-in-frame)

;;; [2009/12/24] (@* "`recentf' directory")
(defun recentf-add-dired-directory ()
  (recentf-add-file dired-directory))
(add-hook 'dired-mode-hook 'recentf-add-dired-directory)

(provide 'recentf-ext)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "recentf-ext.el")
;;; recentf-ext.el ends here

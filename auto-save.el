;;; auto-save.el --- Auto save files when idle

;; Filename: auto-save.el
;; Description: Auto save files when idle
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-20 09:14:21
;; Version: 0.1
;; Last-Updated: 2008-10-20 09:14:25
;;           By: Andy Stewart
;; URL:
;; Keywords: autosave
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; 自动保存的设置设置
;;

;;; Installation:
;;
;; Put auto-save.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-auto-save)
;;
;; No need more.

;;; Change log:
;;
;; 2008/10/20
;;      First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:

;; Emacs' default auto-save is stupid to generate #foo# files!
(setq auto-save-default nil)

(defun autosave-buffers ()
  (interactive)
  (let ((autosave-buffer-list))
	(save-excursion
	 (dolist (buf (buffer-list))
	   (set-buffer buf)
	   (if (and (buffer-file-name) (buffer-modified-p))
		   (progn
			 (push (buffer-name) autosave-buffer-list)
			 (basic-save-buffer))))
	 ;; Tell user when auto save files.
	 (cond
	  ;; It's stupid tell user if nothing to save.
	  ((= (length autosave-buffer-list) 1)
	   (message "# Saved %s" (car autosave-buffer-list)))
	  ((> (length autosave-buffer-list) 1)
	   (message "# Saved %d files: %s"
		     (length autosave-buffer-list)
		     (mapconcat 'identity autosave-buffer-list ", "))))
	 )))

(run-with-idle-timer 1 t #'autosave-buffers)

(provide 'auto-save)

;;; auto-save.el ends here

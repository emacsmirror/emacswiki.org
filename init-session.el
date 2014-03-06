;;; init-session.el --- Init for session save/restore

;; Filename: init-session.el
;; Description: Init for session save/restore
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-28 01:19:38
;; Version: 0.1
;; Last-Updated: 2013-12-28 01:19:38
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-session.el
;; Keywords:
;; Compatibility: GNU Emacs 24.3.50.1
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
;; Init for session save/restore
;;

;;; Installation:
;;
;; Put init-session.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-session)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-session RET
;;

;;; Change log:
;;
;; 2013/12/28
;;      * First released.
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

(require 'windows)
(require 'revive)
(require 'auto-save)

;;; Code:

(defun kill-unused-buffers ()
  (ignore-errors (kill-buffer "*GNU Emacs*"))
  (ignore-errors (kill-buffer "*scratch*"))
  )

(defun emacs-session-restore ()
  (if (file-exists-p "~/.emacs.d/deepin-emacs/Configure-File/Windows/windows-configure")
      (resume-windows 'a))
  (kill-unused-buffers)
  )

(defun emacs-session-save ()
  "Exit emacs."
  (interactive)
  (ignore-errors
    (epc:stop-epc minibuffer-tray-epc)
    (kill-epc-buffers)
    (kill-unused-buffers)
    (auto-save-buffers)
    (make-directory "~/.emacs.d/deepin-emacs/Configure-File/Windows/" t)
    (see-you-again)))

(defun kill-epc-buffers ()
  (interactive)
  (dolist (buf (buffer-list))
    (save-excursion
      (with-current-buffer buf
        (if (string-prefix-p "*epc" (buffer-name buf))
            (ignore-errors (kill-buffer buf)))
        )))
  )

;;; ### Windows ###
;;; --- 用于保存和管理窗口的配置方案
(win:startup-with-window)
(setq win:configuration-file "~/.emacs.d/deepin-emacs/Configure-File/Windows/windows-configure") ;窗口布局管理保存文件

;;; ### Revive ###
;;; --- 用于记录恢复特定窗口配置方案
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)
(setq revive:configuration-file "~/.emacs.d/deepin-emacs/Configure-File/Revive/revive-configure") ;窗口布局设置保存文件

(provide 'init-session)

;;; init-session.el ends here

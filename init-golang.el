;;; init-golang.el --- Extensions for go lang mode

;; Filename: init-golang.el
;; Description: Extensions for go lang mode
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-10-16 22:38:54
;; Version: 0.1
;; Last-Updated: 2013-10-16 22:38:54
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-golang.el
;; Keywords:
;; Compatibility: GNU Emacs 24.2.1
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
;; Extensions for go lang mode
;;

;;; Installation:
;;
;; Put init-golang.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-golang)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-golang RET
;;

;;; Change log:
;;
;; 2013/10/16
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

;;; Code:

(defun go-run-buffer()
  (interactive)
  (shell-command (concat "go run " (buffer-name))))

(defun go-kill()
  (interactive)
  (if (go-mode-in-string)
      (paredit-kill-line-in-string)
    (paredit-kill)))

(defun go-backward-delete()
  (interactive)
  (if (go-mode-in-string)
      (paredit-backward-delete-in-string)
    (paredit-backward-delete)))

(add-hook 'go-mode-hook
          (lambda ()
            (linum-mode 1)
            (flymake-mode 1)
            (auto-complete-mode 1)
            (add-to-list 'ac-sources 'ac-source-go)
            (call-process "gocode" nil nil nil "-s")))

(provide 'init-golang)

;;; init-golang.el ends here

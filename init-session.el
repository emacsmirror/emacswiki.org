;;; init-session.el --- Init for session save/restore

;; Filename: init-session.el
;; Description: Init for session save/restore
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-28 01:19:38
;; Version: 0.4
;; Last-Updated: 2018-06-20 05:59:41
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-session.el
;; Keywords:
;; Compatibility: GNU Emacs 24.3.50.1
;;
;; Features that might be required by this library:
;;
;; `basic-tookit' `auto-save'
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
;; 2018/06/20
;;      * Make `desktop.el' don't ask user if found file locked by other emacs process.
;;
;; 2018/06/14
;;      * Use `desktop.el' instead window.el and revive.el, those two libraries buggy.
;;      * Move `kill-unused-buffers' to package `basic-tookit.el'
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
(require 'auto-save)
(require 'basic-toolkit)

;;; Code:

(setq desktop-load-locked-desktop t) ;don't popup dialog ask user, load anyway

(defun emacs-session-restore ()
  "Restore emacs session."
  (interactive)
  (ignore-errors
    ;; Kill unused buffers.
    (kill-unused-buffers)
    ;; Restore session.
    (desktop-read "~/.emacs.d/")
    ))

(defun emacs-session-save ()
  "Save emacs session."
  (interactive)
  (ignore-errors
    ;; Kill unused buffers.
    (kill-unused-buffers)
    ;; Save all buffers before exit.
    (auto-save-buffers)
    ;; Save session.
    (make-directory "~/.emacs.d/" t)
    (desktop-save "~/.emacs.d/")
    ;; Exit emacs.
    (kill-emacs)
    ))

(provide 'init-session)

;;; init-session.el ends here

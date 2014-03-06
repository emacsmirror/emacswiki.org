;;; init-yaoddmuse.el --- Init yaoddmuse

;; Filename: init-yaoddmuse.el
;; Description: Init yaoddmuse
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-30 14:53:13
;; Version: 0.1
;; Last-Updated: 2013-12-30 14:53:13
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-yaoddmuse.el
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
;; Init yaoddmuse
;;

;;; Installation:
;;
;; Put init-yaoddmuse.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-yaoddmuse)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-yaoddmuse RET
;;

;;; Change log:
;;
;; 2013/12/30
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

(require 'yaoddmuse)

;;; Code:

(setq yaoddmuse-browse-function 'yaoddmuse-browse-page-in-w3m)  ;设置yaoddmuse浏览函数
(setq yaoddmuse-notify-function 'yaoddmuse-notify-popup-window) ;设置yaoddmuse提示函数
(setq yaoddmuse-wikis                                           ;只更新 EmacsWiki
      '(("EmacsWiki" "http://www.emacswiki.org/cgi-bin/emacs" utf-8 "uihnscuskc=1;")))

(defvar one-key-menu-yaoddmuse-alist nil
  "The `one-key' menu alist for YAODDMUSE.")

(setq one-key-menu-yaoddmuse-alist
      '(
        ;; Edit.
        (("e" . "Edit Default") . yaoddmuse-edit-default)
        (("E" . "Edit") . yaoddmuse-edit)
        (("o" . "Follow") . yaoddmuse-follow)
        ;; Post
        (("b" . "Post Current Buffer") . yaoddmuse-post-current-buffer)
        (("B" . "Post Buffer") . yaoddmuse-post-buffer)
        (("l" . "Post Library Default") . yaoddmuse-post-library-default)
        (("L" . "Post Library") . yaoddmuse-post-library)
        (("f" . "post File Default") . yaoddmuse-post-file-default)
        (("F" . "Post File") . yaoddmuse-post-file)
        (("y" . "Post Screenshot Default") . yaoddmuse-post-screenshot-default)
        (("Y" . "Post Screenshot") . yaoddmuse-post-screenshot)
        ;; View.
        (("v" . "Browse Page Default") . yaoddmuse-browse-page-default)
        (("V" . "Browse Page") . yaoddmuse-browse-page)
        (("s" . "Brose This Page") . yaoddmuse-browse-current-page)
        ;; Misc
        (("d" . "Delete Page") . yaoddmuse-delete)
        (("D" . "Redirect Page") . yaoddmuse-redirect)
        (("r" . "Revert") . yaoddmuse-revert)
        (("i" . "Insert Pagename") . yaoddmuse-insert-pagename)
        (("x" . "Insert File Content") . yaoddmuse-insert-file-content)
        (("K" . "Kill Url") . yaoddmuse-kill-url)
        (("j" . "Anything Edit or View") . anything-yaoddmuse-emacswiki-edit-or-view)
        (("k" . "Anything Post") . anything-yaoddmuse-emacswiki-post-library)
        ))

(defun one-key-menu-yaoddmuse ()
  "The `one-key' menu for YAODDMUSE."
  (interactive)
  (one-key-menu "YAODDMUSE" one-key-menu-yaoddmuse-alist t))

(provide 'init-yaoddmuse)

;;; init-yaoddmuse.el ends here

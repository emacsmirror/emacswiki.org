;;; init-helm.el --- Init helm

;; Filename: init-helm.el
;; Description: Init helm
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-30 16:23:29
;; Version: 0.4
;; Last-Updated: 2018-06-28 08:01:42
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-helm.el
;; Keywords:
;; Compatibility: GNU Emacs 24.3.50.1
;;
;; Features that might be required by this library:
;;
;; `helm' `helm-buffers'
;; `helm-c-yasnippet' `helm-for-files'
;; `helm-projectile' `helm-ring'
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
;; Init helm
;;

;;; Installation:
;;
;; Put init-helm.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-helm)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-helm RET
;;

;;; Change log:
;;
;; 2018/06/29
;;      * Remove `helm-autoload-commands', one-key is enough.
;;
;; 2018/06/28
;;      * Update some key of helm-map.
;;
;; 2018/06/14
;;      * Upgrade configuration to match git version of helm.
;;      * Just add helm-source-projectile-* in list when current place in project, avoid `helm-source-projectile-files-list' block `helm-dwim'.
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
(require 'helm)
(require 'helm-buffers)
(require 'helm-c-yasnippet)
(require 'helm-for-files)
(require 'helm-projectile)
(require 'helm-ring)

;;; Code:

(defun helm-dwim ()
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil)
        helm-source-list)
    (cond (
           ;; Just add helm-source-projectile-* in list when current place in project.
           (projectile-project-p)
           (setq helm-source-list
                 '(
                   helm-source-buffers-list
                   helm-source-recentf
                   helm-source-projectile-buffers-list
                   helm-source-projectile-files-list
                   helm-source-projectile-projects
                   helm-source-kill-ring
                   helm-source-yasnippet
                   )))
          (t
           (setq helm-source-list
                 '(
                   helm-source-buffers-list
                   helm-source-recentf
                   helm-source-kill-ring
                   helm-source-yasnippet
                   ))
           ))
    (helm-other-buffer helm-source-list "*helm search*")))

(lazy-set-key
 '(
   ("M-s-j" . helm-next-source)
   ("M-s-k" . helm-previous-source)
   ("M->" . helm-scroll-other-window-down)
   ("M-<" . helm-scroll-other-window)
   ("M-o" . backward-delete-char-untabify)
   )
 helm-map)

(provide 'init-helm)

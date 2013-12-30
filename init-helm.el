;;; init-helm.el --- Init helm

;; Filename: init-helm.el
;; Description: Init helm
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-30 16:23:29
;; Version: 0.1
;; Last-Updated: 2013-12-30 16:23:29
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-helm.el
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

(require 'helm-files)
(require 'helm-config)
(require 'helm-helm-commands)

;;; Code:

(defun helm-dwim ()
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm-other-buffer
     '(
       helm-source-buffers-list
       helm-source-recentf
       helm-source-locate
       helm-source-files-in-current-dir
       helm-source-moccur
       helm-source-occur
       helm-source-helm-commands
       )
     "*helm search*")))

(lazy-set-key
 '(
   ("M-s-j" . helm-next-source)
   ("M-s-k" . helm-previous-source)
   )
 helm-map)

(provide 'init-helm)

;;; init-helm.el ends here

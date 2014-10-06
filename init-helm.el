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

(require 'ag)
(require 'helm-files)
(require 'helm-config)
(require 'helm-helm-commands)
(require 'helm-c-yasnippet)
(require 'helm-apt)
(require 'helm-autoload-commands)
(require 'helm-ls-git)
(require 'helm-descbinds)
(require 'helm-webkit)
(require 'helm-ring)
(require 'helm-imenu)
(require 'helm-man)
(require 'helm-ag)
(require 'apt-utils)

;;; Code:

(setq helm-apt-cache-show-function
      '(lambda (package)
         (require 'init-apt-utils)
         (apt-utils-show-package-1 package)))

(defun helm-dwim ()
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm-other-buffer
     '(
       helm-source-findutils
       helm-source-buffers-list
       helm-source-recentf
       helm-source-occur
       helm-source-do-ag
       helm-source-locate
       helm-source-kill-ring
       helm-source-imenu
       helm-source-autoload-commands
       helm-source-ls-git
       helm-c-source-yasnippet
       helm-source-webkit
       helm-source-apt
       helm-source-man-pages
       )
     "*helm search*")))

(lazy-set-key
 '(
   ("M-s-j" . helm-next-source)
   ("M-s-k" . helm-previous-source)
   )
 helm-map)

(setq helm-autoload-commands-list
      '(
        ("inf-ruby" "Run an inferior Ruby process in a buffer." "inf-ruby")
        ("yaoddmuse-post-library-default" "Post elisp library to EmacsWiki" "yaoddmuse-extension")
        ("yaoddmuse-edit-default" "Edit EmacsWiki page" "yaoddmuse-extension")
        ("auto-install-from-emacswiki" "Install package from EmacsWiki.org" "auto-install")
        )
      )

(provide 'init-helm)

;;; init-helm.el ends here

;;; init-helm.el --- Init helm

;; Filename: init-helm.el
;; Description: Init helm
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-30 16:23:29
;; Version: 1.0
;; Last-Updated: 2018-11-01 20:53:46
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-helm.el
;; Keywords:
;; Compatibility: GNU Emacs 24.3.50.1
;;
;; Features that might be required by this library:
;;
;; `helm' `helm-buffers'
;; `helm-c-yasnippet' `helm-for-files'
;; `helm-ring'
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
;; In Mac OS, you perhaps need execute below command to build locate db:
;;
;; sudo launchctl load -w /System/Library/LaunchDaemons/com.apple.locate.plist
;;

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-helm RET
;;

;;; Change log:
;;
;; 2018/11-01
;;      * Fix `helm-ls-git' not work when first run `helm-dwim'.
;;
;; 2018/10-29
;;      * Use `helm-ls-git' instead `helm-projectile', helm so fast now!!!
;;
;; 2018/09/03
;;      * Make filename has enough width to display full name.
;;
;; 2018/07/26
;;      * Make `helm-source-buffers-list' first, otherwise can't switch to with exist buffer list.
;;
;; 2018/07/12
;;      * Add `helm-source-locate' in `helm-source-list' for search file in file system.
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
(require 'helm-x-files)
(require 'helm-ls-git)
(require 'helm-ring)
(require 'awesome-tab)

;;; Code:

(setq helm-buffer-max-length 60) ; make filename has enough width to display full name
(awesome-tab-build-helm-source)

(defvar helm-source-elisp-library
  (helm-build-in-buffer-source  "Elisp libraries (Scan)"
    :data #'helm-locate-library-scan-list
    :fuzzy-match helm-locate-library-fuzzy-match
    :keymap helm-generic-files-map
    :search (unless helm-locate-library-fuzzy-match
              (lambda (regexp)
                (re-search-forward
                 (if helm-ff-transformer-show-only-basename
                     (replace-regexp-in-string
                      "\\`\\^" "" regexp)
                   regexp)
                 nil t)))
    :match-part (lambda (candidate)
                  (if helm-ff-transformer-show-only-basename
                      (helm-basename candidate) candidate))
    :filter-one-by-one (lambda (c)
                         (if helm-ff-transformer-show-only-basename
                             (cons (helm-basename c) c) c))
    :action (helm-actions-from-type-file)))

;; MacOS use spotlight instead locate.
(defvar helm-source-system
  (if (featurep 'cocoa)
      helm-source-mac-spotlight
    helm-source-locate))

(defun helm-dwim ()
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil)
        helm-source-list)
    (unless helm-source-buffers-list
      (setq helm-source-buffers-list
            (helm-make-source "Buffers" 'helm-source-buffers)))
    (unless helm-source-ls-git
      (setq helm-source-ls-git
            (helm-ls-git-build-ls-git-source)))
    (setq helm-source-list
          '(
            helm-source-awesome-tab-group
            helm-source-buffers-list
            helm-source-ls-git
            helm-source-recentf
            helm-source-kill-ring
            helm-source-system
            helm-source-elisp-library
            helm-source-yasnippet
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

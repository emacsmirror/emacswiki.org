;;; init-projectile-rails.el --- Config for projectile-rails

;; Filename: init-projectile-rails.el
;; Description: Config for projectile-rails
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-12 21:24:21
;; Version: 0.3
;; Last-Updated: 2018-07-07 09:19:09
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-projectile-rails.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `projectile' `projectile-rails' `rake' `rails-lib'
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
;; Config for projectile-rails
;;

;;; Installation:
;;
;; Put init-projectile-rails.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-projectile-rails)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-projectile-rails RET
;;

;;; Change log:
;;
;; 2018/07/07
;;      * Move function `moccur-grep-in-rails-app-directory' to `moccur-extension.el'.
;;
;; 2018/06/13
;;      * Add command `moccur-grep-in-rails-app-directory'
;;
;; 2018/06/12
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
(require 'projectile)
(require 'projectile-rails)
(require 'rake)
(require 'rails-lib)
(require 'moccur-extension)

;;; Code:

(defvar one-key-projectile-rails-alist nil
  "The `one-key' menu alist for DIRECTORY.")

(setq one-key-projectile-rails-alist
      '(
        (("f" . "Ruby File") . (lambda () (interactive) (projectile-rails-ruby-goto-file-at-point)))
        (("d" . "File DWIM") . (lambda () (interactive) (projectile-find-file-dwim)))
        (("r" . "Routes") . (lambda () (interactive) (projectile-rails-goto-routes)))
        (("g" . "Gemfile") . (lambda () (interactive) (projectile-rails-goto-gemfile)))
        (("t" . "Template") . (lambda () (interactive) (projectile-rails-goto-template-at-point)))
        (("s" . "Seeds") . (lambda () (interactive) (projectile-rails-goto-seeds)))
        (("h" . "Helper") . (lambda () (interactive) (projectile-rails-find-current-helper)))
        (("v" . "Current View") . (lambda () (interactive) (projectile-rails-find-current-view)))
        (("c" . "Current Controller") . (lambda () (interactive) (projectile-rails-find-current-controller)))
        (("m" . "Current Model") . (lambda () (interactive) (projectile-rails-find-current-model)))
        (("V" . "View") . (lambda () (interactive) (projectile-rails-find-view)))
        (("C" . "Controller") . (lambda () (interactive) (projectile-rails-find-controller)))
        (("M" . "Model") . (lambda () (interactive) (projectile-rails-find-model)))
        (("k" . "Kill Buffers") . (lambda () (interactive) (projectile-kill-buffers)))
        (("p" . "Grep Project") . (lambda () (interactive) (moccur-grep-in-rails-app-directory)))
        (("P" . "Visit Project") . (lambda () (interactive) (projectile-dired)))
        ))

(defun one-key-projectile-rails ()
  "The `one-key' menu for DIRECTORY."
  (interactive)
  (one-key-menu "PROJECTILE RAILS" one-key-projectile-rails-alist t))

(provide 'init-projectile-rails)

;;; init-projectile-rails.el ends here

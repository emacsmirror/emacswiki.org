;;; projectile-rails-extension.el --- Some extension for projectile-rails

;; Filename: projectile-rails-extension.el
;; Description: Some extension for projectile-rails
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-13 08:41:41
;; Version: 0.1
;; Last-Updated: 2018-06-13 08:41:41
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/projectile-rails-extension.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `projectile-rails' `dired-extension'
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
;; Some extension for projectile-rails
;;

;;; Installation:
;;
;; Put projectile-rails-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'projectile-rails-extension)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET projectile-rails-extension RET
;;

;;; Change log:
;;
;; 2018/06/13
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
(require 'projectile-rails)
(require 'dired-extension)

;;; Code:
(defun moccur-grep-in-rails-app-directory (&optional content)
  (interactive)
  (unless content
    (setq content (read-regexp "Regexp to search: ")))
  (moccur-grep-find
   (concat (projectile-project-root) "app")
   (moccur-split-string (concat "\\b" (replace-regexp-in-string  "\\s-+" "\\\\s-" content) "\\b"))))

(provide 'projectile-rails-extension)

;;; projectile-rails-extension.el ends here

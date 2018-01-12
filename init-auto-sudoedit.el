;;; init-auto-sudoedit.el --- Init for auto-sudoedit.el

;; Filename: init-auto-sudoedit.el
;; Description: Init for auto-sudoedit.el
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-01-12 15:16
;; Version: 0.1
;; Last-Updated: 2018-01-12 15:16
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-auto-sudoedit.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
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
;; Init for auto-sudoedit.el
;;

;;; Installation:
;;
;; Put init-auto-sudoedit.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-auto-sudoedit)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-auto-sudoedit RET
;;

;;; Change log:
;;
;; 2018/01/12
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
(require 'auto-sudoedit)

;; Just hook on `find-file-hook', don't hook `dired-mode-hook', it's unnecessary.
(add-hook 'find-file-hook  'auto-sudoedit)

(provide 'init-auto-sudoedit)
;;; init-auto-sudoedit.el ends here

;;; init-flymake.el --- Flymake configuration

;; Filename: init-flymake.el
;; Description: Flymake configuration
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-20 09:43:11
;; Version: 0.1
;; Last-Updated: 2008-10-20 09:43:11
;;           By: Andy Stewart
;; URL:
;; Keywords: flymake
;; Compatibility: GNU Emacs 23.0.60.1
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
;; Flymake configuration
;;

;;; Installation:
;;
;; Put init-flymake.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-flymake)
;;
;; No need more.

;;; Change log:
;;
;; 2008/10/20
;;      First released.
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

;; flymake
(dolist (hook (list
               'haskell-mode-hook
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               ))
  (add-hook hook 'flymake-find-file-hook))
;; flymake-shell
(add-hook 'sh-mode-hook 'flymake-shell-load)
;; flymake extension
(setq flymake-extension-use-showtip t)  ;use `shotip' display error or warning.

(provide 'init-flymake)

;;; init-flymake.el ends here

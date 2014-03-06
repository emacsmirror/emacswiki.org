;;; ipython-extension.el --- Some extension for ipython

;; Filename: ipython-extension.el
;; Description: Some extension for ipython
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-11-24 09:53:45
;; Version: 0.1
;; Last-Updated: 2008-11-24 09:53:49
;;           By: Andy Stewart
;; URL:
;; Keywords: ipython, python
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `ipython' `lazycat-toolkit'
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
;; Some extension for ipython
;;

;;; Installation:
;;
;; Put ipython-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'ipython-extension)
;;
;; No need more.

;;; Change log:
;;
;; 2008/11/24
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
(require 'ipython)
(require 'lazycat-toolkit)

;;; Code:

(defadvice py-execute-buffer (around python-keep-focus protect)
  "This advice to make focus python source code after execute command `py-execute-buffer'."
  (save-selected-window
    ad-do-it))

(add-hook 'py-shell-hook 'handler-buffer-exit-close)

(provide 'ipython-extension)

;;; ipython-extension.el ends here

;;; LocalWords:  ipython utilties py

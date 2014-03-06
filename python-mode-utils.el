;;; python-mode-utils.el --- Utils for python-mode

;; Filename: python-mode-utils.el
;; Description: Utils for python-mode
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2012, Andy Stewart, all rights reserved.
;; Created: 2012-11-10 09:01:38
;; Version: 0.1
;; Last-Updated: 2012-11-10 09:01:38
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/python-mode-utils.el
;; Keywords: python, utils
;; Compatibility: GNU Emacs 24.0.94.2
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
;; Utils for python-mode
;;

;;; Installation:
;;
;; Put python-mode-utils.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'python-mode-utils)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET python-mode-utils RET
;;

;;; Change log:
;;
;; 2012/11/10
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


;;; Code:

(defun jump-to-import()
  (interactive)
  ;; Rember position before jump.
  (remember-init)
  ;; Jump to `import ...` position.
  (goto-char (point-min))
  (search-forward-regexp "\\(^import\\|^from\\)" nil t)
  )

(provide 'python-mode-utils)

;;; python-mode-utils.el ends here

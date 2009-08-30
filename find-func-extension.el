;;; find-func-extension.el --- Some extension functions for `find-func.el'.

;; Filename: find-func-extension.el
;; Description: Some extension functions for `find-func.el'.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-02-07 20:20:08
;; Version: 0.1
;; Last-Updated: 2009-02-07 20:20:08
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/find-func-extension.el
;; Keywords: find-func, define
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
;; Some extension functions for `find-func.el'.
;;

;;; Installation:
;;
;; Put find-func-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'find-func-extension)
;;
;; No need more.

;;; Change log:
;;
;; 2009/02/07
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

(defun find-function-or-variable-at-point (&optional prefix)
  "Find function or variable define at current point."
  (interactive "P")
  (if (equal 0 (variable-at-point))     ;if have not variable at current point
      (if (function-called-at-point)    ;if have function call at current point
          (call-interactively (if (null prefix) 'find-function 'find-function-other-window))
        (if (face-at-point)             ;if have face define at current point
            (call-interactively (if (null prefix) 'find-face-definition 'find-face-definition-other-window))
          (message "Nothing at point.")))
    (call-interactively (if (null prefix) 'find-variable 'find-variable-other-window))))

(defun find-face-definition-other-window (face)
  "Find FACE definition at other window."
  (interactive (find-function-read 'defface))
  (find-function-do-it face 'defface 'switch-to-buffer-other-window))

(provide 'find-func-extension)

;;; find-func-extension.el ends here

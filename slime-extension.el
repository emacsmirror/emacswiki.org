;;; slime-extension.el --- Extension for slime mode

;; Filename: slime-extension.el
;; Description: Extension for slime mode
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-09-26 21:21:12
;; Version: 0.1
;; Last-Updated: 2013-09-26 21:21:12
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/slime-extension.el
;; Keywords:
;; Compatibility: GNU Emacs 24.2.1
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
;; Extension for slime mode
;;

;;; Installation:
;;
;; Put slime-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'slime-extension)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET slime-extension RET
;;

;;; Change log:
;;
;; 2013/09/26
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
(require 'slime)

;;; Code:

(defun slime-load-current-file ()
  (interactive)
  (save-buffer)
  (slime-load-file buffer-file-name)
  (slime-switch-to-output-buffer))

(provide 'slime-extension)

;;; slime-extension.el ends here

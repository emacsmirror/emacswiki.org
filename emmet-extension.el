;;; emmet-extension.el --- Extension for emmet

;; Filename: emmet-extension.el
;; Description: Extension for emmet
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-12 08:41:27
;; Version: 0.1
;; Last-Updated: 2018-06-12 08:41:27
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/emmet-extension.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `emmet-mode'
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
;; Extension for emmet
;;

;;; Installation:
;;
;; Put emmet-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'emmet-extension)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET emmet-extension RET
;;

;;; Change log:
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
(require 'emmet-mode)

;;; Code:

(defun emmet-preview-current-line ()
  "Preview emmet code and don't need move cursor."
  (interactive)
  (let (line-start-pos line-end-pos)
    (save-excursion
      (back-to-indentation)
      (setq line-start-pos (point))
      (move-end-of-line 1)
      (setq line-end-pos (point))
      )
    (emmet-preview line-start-pos line-end-pos)
    ))

(provide 'emmet-extension)

;;; emmet-extension.el ends here

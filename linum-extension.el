;;; linum-extension.el --- Extension for linum mode

;; Filename: linum-extension.el
;; Description: Extension for linum mode
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-10-06 11:52:32
;; Version: 0.1
;; Last-Updated: 2014-10-06 11:52:32
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/linum-extension.el
;; Keywords:
;; Compatibility: GNU Emacs 24.4.50.1
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
;; Extension for linum mode
;;

;;; Installation:
;;
;; Put linum-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'linum-extension)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET linum-extension RET
;;

;;; Change log:
;;
;; 2014/10/06
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

(require 'linum)

;;; Code:

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input.
This function will detect current linum-mode status, it won't disable linum-mode if current buffer has enable it."
  (interactive)
  (let ((linum-mode-p linum-mode))
    (unwind-protect
        (progn
          (linum-mode 1)
          (goto-line (read-number "Goto line: ")))
      (unless linum-mode-p
        (linum-mode -1))
      )))

(provide 'linum-extension)

;;; linum-extension.el ends here

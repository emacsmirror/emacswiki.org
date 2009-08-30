;;; wdired-extension.el --- Some extension functions for `wdired.el'.

;; Filename: wdired-extension.el
;; Description: Some extension functions for `wdired.el'.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-02-09 12:23:03
;; Version: 0.1
;; Last-Updated: 2009-02-09 12:23:03
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/wdired-extension.el
;; Keywords: wdired, format filename
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `rect-extension' `rect-mark' `dired-extension'
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
;; Some extension functions for `wdired.el'.
;;

;;; Installation:
;;
;; Put wdired-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'wdired-extension)
;;
;; No need more.

;;; Change log:
;;
;; 2009/02/09
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
(require 'rect-extension)
(require 'rect-mark)
(require 'dired-extension)

;;; Code:

(defun wdired-format-filename ()
  "Format filename with `wdired-mode' in dired."
  (interactive)
  (save-excursion
    (dired-move-to-first-file)
    (call-interactively 'rm-set-mark)
    (dired-move-to-last-file)
    (mark-rectangle-to-end)
    (execute-command-with-region-replace 'rte-format-filename)))

(provide 'wdired-extension)

;;; wdired-extension.el ends here


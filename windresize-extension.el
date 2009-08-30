;;; windresize-extension.el --- Some extension functions for `windresize.el'

;; Filename: windresize-extension.el
;; Description: Some extension functions for `windresize.el'
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-02-07 22:06:46
;; Version: 0.1
;; Last-Updated: 2009-02-07 22:06:46
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/windresize-extension.el
;; Keywords: windresize
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `windresize'
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
;; Some extension functions for `windresize.el'
;;

;;; Installation:
;;
;; Put windresize-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'windresize-extension)
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
(require 'windresize)

;;; Code:


(defun outward-window ()
  "Outward enlarge current window."
  (interactive)
  (windresize-up-inwards '-1)
  (windresize-down-inwards '-1)
  (windresize-right-inwards '-1)
  (windresize-left-inwards '-1))

(defun inward-window ()
  "Inward shrink current window."
  (interactive)
  (windresize-up-inwards '1)
  (windresize-down-inwards '1)
  (windresize-right-inwards '1)
  (windresize-left-inwards '1))

(provide 'windresize-extension)

;;; windresize-extension.el ends here


;;; festival-extension.el --- Simple festival extension for festival.el

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-07-19 22:37:16
;; Version: 0.1
;; Last-Updated: 2008-07-19 22:37:21
;; URL: not distributed yet
;; Keywords: festival
;; Compatibility: GNU Emacs 23.0.60.1

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

;; Features that might be required by this library:
;;
;;  `festival', `thingatpt'
;;

;;; Installation:
;;
;; Copy festival-extension.el to your load-path and add to your ~/.emacs
;;
;;  (require 'festival-extension)
;;
;; No need more

;;; Commentary:
;;
;; Some simple extension for festival.el
;;

;;; Change log:
;;
;; 2008/07/19
;;         First release
;;

;;; Acknowledgments:
;;
;; Not yet
;;

;;; TODO
;;
;; None
;;

;;; Require
(require 'festival)
(require 'thingatpt)

;;; Code:

(defun festival-read-word()
  "Read current word that at point by Festival."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (festival-say word)
    (message "Read: %s" word)))

(provide 'festival-extension)

;;; festival-extension.el ends here

;;; LocalWords:  thingatpt

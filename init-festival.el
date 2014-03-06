;;; init-festival.el --- Init festival

;; Filename: init-festival.el
;; Description: Init festival
;; Author: Andy Stewart <andy@freedom>
;; Maintainer: Andy Stewart <andy@freedom>
;; Copyright (C) 2013, Andy Stewart, all rights reserved.
;; Created: 2013-12-30 14:57:59
;; Version: 0.1
;; Last-Updated: 2013-12-30 14:57:59
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-festival.el
;; Keywords: 
;; Compatibility: GNU Emacs 24.3.50.1
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
;; Init festival
;; 

;;; Installation:
;;
;; Put init-festival.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-festival)
;;
;; No need more.

;;; Customize:
;;
;; 
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-festival RET
;;

;;; Change log:
;;	
;; 2013/12/30
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

(require 'festival)
(require 'festival-extension)

;;; Code:

(defvar one-key-menu-festival-alist nil
  "The `one-key' menu alist for FESTIVAL.")

(setq one-key-menu-festival-alist
      '(
        (("s" . "Stop") . festival-stop)
        (("a" . "Say") . festival-say)
        (("f" . "Read File") . festival-read-file)
        (("b" . "Read Buffer") . festival-read-buffer)
        (("r" . "Read Region") . festival-read-region)
        (("w" . "Read Word") . festival-read-word)))

(defun one-key-menu-festival ()
  "The `one-key' menu for FESTIVAL."
  (interactive)
  (one-key-menu "FESTIVAL" one-key-menu-festival-alist t))

(provide 'init-festival)

;;; init-festival.el ends here

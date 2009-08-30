;;; newsticker-notify.el --- Notify newsticker news in mode-line

;; Filename: newsticker-notify.el
;; Description: Notify newsticker news in mode-line
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-12-18 09:19:01
;; Version: 0.1.1
;; Last-Updated: 2008-12-19 17:40:16
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/newsticker-notify.el
;; Keywords: newsticker, notify
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
;; Notify newsticker news in mode-line
;;
;; NOTE:
;;      This package just support Newsticker 1.99 or later version.
;;      So be sure you have Newsticker 1.99 or high.

;;; Installation:
;;
;; Put newsticker-notify.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'newsticker-notify)
;;
;; No need more.

;;; Customize:
;;
;; `newsticker-notify-modeline-face' is a face for notify.
;; `newsticker-notify-modeline-string' is a string for notify.
;;
;; All above can customize easy through:
;;      M-x customize-group RET newsticker-notify RET
;;

;;; Change log:
;;
;; 2008/12/19
;;      Use hook `newsticker-new-item-functions' instead timer.
;;
;; 2008/12/18
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


;;; Code:

(defgroup newsticker-notify nil
  "Notify newsticker news in mode-line."
  :group 'newsticker)

(defface newsticker-notify-modeline-face
  '((((class color) (background dark))
     (:foreground "Gold")))
  "Face for highlighting newsticker news number in mode line"
  :group 'newsticker-notify)

(defcustom newsticker-notify-modeline-string "Newsticker"
  "The notify string showing up in modeline."
  :type 'string
  :group 'newsticker-notify)

(defvar newsticker-notify-string nil
  "The highlight string for `newsticker-notify'.")

(unless (member 'newsticker-notify-string global-mode-string)
  (setq global-mode-string (append global-mode-string
                                   (list 'newsticker-notify-string))))

(defun newsticker-notify (&rest arg-list)
  "Update newsticker news and display notify message in mode-line."
  (interactive)
  (when (newsticker-running-p)
    (let (newsticker-news-number)
      (force-mode-line-update t)
      (setq newsticker-news-number (newsticker--stat-num-items-total 'new))
      (if (> newsticker-news-number 0)
          (progn
            ;; the property `risky-local-variable' is a security measure for mode line
            ;; variable that have properties
            (put 'newsticker-notify-string 'risky-local-variable t)
            (setq newsticker-notify-string
                  (propertize (format " [%s: %d]" newsticker-notify-modeline-string newsticker-news-number)
                              'face 'newsticker-notify-modeline-face)))
        (setq newsticker-notify-string "")))))

;; Add hook to `newsticker-new-item-functions' for update.
(add-hook 'newsticker-new-item-functions 'newsticker-notify)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Advice ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice newsticker-treeview-quit (after newsticker-update-news activate)
  "Update news when quit from treeview."
  (newsticker-notify))

(provide 'newsticker-notify)

;;; newsticker-notify.el ends here

;;; LocalWords:  newsticker modeline num treeview

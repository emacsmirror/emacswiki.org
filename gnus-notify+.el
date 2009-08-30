;;; gnus-notify+.el --- Gnus notify

;; Filename: gnus-notify+.el
;; Description: Gnus notify
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-12-08 16:37:00
;; Version: 0.2
;; Last-Updated: 2008-12-18 09:15:59
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/gnus-notify+.el
;; Keywords: gnus, notify
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
;; Gnus notify
;;

;;; Installation:
;;
;; Put gnus-notify+.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'gnus-notify+)
;;
;; I recommended add `gnus-notify+' with below hooks:
;;
;; (add-hook 'gnus-summary-exit-hook 'gnus-notify+)
;; (add-hook 'gnus-group-catchup-group-hook 'gnus-notify+)
;; (add-hook 'mail-notify-pre-hook 'gnus-notify+)
;;

;;; Customize:
;;
;; `gnus-notify+-modeline-face' is a face for notify.
;; `gnus-notify+-modeline-string' is notify string.
;; `gnus-notify+-timeout' is refresh cycle for notify (seconds).
;;
;; All above can customize easy through:
;;      M-x customize-group RET gnus-notify+ RET
;;

;;; Change log:
;;
;; 2008/12/08
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

(defgroup gnus-notify+ nil
  "Display new notify message in modeline"
  :group 'gnus)

(defface gnus-notify+-modeline-face
  '((((class color) (background dark))
     (:foreground "Gold")))
  "Face for highlighting gnus news number in mode line"
  :group 'gnus-notify+)

(defcustom gnus-notify+-modeline-string "Gnus"
  "The string that notify in modeline."
  :type 'string
  :group 'gnus-notify+)

(defcustom gnus-notify+-timeout 113
  "The timeout (second) that call `gnus-notify+'."
  :type 'number
  :group 'gnus-notify+)

(defvar gnus-notify+-string nil
  "The highlight string of for `gnus-notify+'.")

(unless (member 'gnus-notify+-string global-mode-string)
  (setq global-mode-string (append global-mode-string
                                   (list 'gnus-notify+-string))))

(defun gnus-notify+ ()
  "Update gnus news and display notify message in mode-line."
  (interactive)
  (let (gnus-news-number)
    (force-mode-line-update t)
    (flet ((message (&rest args)))      ;use `flet' to filter `gnus-group-get-new-news'
      (gnus-group-get-new-news))        ;output message on minibuffer to disturb user
    (setq gnus-news-number (gnus-get-unread-news-number))
    (if (> gnus-news-number 0)
        (progn
          ;; the property `risky-local-variable' is a security measure for mode line
          ;; variable that have properties
          (put 'gnus-notify+-string 'risky-local-variable t)
          (setq gnus-notify+-string (propertize (format " [%s: %d]" gnus-notify+-modeline-string gnus-news-number)
                                                'face 'gnus-notify+-modeline-face)))
      (setq gnus-notify+-string ""))))

(defun gnus-get-unread-news-number ()
  "Get the total number of unread news of gnus group."
  (let (total-unread-news-number)
    (setq total-unread-news-number 0)
    (mapc '(lambda (g)
             (let* ((group (car g))
                    (unread (gnus-group-unread group)))
               (when (and (numberp unread)
                          (> unread 0))
                 (setq total-unread-news-number (+ total-unread-news-number unread)))))
          gnus-newsrc-alist)
    total-unread-news-number))

;; Add timer to update `gnus-notify+'.
(run-with-timer 0 gnus-notify+-timeout 'gnus-notify+)

(provide 'gnus-notify+)

;;; gnus-notify+.el ends here

;;; LocalWords:  pre modeline args newsrc

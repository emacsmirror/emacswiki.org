;;; newsticker-extension.el --- Some extension function for newsticker

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-08-26 11:59:52
;; Version: 0.1.0
;; Last-Updated: 2008-08-26 11:59:57
;; URL: None
;; Keywords: newsticker, rss, atom
;; Compatibility: GNU Emacs 23.0.60

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
;;  None
;;

;;; Installation:
;;
;; Copy newsticker-extension.el to your load-path and add to your ~/.emacs
;;
;;  (require 'newsticker-extension)
;;
;; No need more

;;; Commentary:
;;
;; This package is a little extension for newsticker.
;; And have some function for handy.
;;

;;; Change log:
;;
;; 2008/08/26
;;         First released.
;;

;;; Thanks to:
;;
;; Not yet
;;

;;; TODO
;;
;; None
;;

;;; Code:

(defun newsticker-treeview-browse-url-with-w3m ()
  "Call `w3m-view-this-url-1' for the link of the item at point."
  (interactive)
  (save-excursion
    (set-buffer (newsticker--treeview-item-buffer))
    (let ((url (get-text-property (point) :nt-link)))
      (when url
        (w3m-view-this-url-1 url t t)
        (if newsticker-automatically-mark-visited-items-as-old
            (newsticker-treeview-mark-item-old))))))

(defun newsticker--treeview-browse-url-with-firefox ()
  "View url with Firefox."
  (interactive)
  (newsticker-treeview-browse-url-with-external-browser "firefox"))

(defun newsticker--treeview-browse-url-with-chromium-browser ()
  "View url with Chromium-Browser."
  (interactive)
  (newsticker-treeview-browse-url-with-external-browser "google-chrome"))

(defun newsticker-treeview-browse-url-with-external-browser (browser)
  "Call `browse-url' for the link of the item at point."
  (save-excursion
    (set-buffer (newsticker--treeview-item-buffer))
    (let ((url (get-text-property (point) :nt-link)))
      (when url
        (start-process "Newsticker-External-Browser" "*Newsticker-External-Browser*" browser url)
        (if newsticker-automatically-mark-visited-items-as-old
            (newsticker-treeview-mark-item-old))))))

(defun newsticker-treeview-prev-page ()
  "Scroll item buffer."
  (interactive)
  (save-selected-window
    (select-window (newsticker--treeview-item-window) t)
    (condition-case nil
        (scroll-down nil)
      (error
       (goto-char (point-max))))))

(defun newsticker-treeview-scroll-item+ ()
  "Scroll current item."
  (interactive)
  (save-selected-window
    (select-window (newsticker--treeview-item-window) t)
    (scroll-down 1)))

(defun newsticker-switch-to-w3m ()
  "Switch to w3m."
  (interactive)
  (newsticker-treeview-quit)
  (w3m))


(defun newsticker-start+ ()
  "Startup newsticker and update info in mode line."
  (interactive)
  (newsticker-start t))

(defun newsticker-treeview-first-feed ()
  "Jump to the depth-first feed in the newsticker-groups tree."
  (interactive)
  (newsticker-treeview-jump
   (car (reverse (newsticker--group-get-feeds newsticker-groups t)))))

(defun newsticker-treeview-last-feed ()
  "Jump to the depth-last feed in the newsticker-groups tree."
  (interactive)
  (newsticker-treeview-jump
   (car (newsticker--group-get-feeds newsticker-groups t))))

(provide 'newsticker-extension)

;;; newsticker-extension.el ends here

;;; LocalWords:  newsticker treeview nt

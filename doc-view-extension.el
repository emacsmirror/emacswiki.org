;;; doc-view-extension.el --- Simple extensions for `doc-view'

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-09-12 11:04:00
;; Version: 0.1.0
;; Last-Updated: 2008-09-12 11:04:04
;; URL:
;; Keywords: doc-view
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
;;  `doc-view'
;;

;;; Installation:
;;
;; Copy doc-view-extension.el to your load-path and add to your ~/.emacs
;;
;;  (require 'doc-view-extension)
;;
;; No need more

;;; Commentary:
;;
;; This package have some little extensions functions for `doc-view'
;;

;;; Change log:
;;
;; 2008/09/12
;;         First release.
;;

;;; Thanks to:
;;
;; Not yet
;;

;;; TODO
;;
;; None
;;

;;; Require
(require 'doc-view)

;;; Code:

(defun doc-view-scroll-up-or-next-page+ (&optional reverse)
  "Get next page in `doc-view-mode' buffer in other window.
Optional argument REVERSE default is scroll up (or next page), if REVERSE is non-nil scroll down (or previous page)."
  (interactive)
  (catch 'found
    (walk-windows
     (lambda (w)
       (with-selected-window w
         (when (eq major-mode 'doc-view-mode)
           (if reverse
               (doc-view-scroll-down-or-previous-page)
             (doc-view-scroll-up-or-next-page))
           (throw 'found "Have found")))))))

(defun doc-view-scroll-down-or-previous-page+ ()
  "Get previous page in `doc-view-mode' buffer in other window."
  (interactive)
  (doc-view-scroll-up-or-next-page+ t))

(defadvice scroll-other-window (around doc-view-scroll-up-or-next-page activate)
  "When next buffer is `doc-view-mode', do `doc-view-scroll-up-or-next-page'."
  (other-window +1)
  (if (eq major-mode 'doc-view-mode)
      (let ((arg (ad-get-arg 0)))
        (if (null arg)
            (doc-view-scroll-up-or-next-page)
          (doc-view-next-line-or-next-page arg))
        (other-window -1))
    (other-window -1)
    ad-do-it))

(defadvice scroll-other-window-down (around doc-view-scroll-down-or-previous-page activate)
  "When next buffer is `doc-view-mode', do `doc-view-scroll-down-or-previous-page'."
  (other-window +1)
  (if (eq major-mode 'doc-view-mode)
      (let ((arg (ad-get-arg 0)))
        (if (null arg)
            (doc-view-scroll-down-or-previous-page)
          (doc-view-previous-line-or-previous-page arg))
        (other-window -1))
    (other-window -1)
    ad-do-it))

(defun doc-view-next-line-or-next-page (arg)
  "Next line if possible, else goto next page."
  (interactive "P")
  (when (= (window-vscroll) (image-next-line (or arg 1)))
    (let ((cur-page (doc-view-current-page)))
      (doc-view-next-page)
      (when (/= cur-page (doc-view-current-page))
        (image-bob)
        (image-bol 1)))))

(defun doc-view-previous-line-or-previous-page (arg)
  "Previous line if possible, else goto previous page."
  (interactive "P")
  (when (= (window-vscroll) (image-previous-line (or arg 1)))
    (let ((cur-page (doc-view-current-page)))
      (doc-view-previous-page)
      (when (/= cur-page (doc-view-current-page))
        (image-eob)
        (image-bol 1)))))

(defun doc-view-page-reach-top-p ()
  "Return t if current page have reach top edge, otherwise return nil."
  (equal (window-vscroll) 0))

(defun doc-view-page-reach-bottom-p ()
  "Return t if current page have reach bottom edge, otherwise return nil."
  (let* ((image (image-get-display-property))
         (edges (window-inside-edges))
         (win-height (- (nth 3 edges) (nth 1 edges)))
         (img-height (ceiling (cdr (image-size image)))))
    (equal img-height (+ win-height (window-vscroll)))))

(provide 'doc-view-extension)

;;; doc-view-extension.el ends here

;;; LocalWords:  vscroll bol eob img

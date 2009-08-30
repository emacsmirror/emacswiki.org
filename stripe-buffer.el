;;; stripe-buffer.el --- Make stripe color with buffer.

;; Filename: stripe-buffer.el
;; Description: Make stripe color with buffer.
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-24 10:38:05
;; Version: 0.1
;; Last-Updated: 2008-10-24 10:38:09
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/stripe-buffer.el
;; Keywords: stripe
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
;; Make stripe color with buffer.
;; So the stripe color interface with gather the readable.
;;

;;; Installation:
;;
;; Put stripe-buffer.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'stripe-buffer)
;;
;; No need more.

;;; Change log:
;;
;; 2008/10/24
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

(defface stripe-highlight
  '((((class color) (background dark))
     (:background "Grey7" :foreground "Gold4")))
  "Face for highlighting current overlay."
  :group 'basic-faces)

(defvar stripe-highlight-face 'stripe-highlight
  "The face variable for `stripe-buffer-on'")

(defvar stripe-highlight-overlays nil
  "The overlays for `stripe-buffer'.")

(make-variable-buffer-local 'stripe-highlight-face)
(make-variable-buffer-local 'stripe-highlight-overlays)

(defun stripe-buffer-on (stripe-regexp &optional stripe-line-interval buffer)
  "Make turn on stripe line with BUFFER.
`stripe-regexp' is match string per line.
`stripe-line-interval' is interval lines that need match."
  (interactive)
  (or stripe-line-interval (setq stripe-line-interval 1))
  (or buffer (setq buffer (current-buffer)))
  (stripe-buffer-off buffer)
  (with-current-buffer buffer
    (save-excursion
      (let (overlay end-point)
        (goto-char (point-min))
        (while (not (eobp))
          (beginning-of-line)
          (when (search-forward-regexp stripe-regexp (line-end-position) t)
            (setq overlay (make-overlay (match-beginning 0) (match-end 0)))
            (overlay-put overlay 'face stripe-highlight-face)
            (push overlay stripe-highlight-overlays))
          (forward-line stripe-line-interval))))))

(defun stripe-buffer-off (&optional buffer)
  "Make turn off stripe line for BUFFER."
  (interactive)
  (or buffer (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (when stripe-highlight-overlays
      (mapc 'delete-overlay stripe-highlight-overlays)
      (setq stripe-highlight-overlays nil))))

(provide 'stripe-buffer)

;;; stripe-buffer.el ends here

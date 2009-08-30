;;; speedbar-extension.el --- Some extensions for speedbar

;; Author: Emacs Guys <emacser@freedom.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2008, 2009, Emacs Guys <emacser@freedom.com>, all rights reserved.
;; Created: 2008-06-23 14:46:02
;; Version: 1.0
;; Last-Updated: 2008-06-23 14:46:05
;; URL: not distributed yet
;; Keywords: speedbar
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
;;  `sr-speedbar'
;;

;;; Installation:
;;
;; Copy speedbar-extension.el to your load-path and add to your ~/.emacs
;;
;;  (require 'speedbar-extension)
;;
;; No need more

;;; Commentary:
;;
;; Some extensions for speedbar.
;;

;;; Change log:
;;
;; 2008/06/23
;;         First release.
;;

;;; Acknowledgments:
;;
;;      Emacs guys.
;;

;;; TODO
;;
;; None
;;

;;; Require
(require 'sr-speedbar)

;;; Code:
(defun speedbar-forced-contract ()
  "collapses the sub list the cursor is in"
  (interactive)
  (let ((depth (save-excursion (beginning-of-line)
                               (if (looking-at "[0-9]+:")
                                   (string-to-number (match-string 0))
                                 0)))
        (lastmatch (point))
        (condition 1))
    (while (/= condition 0)
      (forward-line -1)
      (let ((subdepth (save-excursion (beginning-of-line)
                                      (if (looking-at "[0-9]+:")
                                          (string-to-number (match-string 0))
                                        0))))
        (cond ((or (< subdepth depth)
                   (progn (end-of-line) (eobp))
                   (progn (beginning-of-line) (bobp)))
               ;; We have reached the end of this block.
               (goto-char lastmatch)
               (setq condition 0))
              ((= subdepth depth)
               (setq lastmatch (point))))))
    (speedbar-position-cursor-on-line))
  (forward-line -1)
  (speedbar-contract-line))

(defun speedbar-buffers ()
  "show buffer list in the speedbar"
  (interactive)
  (speedbar-change-initial-expansion-list "quick buffers"))

(defun speedbar-files ()
  "show file list in the speedbar"
  (interactive)
  (speedbar-change-initial-expansion-list "files"))

(provide 'speedbar-extension)

;;; speedbar-extension.el ends here

;;; LocalWords:  sr lastmatch subdepth

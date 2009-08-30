;;; gnus-extension.el --- Simple description

;; Filename: gnus-extension.el
;; Description: Simple description
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-12-29 08:50:01
;; Version: 0.1
;; Last-Updated: 2008-12-29 08:50:01
;;           By: Andy Stewart
;; URL:
;; Keywords: gnus
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
;; Simple description
;;

;;; Installation:
;;
;; Put gnus-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'gnus-extension)
;;
;; No need more.

;;; Change log:
;;
;; 2008/12/29
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

(defvar gnus-summary-sort-method nil
  "The sort method by `gnus-summary-sort'.")

(defvar gnus-summary-sort-order t
  "The sort order by `gnus-summary-sort-by-reverse'.")

(defadvice gnus-summary-sort (around get-sort-method activate)
  "Get sort method by `gnus-summary-sort'."
  (setq gnus-summary-sort-method (or (ad-get-arg 0) ""))
  ad-do-it)

(defun gnus-summary-sort-by-reverse ()
  "Sort the summary buffer by reverse order.
And keep current sort method."
  (interactive)
  (when (and gnus-summary-sort-method
             (not (equal gnus-summary-sort-method "")))
    (gnus-summary-sort (format "%s" gnus-summary-sort-method) gnus-summary-sort-order)
    (setq gnus-summary-sort-order (not gnus-summary-sort-order))))

(add-hook 'gnus-summary-exit-hook '(lambda () (setq gnus-summary-sort-order t)))

(defun gnus-group-read-group-no-prompt ()
  "Read news in this newsgroup and don't prompt.
Use the value of `gnus-large-newsgroup'."
  (interactive)
  (gnus-group-read-group gnus-large-newsgroup))

(provide 'gnus-extension)

;;; gnus-extension.el ends here

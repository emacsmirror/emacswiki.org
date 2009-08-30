;;; gnus-summary-stripe.el --- Strip gnus summary buffer

;; Filename: gnus-summary-stripe.el
;; Description: Strip gnus summary buffer
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-12-08 21:04:25
;; Version: 0.1
;; Last-Updated: 2008-12-08 21:04:29
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/gnus-summary-stripe.el
;; Keywords: gnus, summary, stripe
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `stripe-buffer'
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
;; Strip gnus summary buffer
;;

;;; Installation:
;;
;; Put gnus-summary-stripe.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'gnus-summary-stripe)
;;
;; No need more.

;;; Change log:
;;
;; 2008/12/08
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
(require 'stripe-buffer)

;;; Code:

(defcustom gnus-summary-stripe-regexp nil
  "The regexp that match string in stripe line."
  :type 'string
  :group 'gnus-summary)

(defun gnus-summary-stripe ()
  "Strip line in `gnus-summary-buffer' with `gnus-summary-stripe-regexp'."
  (interactive)
  (stripe-buffer-on gnus-summary-stripe-regexp 2))

(add-hook 'gnus-summary-prepare-hook 'gnus-summary-stripe)
(add-hook 'gnus-summary-update-hook 'gnus-summary-stripe)

(provide 'gnus-summary-stripe)

;;; gnus-summary-stripe.el ends here

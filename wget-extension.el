;;; wget-extension.el --- Some enhanced functions for wget.el

;; Filename: wget-extension.el
;; Description: Some enhanced functions for wget.el
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-03-13 01:50:40
;; Version: 0.1
;; Last-Updated: 2009-03-13 01:50:40
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/wget-extension.el
;; Keywords: wget
;; Compatibility: GNU Emacs 23.0.91.1
;;
;; Features that might be required by this library:
;;
;; `wget'
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
;; Some enhanced functions for wget.el
;;

;;; Installation:
;;
;; Put wget-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'wget-extension)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET wget-extension RET
;;

;;; Change log:
;;
;; 2009/03/13
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
(require 'wget)

;;; Code:
(defvar wget-hide-status t
  "Default hide wget download window.")

(defun wget-hide ()
  "Hide wget download information."
  (interactive)
  (if (bufferp (get-buffer wget-process-buffer))
      (delete-window (get-buffer-window (get-buffer wget-process-buffer))))
  (setq wget-hide-status t))

(defun wget-show ()
  (interactive)
  (call-interactively 'wget-state-of-progress)
  (setq wget-hide-status nil))

(provide 'wget-extension)

;;; wget-extension.el ends here


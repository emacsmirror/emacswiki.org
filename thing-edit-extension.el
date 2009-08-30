;;; thing-edit-extension.el --- Some enhanced functions for thing-edit.el

;; Filename: thing-edit-extension.el
;; Description: Some enhanced functions for thing-edit.el
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-01-09 13:02:23
;; Version: 0.1
;; Last-Updated: 2009-01-09 13:02:23
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/thing-edit-extension.el
;; Keywords: thingatpt, thingedit
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `thing-edit' `paredit'
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
;; Some enhanced functions for thing-edit.el
;;
;; Below are commands you can use:
;;
;; thing-paste-parentheses      paste string in parentheses
;; thing-copy-parentheses       copy string in parentheses
;;

;;; Installation:
;;
;; Put thing-edit-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'thing-edit-extension)
;;
;; No need more.

;;; Change log:
;;
;; 2009/01/09
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
(require 'thing-edit)
(require 'paredit)

;;; Code:

(defun thing-paste-parentheses ()
  "Paste content in match parentheses."
  (interactive)
  (thing-copy-parentheses t))

(defun thing-copy-parentheses (&optional kill-conditional)
  "Copy content in match parentheses.
If `KILL-CONDITIONAL' is non-nil, kill object,
otherwise copy object."
  (interactive)
  (if (paredit-in-string-p)
      (thing-edit-internal
       (1+ (car (paredit-string-start+end-points)))
       (cdr (paredit-string-start+end-points))
       kill-conditional)
    (thing-edit-internal
     (progn
       (backward-up-list)
       (forward-char +1)
       (point))
     (progn
       (up-list)
       (forward-char -1)
       (point))
     kill-conditional)))

(provide 'thing-edit-extension)

;;; thing-edit-extension.el ends here

;;; lazycat-c-style.el --- C programming configuration

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-07-14 14:28:13
;; Version: 0.1
;; Last-Updated: 2008-07-14 14:28:27
;; URL: not distributed yet
;; Keywords: c, style
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
;;  None
;;

;;; Installation:
;;
;; Copy lazycat-c-style.el to your load-path and add to your ~/.emacs
;;
;;  (require 'lazycat-c-style)
;;
;; No need more

;;; Commentary:
;;
;; Some C programming style.
;;

;;; Change log:
;;
;; 2008/07/14
;;         First release.
;;

;;; Acknowledgments:
;;
;; Not yet
;;

;;; TODO
;;
;; None
;;

;;; Code:

(add-hook 'c-mode-common-hook 'lazycat-c-mode)
(defun lazycat-c-mode ()
  "My C Style."
  (c-set-style "k&r")
  (setq c-basic-offset 4)
  ;; (c-toggle-auto-newline 1)
  (c-toggle-hungry-state 1)
  (overwrite-alist (quote ((defun-open after)
                           (defun-close after)
                           (class-open before after)
                           (class-close ())
                           (block-open after)
                           (block-close . c-snug-do-while)
                           (topmost-intro)
                           (brace-list-open)
                           (brace-list-close)
                           (do-while-closure  after)
                           (substatement-open  after)
                           (else-clause after)
                           (access-label after)
                           (catch-clause  after)))
                   'c-hanging-braces-alist)
  (overwrite-alist (quote ((case-label after)
                           (access-label after)
                           (label after)
                           (inher-intro)))
                   'c-hanging-colons-alist)
  (setq c-hanging-semi&comma-criteria
        (quote (c-semi&comma-inside-parenlist)))
  (setq c-cleanup-list
        (quote (brace-catch-brace
                brace-else-brace
                brace-elseif-brace
                empty-defun-braces
                defun-close-semi
                one-liner-defun
                list-close-comma
                scope-operator))))
(defun xgp-cfsi-modify-alist (alist term new)
  "Add alist."
  (let ((tl (assq term (symbol-value alist))))
    (if tl
        (setcdr tl new)
      (add-to-list alist (cons term new)))))
(defun overwrite-alist (from to)
  "Overwrite alist."
  (cond ((null from) nil)
        (t (progn (xgp-cfsi-modify-alist to
                                         (car (car from))
                                         (cdr (car from)))
                  (overwrite-alist (cdr from) to)))))

(provide 'lazycat-c-style)

;;; lazycat-c-style.el ends here

;;; LocalWords:  lazycat substatement inher parenlist elseif xgp cfsi tl

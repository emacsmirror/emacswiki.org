;;; helm-webkit.el --- Search browse history of emacs-webkit

;; Filename: helm-webkit.el
;; Description: Search browse history of emacs-webkit
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-06-13 03:30:03
;; Version: 0.1
;; Last-Updated: 2014-06-13 03:30:03
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/helm-webkit.el
;; Keywords: webkit
;; Compatibility: GNU Emacs 24.4.50.1
;;
;; Features that might be required by this library:
;;
;; `webkit'
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
;; `helm-webkit' is extension to search browse history of emacs-webkit.
;;

;;; Installation:
;;
;; You need install emacs-webkit first: http://www.emacswiki.org/emacs/WebKit
;;
;; Put helm-webkit.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'helm-webkit)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET helm-webkit RET
;;

;;; Change log:
;;
;; 2014/06/13
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
(require 'webkit)

;;; Code:

(defun hash-to-list (hashtable)
  "Return a list that represent the hashtable."
  (let (hashlist)
    (maphash (lambda (kk vv) (setq hashlist (cons (list kk vv) hashlist))) hashtable)
    hashlist))

(defvar helm-source-webkit-buffer "*helm webkit browse history*")

(defvar helm-source-webkit
  `((name . "Web Browser")
    (candidate-number-limit . 9999)
    (candidates
     . (lambda nil
         (loop for url-info in (mapcar (lambda (a) (list (car a)  (cadadr a))) (sort (hash-to-list webkit-history-urls) (lambda (a b) (> (caadr a) (caadr b)))))
               for name = (car url-info)
               for title = (cdr url-info)
               collect
               (cons
                (concat
                 (propertize (format "%s" name)
                             'face 'font-lock-function-name-face)
                 (propertize (format " %s" title)
                             'face 'font-lock-doc-face))
                (list name title)))
         ))
    (action . (("Open URL" .
                (lambda (candidate)
                  (webkit-open-url (car candidate))
                  ))
               ("Delete history" .
                (lambda (candidate)
                  (webkit-delete-history-url (car candidate))))
               ("Clean all history" .
                (lambda (candidate)
                  (webkit-clean-history)))
               ))
    (persistent-action . describe-command)))

;;;###webkit
(defun helm-webkit-commands nil
  "Select from webkit commands to execute."
  (interactive)
  (helm :sources 'helm-source-webkit
        :buffer helm-source-webkit-buffer))

(provide 'helm-webkit)

;;; helm-webkit.el ends here

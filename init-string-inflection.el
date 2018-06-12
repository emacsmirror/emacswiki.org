;;; init-string-inflection.el --- Config for string-inflection.el

;; Filename: init-string-inflection.el
;; Description: Config for string-inflection.el
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-12 20:44:16
;; Version: 0.1
;; Last-Updated: 2018-06-12 20:44:16
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-string-inflection.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `string-inflection' `one-key'
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
;; Config for string-inflection.el
;;

;;; Installation:
;;
;; Put init-string-inflection.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-string-inflection)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-string-inflection RET
;;

;;; Change log:
;;
;; 2018/06/12
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
(require 'string-inflection)

;;; Code:

(defvar one-key-string-inflection-alist nil
  "The `one-key' menu alist for DIRECTORY.")

(setq one-key-string-inflection-alist
      '(
        (("c" . "FooBar") . (lambda () (interactive) (string-inflection-camelcase)))
        (("l" . "fooBar") . (lambda () (interactive) (string-inflection-lower-camelcase)))
        (("_" . "foo_bar") . (lambda () (interactive) (string-inflection-underscore)))
        (("u" . "FOO_BAR") . (lambda () (interactive) (string-inflection-upcase)))
        (("k" . "foo-bar") . (lambda () (interactive) (string-inflection-kebab-case)))
        (("t" . "foo_bar <=> FooBar") . (lambda () (interactive) (string-inflection-toggle)))
        ))

(defun one-key-string-inflection ()
  "The `one-key' menu for DIRECTORY."
  (interactive)
  (one-key-menu "STRING INFLECTION" one-key-string-inflection-alist t))

(provide 'init-string-inflection)

;;; init-string-inflection.el ends here

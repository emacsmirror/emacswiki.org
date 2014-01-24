;;; auto-complete-clang-extension.el --- Extension for Clang auto completion.

;; Filename: auto-complete-clang-extension.el
;; Description: Extension for Clang auto completion.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-01-24 13:38:11
;; Version: 0.1
;; Last-Updated: 2014-01-24 13:38:11
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/auto-complete-clang-extension.el
;; Keywords:
;; Compatibility: GNU Emacs 24.3.50.1
;;
;; Features that might be required by this library:
;;
;; `auto-complete' `auto-complete-c-headers' `auto-complete-clang'
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
;; Extension for Clang auto completion.
;;

;;; Installation:
;;
;; Put auto-complete-clang-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'auto-complete-clang-extension)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET auto-complete-clang-extension RET
;;

;;; Change log:
;;
;; 2014/01/24
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

(require 'auto-complete-config)

;;; Code:

(defun get-include-dirs ()
  (let* ((command-result (shell-command-to-string "echo \"\" | g++ -v -x c++ -E -"))
         (start-string "#include <...> search starts here:\n")
         (end-string "End of search list.\n")
         (start-pos (string-match start-string command-result))
         (end-pos (string-match end-string command-result))
         (include-string (substring command-result (+ start-pos (length start-string)) end-pos)))
    (split-string include-string)))

(add-hook
 'c-mode-common-hook
 '(lambda ()
    (require 'auto-complete-c-headers)
    (require 'auto-complete-clang)
    (setq ac-sources (append '(ac-source-c-headers ac-source-clang) ac-sources))
    (setq ac-clang-flags
          (mapcar (lambda (item) (concat "-I" item)) (get-include-dirs)))
    ))

(provide 'auto-complete-clang-extension)

;;; auto-complete-clang-extension.el ends here

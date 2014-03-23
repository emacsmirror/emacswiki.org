;;; inf-ruby-extension.el --- inf-ruby extension

;; Filename: inf-ruby-extension.el
;; Description: inf-ruby extension
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-03-23 14:37:30
;; Version: 0.1
;; Last-Updated: 2014-03-23 14:37:30
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/inf-ruby-extension.el
;; Keywords:
;; Compatibility: GNU Emacs 24.3.50.1
;;
;; Features that might be required by this library:
;;
;; `inf-ruby'
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
;; `inf-ruby' provide a way run ruby subprocess, but it like term.el that
;; not close inf-ruby buffer after user type 'quit' command in it.
;;
;; `inf-ruby-extension' add `inf-ruby-handle-close' function in `inf-ruby-mode-hook' to
;; make inf-ruby close automatically after we don't need it.
;;

;;; Installation:
;;
;; Put inf-ruby-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'inf-ruby-extension)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET inf-ruby-extension RET
;;

;;; Change log:
;;
;; 2014/03/23
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

(require 'inf-ruby)

;;; Code:

(add-hook 'inf-ruby-mode-hook (lambda () (inf-ruby-handle-close)))

(defun inf-ruby-handle-close ()
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)" change)
                              (message "hello")
                              (kill-buffer (process-buffer proc)))))))

(provide 'inf-ruby-extension)

;;; inf-ruby-extension.el ends here

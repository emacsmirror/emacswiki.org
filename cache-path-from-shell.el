;;; cache-path-from-shell.el --- Provide a chache mechanism make sure exec-path-from-shell just execute once.

;; Filename: cache-path-from-shell.el
;; Description: Provide a chache mechanism make sure exec-path-from-shell just execute once.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-09-19 14:05:39
;; Version: 0.2
;; Last-Updated: 2018-09-19 15:07:46
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/cache-path-from-shell.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `exec-path-from-shell'
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
;; Provide a chache mechanism make sure exec-path-from-shell just execute once.
;;
;; On MacOS, we always need load environment variables in Emacs
;; to make `shell-command' etc tools can works as expected.
;; `exec-path-from-shell' is wonderful extension to do that.
;;
;; But command `exec-path-from-shell-initialize' is very slow
;; and perhaps many libraries need call `exec-path-from-shell-initialize' itself.
;;
;; So, `cache-path-from-shell' use cache mechanism make sure
;; `exec-path-from-shell-initialize' just execute once and
;; no matter how many times you call it.
;;

;;; Installation:
;;
;; Put cache-path-from-shell.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'cache-path-from-shell)
;;
;; Then feel free to call `exec-path-from-shell-initialize' at anyplace . ;)
;;

;;; Change log:
;;
;; 2018/09/19
;;      * First released.
;;      * Disable startup warning of `exec-path-from-shell' default.
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
(require 'exec-path-from-shell)

;;; Code:

;; We don't need warnings from `exec-path-from-shell'. ;)
(setq exec-path-from-shell-check-startup-files nil)

(defvar cache-path-from-shell-loaded-p nil)

(defadvice exec-path-from-shell-initialize (around cache-path-from-shell-advice activate)
  (if cache-path-from-shell-loaded-p
      (message "All shell environment variables has loaded in Emacs, yow!")
    (setq cache-path-from-shell-loaded-p t)
    ad-do-it
    ))

(provide 'cache-path-from-shell)

;;; cache-path-from-shell.el ends here

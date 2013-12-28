;;; save-load-path.el --- save load-path and reuse it to test

;; Time-stamp: <2013-12-29 04:49:07 rubikitch>

;; Copyright (C) 2010  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: lisp
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/save-load-path.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Prepare `load-path' for testing in emacs -Q.
;; First, save your `load-path' in your .emacs.
;; Then, invoke emacs with command:
;;   $ emacs -Q -l ~/.emacs.d/saved-load-path.el
;; It is ready for all emacs lisp programs you have!

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Installation:
;;
;; Put save-load-path.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'save-load-path)
;; (save-load-path-initialize)
;;
;; No need more.

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET save-load-path RET
;;


;;; History:

;; See http://www.rubyist.net/~rubikitch/gitlog/save-load-path.txt

;;; Code:

(defgroup save-load-path nil
  "save-load-path"
  :group 'emacs)

(defvar save-load-path-file "~/.emacs.d/saved-load-path.el")
(defun save-load-path ()
  "Save `load-path' to file `save-load-path-file'."
  (with-temp-buffer
    (insert
     (let (print-level print-length)
       (prin1-to-string `(setq load-path ',load-path))))
    (write-region (point-min) (point-max) save-load-path-file)))

(defun save-load-path-initialize ()
  "Initialize `save-load-path'."
  (add-hook 'after-init-hook 'save-load-path))

;; (bg0 "emacs -Q -l ~/.emacs.d/saved-load-path.el")
(provide 'save-load-path)

;; How to save (DO NOT REMOVE!!)
;; (progn (git-log-upload) (emacswiki-post "save-load-path.el"))
;;; save-load-path.el ends here

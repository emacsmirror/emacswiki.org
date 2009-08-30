;;; anything-extension.el --- Extension functions for anything.el

;; Filename: anything-extension.el
;; Description: Extension functions for anything.el
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-01-27 22:33:54
;; Version: 0.2
;; Last-Updated: 2009-02-09 10:44:04
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/anything-extension.el
;; Keywords: anything
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `anything' `anything-config'
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
;; Extension functions for anything.el
;;

;;; Installation:
;;
;; Put anything-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'anything-extension)
;;
;; No need more.

;;; Change log:
;;
;; 2009/02/09
;;      * Remove `semantic' depend, just load `anything-c-source-semantic'
;;        when you load `semantic' successful.
;;
;; 2009/01/27
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
(require 'anything)
(require 'anything-config)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Sources ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elisp Library catalog
;;
;; `anything-c-source-elisp-library-catalog' is faster than `anything-c-source-elisp-library'
;; But this need you setup it.
;;
;; If you want use those code,
;; you have to use `crontab' to update `~/MyEmacs/Document/library-list.txt', like me:
;;
;; crontab -e
;;
;; */5 * * * * find ~/MyEmacs \( -name '*.el' -o -name '*.el.gz' \) > ~/MyEmacs/Document/library-list.txt
;;
(defvar anything-c-elisp-library-catalog-timeout 150
  "The timeout that update library list.")

(defvar anything-c-elisp-library-catalog-alist nil
  "The alist for storage library filepath.")

(defvar anything-c-elisp-library-catalog-filename "~/MyEmacs/Document/library-list.txt"
  "The filename that contain library list.")

(defvar anything-c-source-elisp-library-catalog
  '((name . "Elisp libraries (Catalog)")
    (init . (anything-c-elisp-library-catalog-init))
    (candidates-in-buffer)
    (type . file)
    (requires-pattern . 1)
    (major-mode emacs-lisp-mode)))

(defun anything-c-elisp-library-catalog-init ()
  "Init anything buffer status."
  (let ((anything-buffer (anything-candidate-buffer 'global))
        (library-list (or anything-c-elisp-library-catalog-alist (anything-c-elisp-library-catalog-update))))
    (with-current-buffer anything-buffer
      (dolist (library library-list)
        (insert (format "%s\n" library))))))

(defun anything-c-elisp-library-catalog-update ()
  "Update library list"
  (setq anything-c-elisp-library-catalog-alist (anything-c-elisp-library-catalog-fileline-to-alist anything-c-elisp-library-catalog-filename)))

(run-with-timer 0 anything-c-elisp-library-catalog-timeout 'anything-c-elisp-library-catalog-update)

(defun anything-c-elisp-library-catalog-fileline-to-alist (file)
  "Transform line in special file to element of list.
And return list."
  (let (return-list)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) return-list)
        (forward-line +1))
      (nreverse return-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Redefine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq anything-c-source-tracker-search
      '((name . "Tracker Search")
        (candidates . (lambda ()
                        (start-process "tracker-search-process" nil
                                       "tracker-search"
                                       anything-pattern)))
        (type . file)
        (requires-pattern . 1)          ;setup `1' to make just need type 1 character can get result
        (volatile)))

(provide 'anything-extension)

;;; anything-extension.el ends here

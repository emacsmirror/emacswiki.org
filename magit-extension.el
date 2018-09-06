;;; magit-extension.el --- Extension for magit

;; Filename: magit-extension.el
;; Description: Extension for magit
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-09-07 03:32:14
;; Version: 0.1
;; Last-Updated: 2018-09-07 03:32:14
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/magit-extension.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `magit'
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
;; Extension for magit
;;

;;; Installation:
;;
;; Put magit-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'magit-extension)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET magit-extension RET
;;

;;; Change log:
;;
;; 2018/09/07
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
(require 'magit)

;;; Code:

(defun magit-submodule-remove (&optional module-name)
  (interactive)
  (save-excursion
    ;; Get current directory for restore after remove submodule.
    (setq current-directory default-directory)
    ;; Cd magit toplevel directory make sure `magit-list-module-paths' can work.
    (cd (magit-toplevel))
    ;; Get subodule name/path will to remove.
    (setq submodule-name (or module-name (completing-read "Remove submodule: " (magit-list-module-paths))))
    (setq submodule-path (concat (magit-toplevel) submodule-name))
    ;; Delete submodule directory first.
    (when (file-exists-p submodule-path)
      (delete-directory submodule-path t))
    ;; Delete submodule from .gitmodules file.
    (setq gitmodules-file-path (concat (magit-toplevel) ".gitmodules"))
    (magit-kill-buffer-match-file gitmodules-file-path)
    (with-current-buffer (find-file gitmodules-file-path)
      (goto-char (point-min))
      (when (search-forward-regexp (format "path\\s-=\\s-%s" submodule-name) nil t)
        (previous-line)
        (beginning-of-line)
        (kill-line 3)
        (save-buffer)))
    ;; Get submodule url.
    (setq git-modules-submodule-path (concat (magit-toplevel) ".git/modules/" (file-name-base submodule-name)))
    (magit-kill-buffer-match-file (concat git-modules-submodule-path "/config"))
    (with-current-buffer (find-file (concat git-modules-submodule-path "/config"))
      (goto-char (point-min))
      (when (search-forward-regexp "^\\[remote\\s-\"origin\"\\]" nil t)
        (when (search-forward-regexp "url\\s-=\\s-" nil t)
          (setq submodule-url (buffer-substring-no-properties
                               (point)
                               (save-excursion
                                 (end-of-line)
                                 (point))))
          )))
    ;; Delete submodule from .git/config file.
    (setq git-config-file-path (concat (magit-toplevel) ".git/" "config"))
    (magit-kill-buffer-match-file git-config-file-path)
    (with-current-buffer (find-file git-config-file-path)
      (goto-char (point-min))
      (when (search-forward-regexp (format "url\\s-=\\s-%s" submodule-url) nil t)
        (previous-line)
        (beginning-of-line)
        (kill-line 3)
        (save-buffer)))
    ;; Delete submodule under .git/modules/ directory.
    (when (file-exists-p git-modules-submodule-path)
      (delete-directory git-modules-submodule-path t))
    ;; Kill temp buffer.
    (magit-kill-buffer-match-file gitmodules-file-path)
    (magit-kill-buffer-match-file (concat git-modules-submodule-path "/config"))
    (magit-kill-buffer-match-file git-config-file-path)
    ;; Restore current directory.
    (cd current-directory)
    ))

(defun magit-kill-buffer-match-file (filepath)
  (catch 'find-match
    (dolist (buffer (buffer-list))
      (when (string-equal (buffer-file-name buffer) filepath)
        (kill-buffer buffer)
        (throw 'find-match buffer)))
    nil))

(provide 'magit-extension)

;;; magit-extension.el ends here

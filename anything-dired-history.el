;;; anything-dired-history.el --- Show dired history with anything.el support.

;; Filename: anything-dired-history.el
;; Description:  Show dired history with anything.el support.
;; Author: Joseph <jixiuf@gmail.com>
;; Maintainer: Joseph <jixiuf@gmail.com>
;; Copyright (C) 2011~, Joseph, all rights reserved.
;; Created: 2011-03-26
;; Version: 0.1.0
;; URL: http://www.emacswiki.org/emacs/download/anything-dired-history.el
;; Keywords: anything, dired history
;; Compatibility: (Test on GNU Emacs 23.2.1)
;;  .
;;
;; Features that might be required by this library:
;;
;; `anything' `dired'
;;
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
;; Someone like to reuse the current dired buffer to visit
;; another directory, so that you just need open one dired
;; buffer. but the bad point is ,you can't  easily go
;; forward and back in different dired directory. this file
;; can remember dired directory you have visited and list them
;; using `anything.el'.
;; `anything-dired-history.el' will save all dired directory you
;; have visited to file `anything-dired-history-cache-file'
;;

;;; Installation:
;;
;; (require 'anything-dired-history)
;; (define-key dired-mode-map "," 'anything-dired-history-view)

;; (require 'savehist)
;; (setq savehist-additional-variables
;;       '( anything-dired-history-variable
;;          ))
;; (savehist-mode 1)
;;
;; Or:
;; (autoload 'anything-dired-history-view "anything-dired-history"
;;    "view dired directories you have visited." t)
;; (define-key dired-mode-map "," 'anything-dired-history-view)
;;
;;
;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-dired-history-view'
;;    call `anything' to show dired history.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;


;;; Require
(require 'anything)
(require 'dired)


;; (defcustom anything-dired-history-cache-file
;;   "~/.emacs.d/cache/anything-dired-history-cache-file"
;;   "anything-dired-history-cache-file."
;;   :group 'anything-dired-history)

;; (defcustom anything-dired-history-max-length 20
;;     "the max length of dired history."
;;   :group 'anything-dired-history)

(defvar anything-dired-history-variable nil)

;; ;; if `anything-dired-history-cache-file' exists ,init
;; ;; `anything-dired-history-variable' with data from this file.
;; (when (file-exists-p (expand-file-name anything-dired-history-cache-file))
;;   (with-current-buffer (find-file-noselect anything-dired-history-cache-file)
;;     (goto-char (point-min))
;;     (setq anything-dired-history-variable (read (current-buffer)))
;;     (kill-buffer)))
;;;###autoload
(defun anything-dired-history-update()
  "update variable `anything-dired-history-variable'."
  (setq anything-dired-history-variable
        (delete-dups (delete (dired-current-directory) anything-dired-history-variable)))
  (setq anything-dired-history-variable
        (append (list (dired-current-directory)) anything-dired-history-variable)))

;;when you open dired buffer ,update `anything-dired-history-variable'.
;;;###autoload
(add-hook 'dired-after-readin-hook 'anything-dired-history-update)

;; (defun anything-dired-history-write2dist()
;;   "write `anything-dired-history-variable' to disk."
;;   (let ((tmp-history)(index 0))
;;     (while  (< index (min (length anything-dired-history-variable)
;;                           anything-dired-history-max-length))
;;       (setq tmp-history (append tmp-history (list (nth index anything-dired-history-variable))))
;;       (setq index (1+ index)))
;;       (with-temp-file (expand-file-name anything-dired-history-cache-file)
;;         (prin1 tmp-history (current-buffer)))
;;       ))
;; (add-hook 'kill-emacs-hook 'anything-dired-history-write2dist)
;; (run-with-timer 600 1800 'recentf-save-list)

(defvar anything-c-source-dired-history
  '((name . "Dired History:")
    (candidates . anything-dired-history-variable)
    (action . (("Go" . (lambda(candidate) (dired candidate)))))))

;;;###autoload
(defun anything-dired-history-view()
  "call `anything' to show dired history."
  (interactive)
  (let ((anything-execute-action-at-once-if-one t)
        (anything-quit-if-no-candidate
         (lambda () (message "No history record."))))
    (anything '(anything-c-source-dired-history)
              ;; Initialize input with current symbol
              ""  nil nil)))

(provide 'anything-dired-history)
;;ends here.

;;; chm-view.el --- View CHM file.

;; Filename: chm-view.el
;; Description: View CHM file.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-01-28 21:35:26
;; Version: 0.2.2
;; Last-Updated: 2009-04-16 10:48:52
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/chm-view.el
;; Keywords: chm, chm-view
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
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
;; View CHM file.
;;
;; This package is view CHM file in Emacs.
;; This package use `archmage' decompress
;; CHM file and view in browser.
;;
;; Below are commands you can use:
;;
;;      `chm-view-file'         View CHM file.
;;      `chm-view-dired'        View dired marked CHM files.
;;

;;; Installation:
;;
;; This package is base on `archmage', so make sure
;; `archmage' have install in your system, like me
;; (I use Debian), install `archmage':
;;
;;      sudo aptitude install archmage -y
;;
;; And then put chm-view.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'chm-view)
;;
;; That's all, enjoy! :)
;;

;;; Note:
;;
;; Because “archmage” need time to extract CHM file,
;; if browser (such as emacs-w3m) open special port before “archmage” extract completed,
;; it will got error “Cannot retrieve URL: http://localhost:531560 (exit status: 0)”,
;; then you just refresh current page in browser, problem will be fix.
;; If you always get error when open CHM file,
;; you need setup a bigger value to option `chm-view-delay' (default is 0.3 second).
;;

;;; Customize:
;;
;; `chm-view-delay'
;;      The delay time before view CHM file.
;;
;; All of the above can customize by:
;;      M-x customize-group RET chm-view RET
;;

;;; Change log:
;;
;; 2009/04/16
;;      * Fix `absolute path' problem with file.
;;
;; 2009/04/10
;;      * Fix doc.
;;
;; 2009/01/29
;;      * Add option `chm-view-delay'.
;;
;; 2009/01/28
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


;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup chm-view nil
  "Interface for chm-view."
  :group 'edit)

(defcustom chm-view-delay 0.3
  "The delay time before view CHM file.
This is necessary spend time to start sub-process."
  :type 'number
  :group 'chm-view)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar chm-view-last-filename nil
  "The name of last visit CHM file.")

(defvar chm-view-pid nil
  "The PID of chm-view process.")
(make-variable-buffer-local 'chm-view-pid)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun chm-view-file (&optional file)
  "View CHM FILE."
  (interactive)
  ;; Get file name.
  (or file (setq file (read-file-name (format "CHM file: (%s) " (or chm-view-last-filename ""))
                                      nil chm-view-last-filename)))
  ;; Record last visit file name.
  (setq chm-view-last-filename file)
  ;; View.
  (chm-view-internal file))

(defun chm-view-dired ()
  "View dired marked files."
  (interactive)
  (dolist (file (dired-get-marked-files))
    (chm-view-internal file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun chm-view-internal (file)
  "Internal function for view CHM FILE."
  (let ((random-number (chm-view-get-unique-number))
        proc)
    (with-current-buffer (get-buffer-create
                          ;; Need leave one whitespace before buffer name.
                          (format " *chm-view<%s>*" random-number))
      (setq proc (start-process (buffer-name) (current-buffer)
                                "archmage" "-p" random-number (expand-file-name file)))
      ;; Just wait a moment.
      (sit-for chm-view-delay)
      ;; Browse.
      (browse-url (format "http://localhost:%s" random-number))
      ;; Record PID of `chm-view' process.
      (setq chm-view-pid (process-id proc))
      ;; Add interrupt process of buffer to kill-buffer-hook.
      (add-hook 'kill-buffer-hook
                '(lambda ()
                   (when chm-view-pid
                     ;; Kill `chm-view' process,
                     ;; and don't avoid output message.
                     (flet ((message (&rest args)))
                       (shell-command (format "kill -9 %s" chm-view-pid)))
                     ;; Reset `chm-view-pid'.
                     (setq chm-view-pid nil)))))))

(defun chm-view-get-unique-number ()
  "Get a unique number."
  (let (number buffer)
    (setq number '-1)
    (while (and (= number -1) (< number 49152))
      (setq number (nth 2 (current-time)))
      (while (> number '65535)
        (setq number (/ number '2))))
    (format "%s" number)))

(provide 'chm-view)

;;; chm-view.el ends here


;;; LocalWords:  archmage pid args

;;; gnus-switch.el --- Smart switch with gnus buffers

;; Filename: gnus-switch.el
;; Description: Smart stitch with gnus buffers
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-12-08 17:57:38
;; Version: 0.2
;; Last-Updated: 2008-12-20 12:39:30
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/gnus-switch.el
;; Keywords: gnus, switch, smart
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
;; Smart switch with gnus buffers
;;

;;; Installation:
;;
;; Put gnus-switch.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'gnus-switch)
;;
;; No need more.

;;; Change log:
;;
;; 2008/12/20
;;      Fix the bug of `gnus-switch-off', sometimes can't quit from
;;      gnus group buffer.
;;
;; 2008/12/08
;;      First released.
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

(defgroup gnus-switch nil
  "Smart switch with gnus buffers."
  :group 'gnus)

(defcustom gnus-switch-buffer-equal-list
  '("*Group*" "*BBDB*")
  "The list that contain buffer string and use `string-equal' for compare."
  :type 'list
  :group 'gnus-switch)

(defcustom gnus-switch-buffer-match-list
  '("\*Summary" "\*mail" "\*wide" "\*reply" "\*Article")
  "The list that contain buffer regexp and use `string-match' for compare."
  :type 'list
  :group 'gnus-switch)

(defcustom gnus-switch-on-after-hook nil
  "Run hook after do `gnus-switch-on'."
  :type 'hook
  :group 'gnus-warning)

(defvar gnus-switch-before-window-configuration nil
  "This variable storage window configuration before `gnus-switch-on'.")

(defun gnus-switch ()
  "Switch gnus buffers smart."
  (interactive)
  (let ((bufname (buffer-name)))
    (if (gnus-switch-buffer-exist bufname)
        (gnus-switch-off) ;; If current buffer is match of gnus buffers, switch off
      (gnus-switch-on)    ;; If current buffer is not match of gnus buffers, switch on
      )))

(defun gnus-switch-on ()
  "Switch gnus buffer on."
  (setq gnus-switch-before-window-configuration (current-window-configuration))
  (delete-other-windows)
  (if (get-buffer "*Group*")
      (switch-to-buffer "*Group*")
    (gnus))
  (run-hooks 'gnus-switch-on-after-hook))

(defun gnus-switch-off ()
  "Swtich gnus buffer off."
  (let (buf bufname)
    (when (and gnus-switch-before-window-configuration
               (boundp 'gnus-switch-before-window-configuration))
      (set-window-configuration gnus-switch-before-window-configuration))
    ;; Bury buffer if is still "gnus-buffer"
    ;; after change window configuration.
    (dolist (buf (buffer-list))
      (setq bufname (buffer-name buf))
      (if (gnus-switch-buffer-exist bufname)
          (bury-buffer buf)))))

(defun gnus-switch-buffer-exist (bufname)
  "If gnus buffer have exist, return t.
Otherwise return nil."
  (let (buffer-exist-p)
    (catch 'exist
      (dolist (element gnus-switch-buffer-equal-list)
        (when (string-equal element bufname)
          (setq buffer-exist-p t)
          (throw 'exist "Equal with `gnus-switch-buffer-equal-list'.")))
      (dolist (element gnus-switch-buffer-match-list)
        (when (string-match (regexp-quote element) bufname)
          (setq buffer-exist-p t)
          (throw 'exist "Match with `gnus-switch-buffer-match-list'."))))
    buffer-exist-p))

(provide 'gnus-switch)

;;; gnus-switch.el ends here

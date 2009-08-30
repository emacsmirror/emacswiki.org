;;; auto-install-extension.el --- Some extension functions for auto-install.el

;; Filename: auto-install-extension.el
;; Description: Some extension functions for auto-install.el
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-01-07 20:17:37
;; Version: 0.1
;; Last-Updated: 2009-01-07 20:17:37
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/auto-install-extension.el
;; Keywords: auto-install
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `auto-install' `w3m-util'
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
;; Some extension functions for auto-install.el
;;
;; This package contain some extension functions for auto-install.el
;; Details use see auto-install.el, at:
;; http://www.emacswiki.org/emacs/download/auto-install.el
;;
;; You can below commands:
;;
;;      `auto-install-from-w3m'         download elisp files with links in w3m
;;
;; Tips:
;;
;;      By default, `auto-install-from-w3m' downloads the file of the
;;      current link.  If you select text in the w3m buffer,
;;      `auto-install-from-w3m' looks for the all valid link in the
;;      region and install them.
;;
;;


;;; Installation:
;;
;; Put auto-install-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'auto-install-extension)
;;
;; No need more.

;;; Customize:
;;
;; `auto-install-from-w3m-confirm' whether need confirm when download file from w3m.
;;
;; All of the above can customize by:
;;      M-x customize-group RET auto-install-extension RET
;;

;;; Change log:
;;
;; 2009/01/07
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
(require 'auto-install)
(require 'w3m-util)

;;; Code:

(defcustom auto-install-from-w3m-confirm t
  "Whether confirmation is needed to download a file from w3m.
Nil means no confirmation is needed."
  :type 'boolean
  :group 'auto-install)

(defun auto-install-from-w3m ()
  "Download elisp file from w3m."
  (interactive)
  (if (eq major-mode 'w3m-mode)
      (if (or (not auto-install-from-w3m-confirm)
              (yes-or-no-p "Do you want to save and install elisp file from current buffer?"))
          (if mark-active
              (save-excursion
                (let (mark-start mark-end out-bound)
                  ;; Get mark position.
                  (setq mark-start (region-beginning))
                  (setq mark-end (region-end))
                  (deactivate-mark)
                  (goto-char mark-start)
                  ;; Search loop.
                  (while (and (not out-bound)
                              (not (auto-install-w3m-no-next-link-p)))
                    (or (w3m-anchor (point)) (goto-char (auto-install-w3m-get-next-link-start)))
                    (if (<= (point) mark-end)
                        ;; Download link.
                        (progn
                          (auto-install-download (w3m-anchor (point)))
                          (auto-install-w3m-get-anchor-end))
                      ;; for break out `while' loop.
                      (setq out-bound t)))))
            (if (w3m-anchor (point))
                (auto-install-download (w3m-anchor (point)))
              (message "No url at point."))))
    (message "This command is only for `w3m-mode'.")))

(defun auto-install-w3m-no-next-link-p ()
  "Whether there is no next link after the cursor.
Return t if there is no next link; otherwise, return nil."
  (save-excursion
    (equal (point) (auto-install-w3m-get-next-link-start))))

(defun auto-install-w3m-get-next-link-start ()
  "Move cursor to the start of the next link.  Return point."
  (catch 'reach
    (while (next-single-property-change (point) 'w3m-anchor-sequence) ;jump to next anchor
      (goto-char (next-single-property-change (point) 'w3m-anchor-sequence))
      (when (w3m-anchor (point))        ;return point when current is valid link
        (throw 'reach nil))))
  (point))

(defun auto-install-w3m-get-anchor-end ()
  "Move cursor to the end of the current anchor.  Return point."
  (goto-char (or (next-single-property-change (point) 'w3m-anchor-sequence) ;get end position of anchor
                 (point))))

(provide 'auto-install-extension)

;;; auto-install-extension.el ends here

;;; anything-irfc.el --- Integration irfc.el with anything.el

;; Filename: anything-irfc.el
;; Description: Integration irfc.el with anything.el
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-02-13 03:09:36
;; Version: 0.1
;; Last-Updated: 2009-02-13 03:09:36
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/anything-irfc.el
;; Keywords: irfc, anything, rfc
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `irfc', `anything'
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
;; Integration irfc.el with anything.el.
;;
;; Below are comands you can use:
;;
;; `anything-irfc-table-jump'
;;      Jump table or content.
;;
;; You can also make this package integrate with `anything',
;; just setup like below:
;;
;; (setq anything-sources
;;       (list
;;        anything-c-source-irfc-table-jump
;;        ))
;;

;;; Installation:
;;
;; Put anything-irfc.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'anything-irfc)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET anything-irfc RET
;;

;;; Change log:
;;
;; 2009/02/13
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
(require 'irfc)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar anything-c-source-irfc-table-jump
  '((name . "Irfc Table Switch")
    (init . (anything-irfc-init))
    (candidates-in-buffer)
    (action . (("Jump to content" . (lambda (candidate)
                                      (let (head-name page-number)
                                        (with-current-buffer anything-current-buffer
                                          ;; Jump table.
                                          (irfc-page-table)
                                          (search-forward candidate nil t)
                                          (move-beginning-of-line 1)
                                          (looking-at irfc-table-regex)
                                          (move-end-of-line 1)
                                          ;; Get page number and head name.
                                          (setq head-name (match-string 0))
                                          (setq head-name (replace-regexp-in-string "[\\. ]+\\([0-9]+\\)$" "" head-name))
                                          (setq head-name (replace-regexp-in-string "^[ ]+" "" head-name))
                                          (setq page-number (string-to-number (match-string 2)))
                                          ;; Jump page.
                                          (irfc-page-goto page-number)
                                          ;; Search table item.
                                          (search-forward head-name nil t)
                                          ;; Indent.
                                          (back-to-indentation)))))
               ("Jump to table" . (lambda (candidate)
                                    (with-current-buffer anything-current-buffer
                                      ;; Jump table.
                                      (irfc-page-table)
                                      ;; Search table item.
                                      (search-forward candidate nil t)
                                      ;; Indent.
                                      (back-to-indentation))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun anything-irfc-table-jump ()
  "Jump irfc table."
  (interactive)
  (anything 'anything-c-source-irfc-table-jump))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Uilites Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun anything-irfc-init ()
  "Init `anything-c-source-irfc-table-jump'."
  (let ((anything-buffer (anything-candidate-buffer 'global))
        (table-list (nreverse (anything-irfc-get-table-list))))
    (with-current-buffer anything-buffer
      (dolist (table-item table-list)
        (insert (format "%s\n" table-item))))))

(defun anything-irfc-get-table-list ()
  "Get irfc table list."
  (when (and (eq major-mode 'irfc-mode)
             (irfc-have-table-p))
    (save-excursion
      (goto-char (point-min))
      (let* ((case-fold-search t)
             (start-position (re-search-forward "^Table of Contents$" nil t))
             (end-position (re-search-forward "^[0-9\\.]+" nil t))
             table-list)
        (goto-char start-position)
        (while (re-search-forward irfc-table-regex end-position t)
          (add-to-list 'table-list (match-string 0)))
        table-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Setup keybinding ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key irfc-mode-map (kbd "a") 'anything-irfc-table-jump)

(provide 'anything-irfc)

;;; anything-irfc.el ends here


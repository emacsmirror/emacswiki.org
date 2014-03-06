;;; org-extension.el --- Some extensions for Org mode

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-06-17 08:34:21
;; Version: 1.0
;; Last-Updated: 2008-07-27 11:01:03
;; URL: not distributed yet
;; Keywords: org
;; Compatibility: GNU Emacs 23.0.60.1

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

;; Features that might be required by this library:
;;
;;  None
;;

;;; Installation:
;;
;; Copy org-extension.el to your load-path and add to your ~/.emacs
;;
;;  (require 'org-extension)
;;
;; No need more

;;; Commentary:
;;
;; This packages have some extensions for more quickly operation in Org mode.
;;

;;; Change log:
;;
;; 2008/06/17
;;         First release.
;;

;;; Acknowledgments:
;;
;; Not yet
;;

;;; TODO
;;
;; None
;;

;;; Code:

(defun org-fore-to-heading ()
  "Move to next heading line, or end of this line if it's a heading."
  (interactive)
  (org-end-of-line)
  (let (found)
    (save-excursion
      (while (not found)
        (or (re-search-forward (concat "^\\(?:" outline-regexp "\\)")
                               nil t)
            (end-of-line))
        (setq found (point))))
    (goto-char found)
    found))

(defun org-forward-same-level-or-end-subtree (arg)
  "Move forward same level or end of subtree"
  (interactive "p")
  (outline-back-to-heading t)
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion
                              (org-get-next-sibling))))
      (if point-to-move-to
          (progn
            (goto-char point-to-move-to)
            (setq arg (1- arg)))
        (org-end-of-subtree)
        (setq arg 0)))))

(defun org-end-of-heading ()
  "Move end of current heading."
  (interactive)
  (org-back-to-heading t)
  (outline-next-heading)
  (if (bolp)
      (forward-char -1)))

(defun org-archive-all-done-item ()
  "Archive all item that have with prefix DONE."
  (interactive)
  (save-excursion
    (show-all)
    (goto-char (point-min))
    (if (search-forward-regexp "^[\\*]+ DONE" nil t)
        (progn
          (goto-char (point-min))
          (while (search-forward-regexp "^[\\*]+ DONE" nil t)
            (org-advertized-archive-subtree))
          (org-display-all-todo-item)
          (message "Archive finished"))
      (org-display-all-todo-item)
      (message "No need to archive"))))

(defun org-switch-item-to-done ()
  "Switch current org item to DONE."
  (interactive)
  (save-excursion
    (org-map-tree '(lambda () (org-todo "DONE"))))
  (org-display-all-todo-item))

(defun org-insert-cur-item ()
  "Insert current item."
  (interactive)
  (if (bolp)
      (call-interactively 'move-beginning-of-line)
    (call-interactively 'org-end-of-heading))
  (call-interactively 'org-insert-heading))

(defun org-insert-sub-item()
  "Insert one sub item"
  (interactive)
  (org-insert-cur-item)
  (org-metaright))

(defun org-insert-sup-item()
  "Insert one father item"
  (interactive)
  (org-insert-cur-item)
  (org-metaleft))

(defun org-display-all-todo-item()
  "Automatic save and display all todo items in Org mode."
  (interactive)
  (save-excursion
    (save-buffer)
    (org-show-todo-tree t)))

(defun org-insert-cur-todo-heading ()
  "Insert todo item"
  (interactive)
  (if (bolp)
      (call-interactively 'move-beginning-of-line)
    (call-interactively 'org-end-of-heading))
  (call-interactively 'org-insert-todo-heading))

(defun org-insert-sup-todo-heading ()
  "Insert todo item, and shift left."
  (interactive)
  (org-insert-cur-todo-heading)
  (org-metaleft))

(defun org-insert-sub-todo-heading ()
  "Insert todo item, and shift right."
  (interactive)
  (org-insert-cur-todo-heading)
  (org-metaright))

(defun org-subtree-shiftleft ()
  "Do `org-shiftleft' with element of current substree."
  (interactive)
  (save-excursion
    (org-map-tree 'org-shiftleft)))

(defun org-subtree-shiftright ()
  "Do `org-shiftright' with element of current substree."
  (interactive)
  (save-excursion
    (org-map-tree 'org-shiftright)))

(defun org-toggle-iimage-in-org ()
  "display images in your org file"
  (interactive)
  (if (face-underline-p 'org-link)
      (set-face-underline-p 'org-link nil)
    (set-face-underline-p 'org-link t))
  (iimage-mode))

(defun org-switch-done-and-archive ()
  "Switch item to DONE, and archive it."
  (interactive)
  (org-switch-item-to-done)
  (org-archive-all-done-item))

(defun org-switch-done-and-archive_ ()
  "Switch item to DONE, and archive it."
  (interactive)
  (org-switch-item-to-done)
  (org-archive-all-done-item_))

(defun org-archive-all-done-item_ ()
  "Archive all item that have with prefix DONE."
  (interactive)
  (save-excursion
    (show-all)
    (goto-char (point-min))
    (if (search-forward-regexp "^[\\*]+ DONE" nil t)
        (progn
          (goto-char (point-min))
          (while (search-forward-regexp "^[\\*]+ DONE" nil t)
            (org-advertized-archive-subtree))
          (message "Archive finished"))
      (message "No need to archive"))))

(provide 'org-extension)

;;; org-extension.el ends here

;;; LocalWords:  debian subtree advertized todo metaright metaleft shiftleft
;;; LocalWords:  substree shiftright iimage

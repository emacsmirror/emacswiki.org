;;; imenu-tree.el --- A mode to view imenu using tree-widget

;; Copyright 2007 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Version: $Id: imenu-tree.el,v 1.1.1.1 2007-03-13 13:16:10 ywb Exp $
;; Keywords: help, convenience
;; 
;; This file is part of PDE (Perl Development Environment).
;; But it is useful for generic programming.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Dependencies:
;; windata.el   -- http://www.emacswiki.org/cgi-bin/wiki/windata.el
;; tree-mode.el -- http://www.emacswiki.org/cgi-bin/wiki/tree-mode.el

;;; Installation:
;; 1. Download Icons from http://www.emacswiki.org/cgi-bin/wiki/ImenuTreeIcons
;; and rename the file with suffix .tar.gz to extract it property.
;; Put the `tree-widget' directory to load-path.
;; 
;; 2. Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'imenu-tree)

;;; Code:

(provide 'imenu-tree)
(eval-when-compile
  (require 'cl))

(require 'imenu)
(require 'tree-mode)
(require 'windata)

(defgroup imenu-tree nil
  "Display imenu using tree-widget"
  :group 'convenience
  :group 'pde)

(defcustom imenu-tree-create-buffer-function nil
  "*A function to create buffer for insert imenu tree"
  :group 'imenu-tree
  :type 'function)

(defcustom imenu-tree-name `(concat mode-name ": " (or (buffer-name) "<NIL>"))
  "*Tree imenu root name. "
  :group 'imenu-tree
  :type 'sexp)

;;;###autoload
(defcustom imenu-tree-icons
  '(("Types" . "function")
    ("Variables" . "variable"))
  "*A list to search icon for the button in the tree.
The key is a regexp match the tree node name. The value is the icon
name for the children of the tree node."
  :group 'imenu-tree
  :type '(alist :keytype regexp :value-type string))

(defcustom imenu-tree-windata
  '(frame left 0.3 delete)
  "*Arguments to set the window buffer display.
See `windata-display-buffer' for setup the arguments."
  :group 'imenu-tree
  :type 'sexp)

(defcustom imenu-tree-auto-update nil
  "*Non-nil means auto update imenu-tree."
  :group 'imenu-tree
  :type 'boolean)

(defcustom imenu-tree-update-interval 2
  "*Seconds between update imenu tree."
  :type 'integer
  :group 'imenu-tree)

(defvar imenu-tree-need-update nil)
(defvar imenu-tree-update-timer nil)
(defvar imenu-tree-buffer nil)
(defvar imenu-tree nil)

(define-derived-mode imenu-tree-mode tree-mode "Imenu-Tree"
  "A mode to display tree of imenu"
  (tree-widget-set-theme "imenu")
  (add-hook 'tree-mode-delete-tree-hook 'tree-mode-kill-buffer))

;;;###autoload 
(defun imenu-tree (arg)
  "Display tree view of imenu.
With prefix argument, select imenu tree buffer window."
  (interactive "P")
  (let ((old-tree (and (local-variable-p 'imenu-tree) imenu-tree))
        (buf (current-buffer))
        tree)
    (if (and (local-variable-p 'imenu-tree-buffer)
             (buffer-live-p imenu-tree-buffer))
        (with-current-buffer imenu-tree-buffer
          (if (and old-tree (memq old-tree tree-mode-list))
              (setq tree old-tree)
            (setq tree (tree-mode-insert (imenu-tree-widget buf)))))
      (let ((buffer (if (functionp imenu-tree-create-buffer-function)
                        (funcall imenu-tree-create-buffer-function buf)
                      (get-buffer-create "*imenu-tree*"))))
        (set (make-local-variable 'imenu-tree-buffer) buffer)
        (when imenu-tree-auto-update
          (or imenu-tree-update-timer
              (imenu-tree-toggle-auto-update t))
          (set (make-local-variable 'imenu-tree-need-update) nil)
          (add-hook 'after-change-functions 'imenu-tree-after-change nil t))
        (add-hook 'kill-buffer-hook 'imenu-tree-kill nil t)
        (with-current-buffer buffer
          (unless (eq major-mode 'imenu-tree-mode)
            (imenu-tree-mode))
          (setq tree (tree-mode-insert (imenu-tree-widget buf))))))
    (set (make-local-variable 'imenu-tree) tree)
    (let ((win (get-buffer-window imenu-tree-buffer)))
      ;; if imenu-tree-buffer is visible, do nothing
      (unless win
        (setq win (apply 'windata-display-buffer
                         imenu-tree-buffer
                         imenu-tree-windata))
        (select-window win))
      (with-selected-window win
        (unless (widget-get tree :open)
          (widget-apply-action tree))
        (goto-char (widget-get tree :from))
        (recenter 1))
      (if arg
          (select-window win)))))

(defun imenu-tree-kill ()
  (let ((tree imenu-tree))
    (when (and tree
               imenu-tree-buffer
               (buffer-live-p imenu-tree-buffer))
      (with-current-buffer imenu-tree-buffer
        (ignore-errors
          (tree-mode-delete tree))))))

(defun imenu-tree-show ()
  "If the `imenu-tree' of current buffer is not visible, show the tree."
  (interactive)
  (let (win)
    (when (and imenu-tree
               (setq win (get-buffer-window imenu-tree-buffer)))
      (let ((pos (window-point win)))
        (if (not (and (>= pos (widget-get imenu-tree :from))
                      (<= pos (widget-get imenu-tree :to))))
            (set-window-start win (widget-get imenu-tree :from)))))))

(defun imenu-tree-toggle-auto-update (arg)
  "Toggle imenu-tree auto update.
With prefix argument, turn on auto update."
  (interactive "P")
  (setq imenu-tree-auto-update
        (if (null arg)
            (not imenu-tree-auto-update)
          (> (prefix-numeric-value arg) 0)))
  (and imenu-tree-update-timer
       (cancel-timer imenu-tree-update-timer))
  (when imenu-tree-auto-update
    (setq imenu-tree-update-timer
          (run-with-timer nil imenu-tree-update-interval
                          'imenu-tree-update-timer))
    (mapc (lambda (buf)
            (when (local-variable-if-set-p 'imenu-tree)
              (set (make-local-variable 'imenu-tree-need-update) t)
              (add-hook 'after-change-functions 'imenu-tree-after-change nil t)))
          (buffer-list))))

(defun imenu-tree-update-timer ()
  "Update and show the tree if needed."
  (imenu-tree-show)
  (when (and imenu-tree
             ;; the tree is visible
             (get-buffer-window imenu-tree-buffer) 
             imenu-tree-need-update
             ;; the buffer is not too large
             (not (> (buffer-size) imenu-auto-rescan-maxout)))
    (setq imenu--index-alist nil)
    (imenu--make-index-alist t)
    (let ((tree imenu-tree))
      (with-current-buffer imenu-tree-buffer
        (goto-char (widget-get tree :from))
        (tree-mode-reflesh)))
    (setq imenu-tree-need-update nil)))

(defun imenu-tree-after-change (&rest ignore)
  "Mark `imenu-tree-need-update' if make change in buffer"
  (setq imenu-tree-need-update t))

(defun imenu-tree-widget (buf)
  `(tree-widget
    :node (push-button
           :tag ,(with-current-buffer buf
                   (eval imenu-tree-name))
           :format "%[%t%]\n"
           :notify tree-mode-reflesh-parent)
    :dynargs imenu-tree-expand
    :has-children t
    :buffer ,buf
    :open t))

(defun imenu-tree-item (item buf icon)
  (if (listp (cdr item))
      `(tree-widget
        :node (push-button
               :tag ,(car item)
               :button-icon "bucket"
               :notify tree-mode-reflesh-parent
               :format "%[%t%]\n")
        :dynargs imenu-tree-expand-bucket
        :has-children t)
    `(push-button
      :tag ,(car item)
      :imenu-marker ,(let ((pos (cdr item)))
                       (cond ((markerp pos) pos)
                             ((numberp pos)
                              (set-marker (make-marker) pos buf))
                             ((overlayp pos)
                              (set-marker (make-marker) (overlay-start pos) buf))
                             (t (error "Unknown position type: %S" pos))))
      :button-icon ,icon
      :format "%[%t%]\n"
      :notify imenu-tree-select)))

(defun imenu-tree-select (node &rest ignore)
  (let ((marker (widget-get node :imenu-marker)))
    (select-window (display-buffer (marker-buffer marker)))
    (goto-char marker)))

(defun imenu-tree-expand-bucket (bucket)
  (let ((tree bucket) path buf index name)
    (while (and (tree-widget-p tree)
                (widget-get tree :parent))
      (push (widget-get (widget-get tree :node) :tag) path)
      (setq tree (widget-get tree :parent)))
    (setq buf (widget-get tree :buffer)
          name (car (last path)))
    (setq index (buffer-local-value 'imenu--index-alist buf))
    (while path
      (setq index (cdr (assoc (car path) index)))
      (if (null index)
          (error "Type g to update imenu index"))
      (setq path (cdr path)))
    (mapcar (lambda (item)
              (imenu-tree-item item buf
                               (or (assoc-default name imenu-tree-icons
                                                  'string-match)
                                   "function")))
            index)))

(defun imenu-tree-expand (tree)
  (or (widget-get tree :args)
      (let ((buf (widget-get tree :buffer))
            index)
        (setq index (with-current-buffer buf
                      (setq imenu--index-alist nil)
                      (imenu--make-index-alist t)
                      (delq nil imenu--index-alist)))
        (mapcar (lambda (item)
                  (imenu-tree-item item buf "function"))
                index))))

(defun imenu-tree-display ()
  (interactive)
  (let ((widget (widget-at (1- (line-end-position))))
        marker)
    (if (setq marker (widget-get widget :imenu-marker))
        (with-selected-window (display-buffer (marker-buffer marker))
          (goto-char marker)))))

(define-key imenu-tree-mode-map "\C-o" 'imenu-tree-display)

;;; imenu-tree.el ends here

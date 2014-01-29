;;; dirtree.el --- Directory tree views

;; Copyright (C) 2010 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Maintainer: Ye Wenbin <wenbinye@gmail.com>
;; Package-Requires: ((tree-mode "1.1.1.1") (windata "0"))
;; Created: 09 Jan 2010
;; Version: 0.01
;; Keywords

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

;;; Commentary:
;; There are several dir-tree widget implements, but I need these features:
;;  1. display many directory in one buffer to reduce buffer numbers
;;  2. reuse directory tree when already there is one
;;  3. use my favarite key binding
;;
;; So I wrote this one use `tree-mode'.
;; 
;; See also:
;; http://www.splode.com/~friedman/software/emacs-lisp/src/dirtree.el
;; http://svn.halogen.kharkov.ua/svn/repos/alex-emacs-settings/emhacks/dir-tree.el

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (autoload 'dirtree "dirtree" "Add directory to tree view" t)

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'tree-mode)
(require 'windata)
(require 'dired-x)

(defgroup dirtree nil
  "Directory tree views"
  :group 'tools)

(defcustom dirtree-windata '(frame left 0.3 delete)
  "*Arguments to set the window buffer display.
See `windata-display-buffer' for setup the arguments."
  :type 'sexp
  :group 'dirtree)

(defcustom dirtree-buffer "*dirtree*"
  "*Buffer name for `dirtree'"
  :type 'string
  :group 'dirtree)

(define-widget 'dirtree-dir-widget 'tree-widget
  "Directory Tree widget."
  :dynargs        'dirtree-expand
  :has-children   t)

(define-widget 'dirtree-file-widget 'push-button
  "File widget."
  :format         "%[%t%]\n"
  :button-face    'default
  :notify         'dirtree-select)

(defun dirtree-show ()
  "Show `dirtree-buffer'. Create tree when no parent directory find."
  (interactive)
  (let ((buffer (get-buffer-create dirtree-buffer))
        (dir default-directory)
        trees tree button path)
    (with-current-buffer buffer
      (setq trees tree-mode-list)
      (while (and trees
                  (not tree))
        (if (string-match (concat "^" (regexp-quote (widget-get (car trees) :file))) dir)
            ;; if parent directory in buffer
            (setq tree (car trees))
          (setq trees (cdr trees)))))
    (if tree
        (progn
          (setq path (split-string (file-relative-name buffer-file-name (widget-get tree :file)) "/"))
          (dirtree (widget-get tree :file) t)
          (setq button (tree-mode-find-node tree path))
          (if button
              (goto-char (widget-get (car button) :from))))
      (call-interactively 'dirtree))))

(defun dirtree (root select)
  "create tree of `root' directory
With prefix arguement select `dirtree-buffer'"
  (interactive "DDirectory: \nP")
  (let ((buffer (get-buffer-create dirtree-buffer))
        tree win)
    (with-current-buffer buffer
      (unless (eq major-mode 'dirtree-mode)
        (dirtree-mode))
      (dolist (atree tree-mode-list)
        (if (string= (widget-get atree :file) root)
            (setq tree atree)))
      (or tree
          (setq tree (tree-mode-insert (dirtree-root-widget root)))))
    (setq win (get-buffer-window dirtree-buffer))
    (unless win
      (setq win (apply 'windata-display-buffer dirtree-buffer dirtree-windata))
      (select-window win))
    (with-selected-window win
      (unless (widget-get tree :open)
        (widget-apply-action tree))
      (goto-char (widget-get tree :from))
      (recenter 1))
    (if select
        (select-window win))))

(define-derived-mode dirtree-mode tree-mode "Dir-Tree"
  "A mode to display tree of directory"
  (tree-widget-set-theme "folder"))

(defun dirtree-root-widget (directory)
  "create the root directory"
  `(dirtree-dir-widget
    :node (dirtree-file-widget
           :tag ,directory
           :file ,directory)
    :file ,directory
    :open t))

(defun dirtree-expand (tree)
  "expand directory"
  (or (widget-get tree :args)
      (let ((directory (widget-get tree :file))
            (re (dired-omit-regexp))
            dirs files basename)
        (dolist (file (directory-files directory t))
          (setq basename (file-name-nondirectory file))
          (unless (string-match re basename)
            (if (file-directory-p file)
                (push (cons file basename) dirs)
              (push (cons file basename) files))))
        (setq dirs (sort dirs (lambda (a b) (string< (cdr a) (cdr b)))))
        (setq files (sort files (lambda (a b) (string< (cdr a) (cdr b)))))
        (append
         (mapcar (lambda (file)
                   `(dirtree-dir-widget
                     :file ,(car file)
                     :node (dirtree-file-widget
                            :tag ,(cdr file)
                            :file ,(car file))))
                 dirs)
         (mapcar (lambda (file)
                   `(dirtree-file-widget
                     :file ,(car file)
                     :tag ,(cdr file)))
                 files)))))

(defun dirtree-select (node &rest ignore)
  "Open file in other window"
  (let ((file (widget-get node :file)))
    (and file
         (find-file-other-window file))))

(defun dirtree-display ()
  "Open file under point"
  (interactive)
  (let ((widget (widget-at (1- (line-end-position))))
        file)
    (if (setq file (widget-get widget :file))
        (find-file-other-window file))))

(define-key dirtree-mode-map "\C-o" 'dirtree-display)
(provide 'dirtree)
;;; dirtree.el ends here

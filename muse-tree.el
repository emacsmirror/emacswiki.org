;;; muse-tree.el --- Show muse project using tree-widget

;; Copyright (C) 2008 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Maintainer: Ye Wenbin <wenbinye@gmail.com>
;; Created: 12 Apr 2008
;; Version: 0.01
;; Keywords: help, convenience

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

;; muse-tree implement two additional features for muse project:
;;  1. notify
;;   use muse-tree-notify/unnotify-project/file to highlight or unhighlight
;;   the button of the project or file.
;;   This feature can give a hit for use to publish file or project.
;;  2. mark up
;;   muse-tree-tag command can mark or unmark some files or projects. 
;;   Commands can apply to tagged files or projects.

;;; TODO:
;;; 1. Add more commands for notified or tagged node
;;; 2. Add a timer to monitor which file or project should publish.
;;;    Make everything automaticly.

;;; Dependencies:
;;  tree-mode.el -- http://www.emacswiki.org/cgi-bin/wiki/tree-mode.el

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'muse-tree)

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'tree-mode)

(defvar muse-tree-buffer "*muse-tree*"
  "*Buffer name for the buffer display tree-widget")

(defvar muse-tree-project-notify-list nil
  "")

(defvar muse-tree-file-notify-list nil
  "")

(defface muse-tree-button-face
  '((((class color)) :inherit widget-button))
  "Face for normal button"
  :group 'muse-tree)

(defface muse-tree-notify-face
  '((((class color) (background light))
     (:foreground "blue" :bold t))
    (((class color) (background dark))
     (:foreground "cyan" :bold t))
    (t (:bold t)))
  "Face for markup button"
  :group 'muse-tree)

(define-derived-mode muse-tree-mode tree-mode "MuseTree"
  "Display muse project using tree-widget.

\\{muse-tree-mode-map}"
  (tree-widget-set-theme "folder"))

(defun muse-tree ()
  (interactive)
  (unless (get-buffer muse-tree-buffer)
    (with-current-buffer (get-buffer-create muse-tree-buffer)
      (muse-tree-mode)
      (tree-mode-insert '(tree-widget
                       :node (push-button
                              :format "%[%t%]\n"
                              :tag "Muse"
                              :notify tree-mode-toggle-expand-node)
                       :open t
                       :dynargs muse-tree-expand))
      (widget-setup)
      (goto-char (point-min))))
  (switch-to-buffer muse-tree-buffer))

(defun muse-tree-expand (tree)
  (or (widget-get tree :args)
      (mapcar (lambda (proj)
                `(tree-widget
                  :node (push-button
                         :format "%[%t%]%d"
                         :doc ""
                         :tag ,(car proj)
                         :button-face ,(if (member (car proj) muse-tree-project-notify-list)
                                           'muse-tree-notify-face
                                         'muse-tree-button-face)
                         :notify tree-mode-toggle-expand-node)
                  :dynargs muse-tree-expand-project))
              muse-project-alist)))

(defun muse-tree-expand-project (tree)
  (or (widget-get tree :args)
      (mapcar (lambda (file)
                (list 'push-button
                      :format "%[%t%]%d"
                      :doc ""
                      :tag (car file)
                      :button-face (if (member (cdr file) muse-tree-file-notify-list)
                                       'muse-tree-notify-face
                                     'muse-tree-button-face)
                      :muse-file (cdr file)
                      :notify 'muse-tree-find-file))
              (muse-project-file-alist (tree-mode-node-tag tree)))))

(defun muse-tree-find-file (node &rest ignore)
  (find-file-other-window (widget-get node :muse-file)))
        
(defun muse-tree-notify-project (project)
  (with-current-buffer muse-tree-buffer
    (if (consp project)
        (setq project (car project)))
    (unless (member project muse-tree-project-notify-list)
      (add-to-list 'muse-tree-project-notify-list project)
      (muse-tree-markup-project project 'muse-tree-notify-face))))

(defun muse-tree-unnotify-project (project)
  (with-current-buffer muse-tree-buffer
    (if (consp project)
        (setq project (car project)))
    (when (member project muse-tree-project-notify-list)
      (setq muse-tree-project-notify-list (delete project muse-tree-project-notify-list))
      (muse-tree-markup-project project 'muse-tree-button-face))))

(defun muse-tree-notify-file (file)
  (with-current-buffer muse-tree-buffer
    (unless (member file muse-tree-file-notify-list)
      (add-to-list 'muse-tree-file-notify-list file)
      (muse-tree-markup-file file 'muse-tree-notify-face))))

(defun muse-tree-unnotify-file (file)
  (with-current-buffer muse-tree-buffer
    (when (member file muse-tree-file-notify-list)
      (setq muse-tree-file-notify-list (delete file muse-tree-file-notify-list))
      (muse-tree-markup-file file 'muse-tree-button-face))))

;;; which is better? set the face directly or recreate the widget?
(defun muse-tree-markup-project (project face)
  (let* ((tree (car (tree-mode-find-node (car tree-mode-list)
                                         (list project))))
         (button (car (widget-get tree :children))))
    (widget-put (tree-widget-node tree) :button-face face)
    ;; (overlay-put
    ;;  (widget-get (car (widget-get tree :children)) :button-overlay)
    ;;  'face face)
    (widget-put button :button-face face)
    (widget-value-set button (widget-value button))))

(defun muse-tree-markup-file (file face)
  (let* ((proj (muse-project-of-file file))
         (button (car (tree-mode-find-node (car tree-mode-list)
                                           (list (car proj)
                                                 (muse-page-name file)))))
         (args (widget-get (widget-get button :parent) :args)))
    (while args
      (when (string= (widget-get (car args) :muse-file)  file)
        (widget-put (car args) :button-face face)
        (setq args nil))
      (setq args (cdr args)))
    (widget-put button :button-face face)
    ;; (overlay-put (widget-get node :button-overlay) 'face face)))
    (widget-value-set button (widget-value button))))

(defun muse-tree-tag (button &optional arg)
  (interactive (list (tree-mode-button-current-line)
                     current-prefix-arg))
  (let ((on (> (length (widget-get button :doc)) 0)))
    (setq on (if (null arg)
                 (not on)
               (> (prefix-numeric-value arg) 0)))
    (widget-put button :doc (if on "*" ""))
    (widget-value-set button (widget-value button))))

(defun muse-tree-tagged-projects ()
  (with-current-buffer muse-tree-buffer
    (delq nil
          (mapcar (lambda (tree)
                    (let ((node (car (widget-get tree :children))))
                      (if (string= (widget-get node :doc) "*")
                          (widget-get node :tag))))
                  (cdr (widget-get (car tree-mode-list) :children))))))

(defun muse-tree-tagged-pages (&optional project)
  (if (consp project) (setq project (car project)))
  (with-current-buffer muse-tree-buffer
    (let ((root (car tree-mode-list))
          trees)
      (if project
          (setq trees (list (car (tree-mode-find-node root (list project)))))
        (setq trees (cdr (widget-get root :children))))
      (apply 'append
             (mapcar (lambda (tree)
                       (delq nil
                             (mapcar (lambda (but)
                                       (if (string= (widget-get but :doc) "*")
                                           (widget-get but :muse-file)))
                                     (cdr (widget-get tree :children)))))
                     trees)))))

(provide 'muse-tree)
;;; muse-tree.el ends here

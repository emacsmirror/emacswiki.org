;;; projectile-speedbar --- projectile integration for speedbar

;; Author: Anshul Verma <anshul.verma86@gmail.com>
;; Maintainer: Anshul Verma <anshul.verma86@gmail.com>
;; Copyright (C) 2014, 2009, Anshul Verma, all rights reserved.
;; Created: 2014
;; Version: 0.0.1
;; Last-Updated: 2014-06-13 03:15:56
;; URL: http://www.emacswiki.org/emacs/download/projectile-speedbar.el
;; Keywords: speedbar, sr-speedbar, projectile, projectile-speedbar.el
;; Compatibility: GNU Emacs 24
;;
;; Features that might be required by this library:
;;
;;  `speedbar' `sr-speedbar' `projectile'
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
;; This package sits on top of speedbar and projectile and provides an
;; easy to use and useful integration between the two.
;;
;; With this package when you switch between projects that work with
;; projectile, speedbar will automatically show the directly listing
;; of that project as well as expand the tree to show the file in the
;; project.
;;
;; To invoke this function manually:
;;
;; `nv-speedbar-open-current-buffer-in-tree
;;

;;; Installation
;;
;; Copy speedbar-projectile.el to your load-path and add this to ~/.emacs
;;
;;  (require 'projectile-emacs)
;;
;; Sometimes, when I am deep in a project tree, I like to use this shortcut
;; to see full context:
;;
;;  (global-set-key (kbd "M-<f2>") 'nv-speedbar-open-current-buffer-in-tree)
;;

;;; Customize:
;;
;;

;;; Change log:
;;
;; * 13 June 2014:
;;   * Anshul Verma
;;     * Initial feature with commentary
;;
;; * 13 June 2014
;;   * Anshul Verma
;;     * fix bug "should switch to file buffer after opening a file via projectile-find-file"
;;

;;; Acknowledgments
;;
;;    All emacsers ... :)
;;

;;; Bug
;;
;; * Should select the current buffer file in directory listing after project switch
;;

;;; TODO
;;
;;

(require 'speedbar)
(require 'sr-speedbar)

(defun nv-find-project-root ()
  (setq nv-current-dir (file-truename buffer-file-name))
  (while (not (file-exists-p (concat nv-current-dir ".git")))
    (setq nv-current-dir (file-name-directory (substring nv-current-dir 0 -1))))
  (concat nv-current-dir ""))

(defun nv-speedbar-project-refresh (root-dir)
  "Refresh the context of speedbar based on project root"
  (when (and (not (equal root-dir sr-speedbar-last-refresh-dictionary))
             (not (sr-speedbar-window-p)))
    (setq sr-speedbar-last-refresh-dictionary root-dir))
  (setq default-directory root-dir)
  (speedbar-refresh))

(defun nv-open-current-project-in-speedbar (root-dir)
  "Refresh speedbar to show current project in tree"
  (if (not (sr-speedbar-exist-p))
      (sr-speedbar-toggle))
  (nv-speedbar-project-refresh root-dir))

(defun nv-speedbar-expand-line-list (&optional arg)
  (when arg
    (re-search-forward (concat " " (car arg) "$"))
    (speedbar-expand-line (car arg))
    (speedbar-next 1)
    (nv-speedbar-expand-line-list (cdr arg))))

(defun nv-speedbar-open-current-buffer-in-tree ()
  (interactive)
  (let* ((root-dir (nv-find-project-root))
         (original-buffer-file-directory (file-name-directory (buffer-file-name)))
         (relative-buffer-path (car (cdr (split-string original-buffer-file-directory root-dir))))
         (parents (butlast (split-string relative-buffer-path "/")))
         (original-window (get-buffer-window)))
    (save-excursion
      (nv-open-current-project-in-speedbar root-dir)
      (select-window (get-buffer-window speedbar-buffer))
      (beginning-of-buffer)
      (nv-speedbar-expand-line-list parents)
      (if (not (eq original-window (get-buffer-window speedbar-buffer)))
          (select-window original-window)
        (other-window 1)))))

(add-hook 'projectile-find-dir-hook 'nv-speedbar-open-current-buffer-in-tree)
(add-hook 'projectile-find-file-hook 'nv-speedbar-open-current-buffer-in-tree)
(add-hook 'projectile-cache-projects-find-file-hook 'nv-speedbar-open-current-buffer-in-tree)
(add-hook 'projectile-cache-files-find-file-hook 'nv-speedbar-open-current-buffer-in-tree)

(provide 'projectile-speedbar)

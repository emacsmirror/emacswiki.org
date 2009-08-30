;;; dirnav.el --- Navigate folders and files with TAB
;;
;; Copyright (C) 2009 Marco Bardelli

;; Author: Marco <Bj> Bardelli <bardelli.marco@gmail.com>
;;
;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Navigate dirs with TAB and q splitting window.
;; View files quickly. Suggest `dired-details'
;; Features that might be required by this library:
;;
;;   `windmove'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
(unless (featurep 'windmove) (require 'windmove))
(defvar dirnav-mode-map (make-sparse-keymap))
(defvar dirnav-splitted-window-stack nil)
(defun dirnav-delete-window () "" (interactive)
  (if (> (length (window-list)) 1)
      (progn
	(delete-window)
	(balance-windows)
	(select-window (pop dirnav-splitted-window-stack)))
    (bury-buffer)))

(defun dirnav-view-to-right () "" (interactive)
  (unless (and (eq major-mode 'dired-mode) (featurep 'windmove))
    (error nil))
  (let ((entry (dired-get-file-for-visit)))
    (save-excursion
      (push (selected-window) dirnav-splitted-window-stack)
      (split-window-horizontally)
      (windmove-right)
      (if (file-directory-p entry)
	  (or (find-buffer-visiting entry)(find-file entry))
	(view-file entry))
      (balance-windows))))

(defun dirnav-view-to-bottom () "" (interactive)
  (unless (eq major-mode 'dired-mode)
    (error nil))
  (let ((entry (dired-get-file-for-visit)))
    (save-excursion
      (push (selected-window) dirnav-splitted-window-stack)
      (split-window-vertically)
      (windmove-down)
      (if (file-directory-p entry)
	  (or (find-buffer-visiting entry)(find-file entry))
	(view-file entry))
      (balance-windows))))

(define-minor-mode dirnav-mode "Nav Dirs" nil " NAV-DIR" dirnav-mode-map
  (define-key dirnav-mode-map (kbd "TAB") 'dirnav-view-to-right)
  (define-key dirnav-mode-map (kbd "M-TAB") 'dirnav-view-to-bottom)
  (define-key dirnav-mode-map (kbd "q") 'dirnav-delete-window))

   
(provide 'dirnav)
;;; dirnav.el ends here

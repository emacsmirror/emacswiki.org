;;; recentf-buffer.el --- creates the buffer with the list of recently open files.
;;
;; $Id$
;;
;; Time-stamp: <01-11-2004 11:46:26>
;;
;; Copyright (C) 2004 Eugene V. Markov

;; Author: Eugene V. Markov
;; Version: $Revision$
;; Keywords: bookmarks, placeholders, annotations

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; It's addition to recentf.el.
;;
;; Install.
;;
;; (load "recentf-buffer")
;; (global-set-key [?\C-c ?r ?f] 'recentf-open-files-in-simply-buffer)

;;; Code:

(require 'recentf)
(require 'wid-edit)

(recentf-mode 1)

(defun recentf-open-files-in-simply-buffer ()
  "Allow the user to open files in simply buffer."
  (interactive)
  (if (and (get-buffer (concat "*" recentf-menu-title " - More*"))
           (get-buffer-window
            (get-buffer (concat "*" recentf-menu-title " - More*"))))
      (select-window (get-buffer-window
                      (get-buffer (concat "*" recentf-menu-title " - More*"))))
    (setq recentf-windows-configure (current-window-configuration))
    (split-window-vertically)
    (other-window 1)
    (with-current-buffer
        (get-buffer-create (concat "*" recentf-menu-title " - More*"))
      (switch-to-buffer (current-buffer))
      (kill-all-local-variables)
      (setq truncate-lines t)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (let ((all (overlay-lists)))
        ;; Delete all the overlays.
        (mapcar 'delete-overlay (car all))
        (mapcar 'delete-overlay (cdr all)))
      ;; Insert the dialog header
      (widget-insert "Click on a file to open it or on Cancel to quit.\n\n")
      ;; Insert the list of files as buttons
      (setq recentf-max-fnl (let ((l recentf-list)(nl 0)(str))
                              (while (setq str (car l))
                                (if (> (length (file-name-nondirectory str)) nl)
                                    (setq nl (length (file-name-nondirectory str))))
                                (setq l (cdr l))
                                )
                              (symbol-value 'nl)))
      (setq recentf-max-fnl (+ recentf-max-fnl 2))
      (mapcar
       '(lambda (menu-element)
          (let ((menu-item (car menu-element))
                (file-path (cdr menu-element)))
            (widget-create 'push-button
                           :tag (concat " " (file-name-nondirectory menu-item)
                                        (make-string
                                         (- recentf-max-fnl
                                            (length (file-name-nondirectory menu-item)))
                                         ? ))
                           :help-echo (concat "Open " file-path)
                           :format "%[%t%]"
                           :notify 'recentf-open-files-in-simply-buffer-action
                           file-path)
            (widget-insert (format "%s\n" menu-item))))
       (funcall (or recentf-menu-filter 'identity)
                (mapcar '(lambda (item) (cons item item))
                        recentf-list)))
      (widget-insert "\n")
      ;; Insert the Cancel button
      (widget-create 'push-button
                     :notify 'recentf-open-files-in-simply-buffer-cancel
                     "Cancel")
      (let ((recentf-keymap1 (copy-keymap widget-keymap)))
        (define-key recentf-keymap1 "q" 'recentf-open-files-in-simply-buffer-cancel)
        (define-key recentf-keymap1  [up] 'widget-backward)
        (define-key recentf-keymap1 [down] 'widget-forward)
        (use-local-map recentf-keymap1))
      (widget-setup)
      (goto-char (point-min)))))

(defun recentf-open-files-in-simply-buffer-action (widget &rest ignore)
  (kill-buffer (current-buffer))
  (set-window-configuration recentf-windows-configure)
  (recentf-find-file (widget-value widget))
  )

(defun recentf-open-files-in-simply-buffer-cancel (&rest ignore)
  (interactive)
  (kill-buffer (current-buffer))
  (set-window-configuration recentf-windows-configure)
  (message "Command canceled."))

(provide 'recentf-buffer)
;;; recentf-buffer.el ends here.

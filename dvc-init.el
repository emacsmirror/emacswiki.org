;;; dvc-init.el --- 
;; 
;; Author: thierry
;; Maintainer: 
;; 
;; Created: dim. avril 26 19:49:14 2009 (+0200)
;; Version: 
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; 
;;; Commentary: 
;; My config for DVC 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;; Basics
(add-to-list 'load-path "/home/thierry/elisp/dvc/lisp/")
(require 'dvc-autoloads)
(dvc-insinuate-gnus)
;(require 'xhg-annotate)

;; Bof...
(setq dvc-tips-enabled nil)

;; Don't show time-stamp by default (i can toggle T-d)
(setq dvc-bookmarks-show-time-stamp nil)

;; Don't show partner url's on startup (T-u)
(setq dvc-bookmarks-show-partner-url nil)

;; Thats shouldn't be set but if not...
(setq vc-delete-logbuf-window t)

;; Reload backend in dvc-status
(defadvice dvc-add-files (after reload-status-buffer)
  "reload dvc-status after adding files"
  (dvc-status))
(ad-activate 'dvc-add-files)

(defadvice dvc-revert-files (after reload-status-buffer)
  "reload dvc-status after reverting files"
  (dvc-status))
(ad-activate 'dvc-revert-files)

(defadvice dvc-remove-files (after reload-status-buffer () activate)
  "update display after removing files"
  (dvc-status))

(add-hook 'xhg-log-mode-hook
          #'(lambda ()
              (font-lock-add-keywords nil '(("^\\+.*" . font-lock-variable-name-face)))
              (font-lock-add-keywords nil '(("^\\-.*" . font-lock-doc-face)))))

(add-hook 'xgit-diff-mode-hook
          #'(lambda ()
              (font-lock-add-keywords nil '(("^\\+.*" . font-lock-variable-name-face)))
              (font-lock-add-keywords nil '(("^\\-.*" . font-lock-doc-face)))))

;; Global keys for mq
(global-set-key (kbd "C-x Q N") 'xhg-qnew)
(global-set-key (kbd "C-x Q R") 'xhg-qrefresh)
(global-set-key (kbd "C-x Q A") 'xhg-qapplied)
(global-set-key (kbd "C-x Q S") 'xhg-qseries)
(global-set-key (kbd "C-x Q X") 'xhg-qsingle)
(global-set-key (kbd "C-x Q P") 'xhg-qpop)
(global-set-key (kbd "C-x Q p") 'xhg-qpush)
(global-set-key (kbd "C-x Q D") 'xhg-qdelete)
(global-set-key (kbd "C-x Q C") 'xhg-qconvert-to-permanent)
(global-set-key (kbd "C-x Q r") 'xhg-qrename)
(global-set-key (kbd "C-x Q h") 'xhg-qheader)
(global-set-key (kbd "C-x Q H") 'xhg-qrefresh-header)

;; Local keys for xhg in dired
(define-key dired-mode-map (kbd "C-c I") 'dvc-apply-patch)
(define-key dired-mode-map (kbd "C-x Q i") 'xhg-qimport)
(define-key dired-mode-map (kbd "C-c V a") 'dvc-bookmarks-dired-add-project)
(define-key dired-mode-map (kbd "C-x Q I") 'xhg-qinit)

;; Global keys for xhg
(global-set-key (kbd "C-x X G") 'xhg-revision-get-last-or-num-revision)

;; Convert with tailor from dvc
(defun tv-dvc-update-from-tailor ()
  (interactive)
  (let* ((target-dir (dvc-bookmarks-current-value 'local-tree))
         (conf-file (concat target-dir ".tailor.cfg")))
    (if (file-exists-p conf-file)
        (progn
          (apply #'call-process-shell-command "~/bin/tailor" nil "*tailor-log*" t
                 `("-c" ,conf-file))
          (display-buffer "*tailor-log*" t))
      (message "%s doesn't exist" conf-file))))

(when (require 'dvc-bookmarks)
  (define-key dvc-bookmarks-mode-map (kbd "M T") 'tv-dvc-update-from-tailor))

;; Convert git/svn to hg with hg convert

(global-set-key (kbd "C-x X C") 'xhg-convert)

;; Launch dvc-bookmarks in a special vertical buffer
(defvar dvc-bookmarks-miniwindow-p nil)
(defun tv-toggle-window-dvc-bookmarks ()
  (interactive)
  (flet ((openbm ()
           (progn
             ;(delete-other-windows)
             (split-window-horizontally 45)
             (other-window -1)
             (dvc-bookmarks)
             (setq dvc-bookmarks-miniwindow-p t)))
         (closebm ()
           (progn
             (when (bufferp (get-buffer "*dvc-bookmarks*"))
               (with-current-buffer "*dvc-bookmarks*"
                 (dvc-bookmarks-quit)
                 (kill-buffer (current-buffer)))
               (delete-window))
             (setq dvc-bookmarks-miniwindow-p nil))))
    (if dvc-bookmarks-miniwindow-p
        (closebm)
        (openbm))))

  
(global-set-key (kbd "<f11> &") 'tv-toggle-window-dvc-bookmarks)

;; clone quickly from dired
;; (defun xhg-dired-clone ()
;;   (interactive)
;;   (let* ((source (dired-filename-at-point))
;;          (target
;;           (read-string (format "Clone(%s)To: " (file-name-nondirectory source))
;;                        (file-name-directory source))))
;;     (xhg-clone source target)))

(define-key dired-mode-map (kbd "C-c V C") 'xhg-dired-clone)

;; «dvc-rename-from-dired» (to ".dvc-rename-from-dired")
(defun dvc-dired-rename (new-name)
  (interactive "sNewName")
  (let ((source (dired-filename-at-point)))
    (dvc-rename source new-name)))

(define-key dired-mode-map (kbd "C-c R") 'dvc-dired-rename)

;; «git-config» (to ".git-config")
(setq xgit-use-index 'never)

;; provide
(provide 'dvc-init)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dvc-init.el ends here

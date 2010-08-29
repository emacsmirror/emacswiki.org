;;; anything-mercurial.el --- 
;;
;; Copyright (C) 2008, 2009 Thierry Volpiatto, all rights reserved
;;
;; Filename: anything-mercurial.el
;; Description: 
;; Author: thierry
;; Maintainer: 
;; URL: 
;; Keywords: 

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; This file provide two sources for that provide support
;; for hg qpatchs within `anything'.
;; `anything-c-source-qapplied-patchs' and
;; `anything-c-source-qunapplied-patchs'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; Code:

;; Internal use only!
(defvar anything-qapplied-alist nil)
(defvar anything-c-qpatch-directory nil)
(defvar anything-c-qunpatch-directory nil)
(defvar anything-c-qapplied-show-headers t)

;; User variables
(defvar anything-hg-default-export-fname ".hg-anything-export"
  "*You can put in this file the path of your usual dir to export patchs.")

;; Clean up
(add-hook 'anything-before-initialize-hook #'(lambda ()
                                               (setq anything-c-qunpatch-directory nil)))

(add-hook 'anything-before-initialize-hook #'(lambda ()
                                               (setq anything-qapplied-alist nil)))

(add-hook 'anything-before-initialize-hook #'(lambda ()
                                               (setq anything-c-qpatch-directory nil)))


;;; Applied patchs
(defun anything-hg-init-applied ()
  (condition-case nil
      (setq anything-c-qpatch-directory
            (xhg-tree-root (expand-file-name
                            (if (eq major-mode 'dired-mode)
                                (dired-current-directory)
                                default-directory))))
    (error nil)))

(defun anything-hg-applied-candidates ()
  (condition-case nil
      (let ((applied-patchs
             (with-temp-buffer
               (apply #'call-process "hg" nil t nil
                      (if anything-c-qapplied-show-headers
                          `("qapplied" "-s" "-R" ,anything-c-qpatch-directory)
                          `("qapplied" "-R" ,anything-c-qpatch-directory)))
               (buffer-string)))
            (top
             (with-temp-buffer
               (apply #'call-process "hg" nil t nil
                      `("tip" "--template" "{rev}" "-R" ,anything-c-qpatch-directory))
               (buffer-string))))
        (setq top (string-to-number top))
        (setq applied-patchs (remove "" (split-string applied-patchs "\n")))
        (setq anything-qapplied-alist
              (loop for i in (reverse applied-patchs)
                 collect (list i top)
                 and do
                 (incf top -1)))
        (unless (or (string-match "abort:" (car applied-patchs))
                    (zerop (length applied-patchs)))
          (setq applied-patchs (reverse applied-patchs))))
    (error nil)))

(defun anything-hg-applied-persistent-action (elm)
  (let ((default-directory anything-c-qpatch-directory))
    (xhg-qpop)
    (anything-delete-current-selection)))

(defun anything-hg-applied-show-patch (elm)
  (let ((default-directory anything-c-qpatch-directory))
    (xhg-log (cadr (assoc elm anything-qapplied-alist)) nil t)))

(defun anything-hg-applied-refresh (elm)
  (let ((default-directory anything-c-qpatch-directory))
    (xhg-qrefresh)))

(defun anything-hg-applied-rename-header (elm)
  (let ((default-directory anything-c-qpatch-directory))
    (xhg-qrefresh-header)
    (save-window-excursion
      (when (get-buffer "*xhg-log*")
        (kill-buffer "*xhg-log*"))
      (xhg-log
       (cadr (assoc elm anything-qapplied-alist)) nil t))
    (save-excursion
      (display-buffer "*xhg-log*"))))

(defun anything-hg-applied-qnew (elm)
  (let ((default-directory anything-c-qpatch-directory))
    (xhg-qnew (xhg-qnew-name-patch)
              "New patch")))

(defun anything-hg-applied-export (elm)
  (let* ((default-directory anything-c-qpatch-directory)
         (abs-export-fname (expand-file-name
                            anything-hg-default-export-fname
                            anything-c-qpatch-directory))
         (export-dir-name (if (file-exists-p abs-export-fname)
                              (with-temp-buffer
                                (insert-file-contents abs-export-fname)
                                (replace-regexp-in-string "\n" "" (buffer-string)))
                              anything-c-qpatch-directory)))
    (xhg-export
     (int-to-string (cadr (assoc elm anything-qapplied-alist)))
     (read-from-minibuffer "Destination: "
                           nil nil nil nil
                           (expand-file-name (if (string-match "^patch-r[0-9]+" elm)
                                                 (match-string 0 elm)
                                                 "Initial-patch")
                                             export-dir-name)))))

(defun anything-hg-applied-export-via-mail (elm)
  (let ((default-directory anything-c-qpatch-directory))
    (xhg-export-via-mail
     (int-to-string (cadr (assoc elm anything-qapplied-alist))))))

(defun anything-hg-applied-apply-all-patchs (elm)
  (let ((default-directory anything-c-qpatch-directory))
    (xhg-qconvert-to-permanent)))

(defun anything-hg-applied-uniquify (elm)
  (let ((default-directory anything-c-qpatch-directory)
        (patch-name (if (string-match "^patch-r[0-9]+" elm)
                        (match-string 0 elm)
                        "Initial-patch")))
    (xhg-qsingle (concat anything-c-qpatch-directory
                         "Single"
                         patch-name
                         "ToTip.patch") patch-name)))

(defun anything-hg-applied-export-single-via-mail (elm)
  (let ((patch-name (if (string-match "^patch-r[0-9]+" elm)
                        (match-string 0 elm)
                        "Initial-patch"))
        (default-directory anything-c-qpatch-directory))
    (xhg-mq-export-via-mail patch-name t)))

(defun anything-hg-applied-qpop (elm)
  (let ((default-directory anything-c-qpatch-directory))
    (xhg-qpop)))

(defun anything-hg-applied-qpop-all (elm)
  (let ((default-directory anything-c-qpatch-directory))
    (xhg-qpop t)))

(defvar anything-c-source-qapplied-patchs
  '((name . "Hg Qapplied Patchs")
    (volatile)
    (init . anything-hg-init-applied)
    (candidates . anything-hg-applied-candidates)
    (persistent-action . anything-hg-applied-persistent-action)
    (action . (("Show Patch" . anything-hg-applied-show-patch)
               ("Hg Qrefresh" . anything-hg-applied-refresh)
               ("Rename Header" . anything-hg-applied-rename-header)
               ("Hg Qnew" . anything-hg-applied-qnew)
               ("Export" . anything-hg-applied-export)
               ("Export via Mail" . anything-hg-applied-export-via-mail)
               ("Apply all patchs" . anything-hg-applied-apply-all-patchs)
               ("Uniquify all patchs from rev" . anything-hg-applied-uniquify)
               ("Export Single Patch via mail"
                . anything-hg-applied-export-single-via-mail)
               ("Hg-Qpop (top of stack)" . anything-hg-applied-qpop)
               ("Hg-Qpop-All" . anything-hg-applied-qpop-all)))))
;; (anything 'anything-c-source-qapplied-patchs)


;;; Unapplied patchs
(defun anything-c-qunapplied-delete (elm)
  (if anything-c-qunapplied-show-headers
      (progn
        (string-match "^patch-r[0-9]+" elm)
        (xhg-qdelete (match-string 0 elm)))
      (xhg-qdelete elm)))

(defun anything-hg-unapplied-init ()
  (condition-case nil
      (setq anything-c-qunpatch-directory
            (xhg-tree-root (expand-file-name
                            (if (eq major-mode 'dired-mode)
                                (dired-current-directory)
                                default-directory))))
    (error nil)))

(defun anything-hg-unapplied-candidates ()
  (condition-case nil
      (let ((unapplied-patchs
             (with-temp-buffer
               (apply #'call-process "hg" nil t nil
                      (if anything-c-qunapplied-show-headers
                          `("qunapplied" "-s" "-R" ,anything-c-qunpatch-directory)
                          `("qunapplied" "-R" ,anything-c-qunpatch-directory)))
               (buffer-string))))
        (setq unapplied-patchs (split-string unapplied-patchs "\n"))
        (unless (or (string-match "abort:" (car unapplied-patchs))
                    (zerop (length unapplied-patchs)))
          unapplied-patchs))
    (error nil)))

(defun anything-hg-unapplied-persistent-action (elm)
  (let ((default-directory anything-c-qpatch-directory))
    (xhg-qpush)
    (anything-delete-current-selection)))

(defun anything-hg-unapplied-push (elm)
  (let ((default-directory anything-c-qpatch-directory))
    (xhg-qpush)))

(defun anything-hg-unapplied-push-all (elm)
  (let ((default-directory anything-c-qpatch-directory))
    (xhg-qpush t)))

(defun anything-hg-unapplied-delete (elm)
  (let ((default-directory anything-c-qpatch-directory))
    (dolist (i (anything-marked-candidates))
      (anything-c-qunapplied-delete i))))

(defvar anything-c-qunapplied-show-headers nil)
(defvar anything-c-source-qunapplied-patchs
  '((name . "Hg Qunapplied Patchs")
    (volatile)
    (init . anything-hg-unapplied-init)
    (candidates . anything-hg-unapplied-candidates)
    (persistent-action . anything-hg-unapplied-persistent-action)
    (action . (("hg-qpush" . anything-hg-unapplied-push)
               ("hg-qpush-all" . anything-hg-unapplied-push-all)
               ("hg-qdelete patch(s)" . anything-hg-unapplied-delete)))))
;; (anything 'anything-c-source-qunapplied-patchs)

(provide 'anything-mercurial)


;;; anything-mercurial.el ends here

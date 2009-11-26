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

;; Sources
(defvar anything-c-source-qapplied-patchs
  '((name . "Hg Qapplied Patchs")
    (volatile)
    (init . (lambda ()
              (condition-case nil
                  (setq anything-c-qpatch-directory
                        (xhg-tree-root (expand-file-name default-directory)))
                (error nil))))
    (candidates . (lambda ()
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
                          (setq top (string-to-int top))
                          (setq applied-patchs (remove "" (split-string applied-patchs "\n")))
                          (setq anything-qapplied-alist
                                (loop for i in (reverse applied-patchs)
                                     collect (list i top)
                                     and do
                                     (incf top -1)))
                          (unless (or (string-match "abort:" (car applied-patchs))
                                      (zerop (length applied-patchs)))
                            (setq applied-patchs (reverse applied-patchs))))
                      (error nil))))
    (persistent-action . (lambda (elm)
                           (xhg-qpop)
                           (anything-delete-current-selection)))
    (action . (("Show Patch" . (lambda (elm)
                                 (xhg-log (cadr (assoc elm anything-qapplied-alist))
                                          nil
                                          t)))
               ("Hg Qrefresh" . (lambda (elm)
                                  (xhg-qrefresh)))
               ("Rename Header" . (lambda (elm)
                                    (xhg-qrefresh-header)
                                    (save-window-excursion
                                      (when (get-buffer "*xhg-log*")
                                        (kill-buffer "*xhg-log*"))
                                      (xhg-log
                                       (cadr (assoc elm anything-qapplied-alist)) nil t))
                                    (save-excursion
                                      (display-buffer "*xhg-log*"))))
               ("Hg Qnew" . (lambda (elm)
                              (xhg-qnew (xhg-qnew-name-patch)
                                        "New patch")))
               ("Export" . (lambda (elm)
                             (let* ((abs-export-fname (expand-file-name
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
                                                                              export-dir-name))))))
               ("Export via Mail" . (lambda (elm)
                                      (xhg-export-via-mail
                                       (int-to-string (cadr (assoc elm anything-qapplied-alist))))))
               ("Apply all patchs" . (lambda (elm)
                                       (xhg-qconvert-to-permanent)))
               ("Uniquify all patchs from rev" . (lambda (elm)
                                          (let ((patch-name (if (string-match "^patch-r[0-9]+" elm)
                                                                (match-string 0 elm)
                                                                "Initial-patch")))
                                            (xhg-qsingle (concat default-directory
                                                                 "Single"
                                                                 patch-name
                                                                 "ToTip.patch") patch-name))))
               ("Export Single Patch via mail"
                . (lambda (elm)
                    (let ((patch-name (if (string-match "^patch-r[0-9]+" elm)
                                          (match-string 0 elm)
                                          "Initial-patch")))
                      (xhg-mq-export-via-mail patch-name t))))
               ("Hg-Qpop (top of stack)" . (lambda (elm)
                                             (xhg-qpop)))
               ("Hg-Qpop-All" . (lambda (elm)
                                  (xhg-qpop t)))))))


;; (anything 'anything-c-source-qapplied-patchs)

(defun anything-c-qunapplied-delete (elm)
  (if anything-c-qunapplied-show-headers
      (progn
        (string-match "^patch-r[0-9]+" elm)
        (xhg-qdelete (match-string 0 elm)))
      (xhg-qdelete elm)))

(defvar anything-c-qunapplied-show-headers nil)
(defvar anything-c-source-qunapplied-patchs
  '((name . "Hg Qunapplied Patchs")
    (volatile)
    (init . (lambda ()
              (condition-case nil
                  (setq anything-c-qunpatch-directory
                        (xhg-tree-root (expand-file-name default-directory)))
                (error nil))))
    (candidates . (lambda ()
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
                      (error nil))))
    (persistent-action . (lambda (elm)
                           (xhg-qpush)
                           (anything-delete-current-selection)))
    (action . (("hg-qpush" . (lambda (elm)
                               (xhg-qpush)))
               ("hg-qpush-all" . (lambda (elm)
                                   (xhg-qpush t)))
               ("hg-qdelete" . anything-c-qunapplied-delete)
               ("hg-qdelete all marked" . (lambda (elm)
                                            (dolist (i anything-c-marked-candidate-list)
                                              (anything-c-qunapplied-delete i))))))))

;; (anything 'anything-c-source-qunapplied-patchs)

(provide 'anything-mercurial)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything-mercurial.el ends here

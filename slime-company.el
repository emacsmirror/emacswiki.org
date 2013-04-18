;;; slime-company.el --- slime completion backend for company mode
;;
;; Copyright (C) 2009-2013  Ole Arndt
;;
;; Author: Ole Arndt <ole@sugarshark.com>
;; Keywords: convenience, lisp, abbrev
;; Version: 0.5
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This is a backend implementation for the completion package
;; company-mode by Nikolaj Schumacher. More info about this package
;; is available at http://nschum.de/src/emacs/company-mode
;; Company-mode is also available at the ELPA http://tromey.com/elpa
;;
;;; Installation:
;;
;;  Put this file somewhere into your load-path
;;  (or just into slime-path/contribs) and then call
;;
;;   (slime-setup '(slime-company))
;;
;; I also have the following, IMO more convenient key bindings for
;; company mode in my .emacs:
;;
;;   (define-key company-active-map (kbd "\C-n") 'company-select-next)
;;   (define-key company-active-map (kbd "\C-p") 'company-select-previous)
;;   (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
;;   (define-key company-active-map (kbd "\C-v") 'company-show-location)
;;   (define-key company-active-map (kbd "<tab>") 'company-complete)
;;   (define-key company-active-map (kbd "\C-g") '(lambda ()
;;                                                  (interactive)
;;                                                  (company-abort)))
;;; Code:

(require 'company)

(define-slime-contrib slime-company
  "Interaction between slime and the company completion mode."
  (:license "GPL")
  (:authors "Ole Arndt <anwyn@sugarshark.com>")
  (:slime-dependencies slime-autodoc)
  (:swank-dependencies swank-arglists)
  (:on-load
   (dolist (h '(slime-mode-hook slime-repl-mode-hook sldb-mode-hook))
     (add-hook h 'slime-company-maybe-enable)))
  (:on-unload
   (dolist (h '(slime-mode-hook slime-repl-mode-hook sldb-mode-hook))
     (remove-hook h 'slime-company-maybe-enable))
   (slime-company-disable)))

(defsubst slime-company-active-p ()
  "Test if the slime-company backend should be active in the current buffer."
  (derived-mode-p 'lisp-mode 'clojure-mode 'slime-repl-mode))

(defun slime-company-maybe-enable ()
  (when (slime-company-active-p)
    (company-mode 1)
    (add-hook 'company-completion-finished-hook 'slime-company-echo-arglist)
    (add-to-list 'company-backends 'slime-company-backend)))

(defun slime-company-disable ()
  (setq company-backends (remove 'slime-company-backend company-backends))
  (remove-hook 'company-completion-finished-hook 'slime-company-echo-arglist))

(defun slime-company-echo-arglist (_)
  (when (slime-company-active-p)
    (slime-echo-arglist)))

(defun slime-company-backend (command &optional arg &rest ignored)
  "Company mode backend for slime."
  (case command
    ('prefix
     (if (slime-company-active-p)
         (company-grab-symbol)))
    ('candidates
     (first (slime-simple-completions (substring-no-properties arg))))
    ('meta
     (let ((arglist (slime-eval `(swank:operator-arglist ,arg ,(slime-current-package)))))
       (if arglist
           (slime-fontify-string arglist)
         :not-available)))
    ('doc-buffer
     (let ((doc (slime-eval `(swank:describe-symbol ,arg))))
       (with-current-buffer (company-doc-buffer)
         (insert doc)
         (goto-char (point-min))
         (current-buffer))))
    ('location
     (let ((source-buffer (current-buffer)))
       (save-window-excursion
         (slime-edit-definition arg)
         (let ((buffer (if (eq source-buffer (current-buffer))
                           slime-xref-last-buffer
                         (current-buffer))))
           (when (buffer-live-p buffer)
             (cons buffer (with-current-buffer buffer
                            (point))))))))
    ('sorted nil)))

(provide 'slime-company)

;;; slime-company.el ends here

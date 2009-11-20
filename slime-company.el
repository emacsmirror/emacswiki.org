;;; slime-company.el --- slime completion backend for company mode
;;
;; Copyright (C) 2009  Ole Arndt
;;
;; Author: Ole Arndt <ole@sugarshark.com>
;; Keywords: convenience, lisp, abbrev
;; Version: 0.4
;;
;; This program is free software; you can redistribute it and/or modify
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
;; The backend is installed into the list of company-backends, but
;; you have to enable company-mode manually (or in lisp-mode-hook).
;;
;;; Installation:
;;
;; Add this to your .emacs:
;;
;;   (add-to-list 'load-path "<directory-of-this-file>")
;;   (add-hook 'slime-load-hook (lambda () (require 'slime-company)))
;;
;; or use:
;;
;;   (slime-setup 'slime-fancy 'slime-company)
;;
;; For the completion to work, you need to enable company mode in
;; slime and set the appropriate company backends. This can be done
;; via a hook function:
;;
;;   (add-hook 'slime-mode-hook
;;             (lambda ()
;;               (company-mode 1)
;;               (set (make-local-variable 'company-backends)
;;                    '(slime-company-backend))))
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

(defun slime-company-backend (command &optional arg &rest ignored)
  (case command
    ('prefix
     (if (derived-mode-p 'lisp-mode 'clojure-mode 'slime-repl-mode)
         (company-grab-lisp-symbol)))
    ('candidates
     (first (slime-simple-completions (substring-no-properties arg))))
    ('meta
     (let ((arglist (slime-retrieve-arglist arg)))
       (when arglist
         (slime-fontify-string arglist))))
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

(add-to-list 'company-backends 'slime-company-backend)

(provide 'slime-company)

;;; slime-company.el ends here

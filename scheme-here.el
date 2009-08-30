;;; scheme-here.el --- cmuscheme extension for multiple inferior processes
;;
;; Copyright (C) 2007 Dimitris Vyzovitis <vyzo@media.mit.edu>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;;; Commentary:
;; This is an extension of cmuscheme for multiple buffer-local
;; inferior scheme processes.
;;
;;; Change Log:
;; Dec 08 2008: updated headers, provide 'scheme-here
;; Mar 12 2007: initial version
;;
;;; Code:

(require 'comint)
(require 'scheme)
(require 'cmuscheme)

(defun run-scheme-here ()
  "Run a new scheme process at the directory of the current buffer.
   If a process is already running, switch to its buffer."
  (interactive)
  (let* ((proc (format "scheme: %s" default-directory))
         (buf (format "*%s*" proc)))
    (unless (comint-check-proc buf)
      (let ((cmd (split-string scheme-program-name)))
        (set-buffer 
         (apply 'make-comint-in-buffer proc buf (car cmd) nil (cdr cmd)))
        (inferior-scheme-mode)))
    (pop-to-buffer buf)))

(defun scheme-here-buffer ()
  (format "*scheme: %s*" default-directory))

(defun scheme-here-proc ()
  (let ((proc (get-buffer-process (if (eq major-mode 'inferior-scheme-mode) 
                                      (current-buffer)
                                    (scheme-here-buffer)))))
    (or proc (error "No scheme process here"))))

(defun switch-to-scheme-here ()
  "Switch to the buffer-local scheme process."
  (interactive)
  (let ((buf (scheme-here-buffer)))
    (if (get-buffer buf)
        (pop-to-buffer buf)
      (error "No scheme process here"))
    (push-mark)))

(defun scheme-here-send-region (start end)
  "Send the current region to the buffer-local scheme process."
  (interactive "r")
  (let ((proc (scheme-here-proc)))
    (comint-send-region proc start end)
    (comint-send-string proc "\n")))

(defun scheme-here-send-sexp ()
  "Send the last sexp to the buffer-local scheme process."
  (interactive)
  (scheme-here-send-region (save-excursion (backward-sexp) (point)) (point)))

(defun scheme-here-send-def ()
  "Send the current definition to the buffer-local scheme process."
  (interactive)
  (save-excursion 
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (scheme-here-send-region (point) end))))

(defun scheme-here-send-region/switch (start end)
  "Send the current region to the buffer-local scheme process.
   Then switch to its buffer."
  (interactive "r")
  (scheme-here-send-region start end)
  (switch-to-scheme-here))

(defun scheme-here-send-sexp/switch ()
  "Send the last sexp to the buffer-local scheme process.
   Then switch to its buffer."
  (interactive)
  (scheme-here-send-sexp)
  (switch-to-scheme-here))

(defun scheme-here-send-def/switch ()
  "Send the current definition to the buffer-local scheme process.
   Then switch to its buffer."
  (interactive)
  (scheme-here-send-def)
  (switch-to-scheme-here))

(defun scheme-here-hook ()
  (define-key scheme-mode-map "\C-x\M-se" 'scheme-here-send-sexp)
  (define-key scheme-mode-map "\C-x\M-sr" 'scheme-here-send-region)
  (define-key scheme-mode-map "\C-x\M-sd" 'scheme-here-send-def)
  (define-key scheme-mode-map "\C-x\M-s\M-e" 'scheme-here-send-sexp/switch)
  (define-key scheme-mode-map "\C-x\M-s\M-r" 'scheme-here-send-region/switch)
  (define-key scheme-mode-map "\C-x\M-s\M-d" 'scheme-here-send-def/switch))

(add-hook 'scheme-mode-hook 'scheme-here-hook)

(provide 'scheme-here)

;;; scheme-here.el ends here

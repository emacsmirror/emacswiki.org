;;; protbuf.el --- protect buffers from accidental killing

;; Copyright (C) 1994, 1999 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: extensions
;; Status: Works with emacs 19.23 or later.
;; Created: 1994-06-21

;; $Id: protbuf.el,v 1.7 2000/08/21 10:45:38 friedman Exp $

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
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package allows you to make it harder to kill buffers accidentally,
;; e.g. by being too trigger happy selecting items in the buffer menu.
;; protect-process-buffer-from-kill-mode is perhaps the more useful of the
;; two, making it harder to accidentally kill shell buffers without
;; terminating the process in them first.

;;; Code:

(defvar protect-buffer-verbose t
  "*If non-nil, print a message when attempting to kill a protected buffer.")

(defvar protect-buffer-bury-p t
  "*If non-nil, bury buffer when attempting to kill it.
This only has an effect if the buffer to be killed is the one
visible in the selected window.")

 
;;;###autoload
(defvar protect-buffer-from-kill-mode nil
  "*If non-`nil', then prevent buffer from being accidentally killed.
This variable is local to all buffers.")
(progn
  (make-variable-buffer-local 'protect-buffer-from-kill-mode)
  (put 'protect-buffer-from-kill-mode 'permanent-local t)
  (or (assq 'protect-buffer-from-kill-mode minor-mode-alist)
      (setq minor-mode-alist (cons '(protect-buffer-from-kill-mode " prot")
                                   minor-mode-alist))))

;;;###autoload
(defvar protect-process-buffer-from-kill-mode nil
  "*If non-`nil', then protect buffer with live process from being killed.
This variable is local to all buffers.")
(progn
  (make-variable-buffer-local 'protect-process-buffer-from-kill-mode)
  (put 'protect-process-buffer-from-kill-mode 'permanent-local t)
  (or (assq 'protect-process-buffer-from-kill-mode minor-mode-alist)
      (setq minor-mode-alist
            (cons '(protect-process-buffer-from-kill-mode " protp")
                  minor-mode-alist))))

;;;###autoload
(defvar protect-process-buffer-from-kill-preserve-function nil
  "*Function to run to determine whether to kill a process buffer.
If function returns non-nil, buffer is preserved.  Otherwise, the buffer
may be killed.

If this variable is undefined, default action is to test whether a process
object is using this buffer as a process buffer.

This variable is buffer-local when set.")
(make-variable-buffer-local 'protect-process-buffer-from-kill-preserve-function)
(put 'protect-process-buffer-from-kill-preserve-function 'permanent-local t)


 
;;;###autoload
(defun protect-buffer-from-kill-mode (&optional prefix buffer)
  "Protect buffer from being killed.
To remove this protection, call this command with a negative prefix argument."
  (interactive "P")
  (or buffer (setq buffer (current-buffer)))
  (save-excursion
    ;; Each cond does its own set-buffer *after* comparing prefix just in
    ;; case there's a buffer-local variable `prefix' to screw up the works.
    (cond
     ((null prefix)
      (set-buffer buffer)
      (setq protect-buffer-from-kill-mode
            (not protect-buffer-from-kill-mode)))
     ((>= prefix 0)
      (set-buffer buffer)
      (setq protect-buffer-from-kill-mode t))
     (t
      (set-buffer buffer)
      (setq protect-buffer-from-kill-mode nil)))
    ;; This is always done because kill-buffer-query-functions might have
    ;; been buffer-local when this package was initially loaded, leaving
    ;; the global value unchanged.
    (add-hook 'kill-buffer-query-functions 'protect-buffer-from-kill)))

;; This function is listed in kill-buffer-query-functions; it should return
;; nil if the buffer should not be killed, t otherwise.
(defun protect-buffer-from-kill ()
  (cond
   (protect-buffer-from-kill-mode
    (and protect-buffer-verbose
         (message "Buffer \"%s\" is protected from being killed."
                  (buffer-name)))
    (and protect-buffer-bury-p
         (eq (current-buffer)
             (window-buffer (selected-window)))
         (bury-buffer))
    nil)
   (t)))

 
;;;###autoload
(defun protect-process-buffer-from-kill-mode (&optional prefix buffer)
  "Protect buffer from being killed as long as it has an active process.
To remove this protection, call this command with a negative prefix argument."
  (interactive "P")
  (or buffer (setq buffer (current-buffer)))
  (save-excursion
    ;; Each cond does its own set-buffer *after* comparing prefix just in
    ;; case there's a buffer-local variable `prefix' to screw up the works.
    (cond
     ((null prefix)
      (set-buffer buffer)
      (setq protect-process-buffer-from-kill-mode
            (not protect-process-buffer-from-kill-mode)))
     ((>= prefix 0)
      (set-buffer buffer)
      (setq protect-process-buffer-from-kill-mode t))
     (t
      (set-buffer buffer)
      (setq protect-process-buffer-from-kill-mode nil)))
    ;; This is always done because kill-buffer-query-functions might have
    ;; been buffer-local when this package was initially loaded, leaving
    ;; the global value unchanged.
    (add-hook 'kill-buffer-query-functions 'protect-process-buffer-from-kill)))

;; This function is listed in kill-buffer-query-functions; it should return
;; nil if the buffer should be protected, t if buffer should be killed.
(defun protect-process-buffer-from-kill ()
  (cond
   ((not protect-process-buffer-from-kill-mode) t)
   ((or (and (boundp 'protect-process-buffer-from-kill-preserve-function)
             protect-process-buffer-from-kill-preserve-function
             (funcall protect-process-buffer-from-kill-preserve-function))
        (get-buffer-process (current-buffer)))
    (and protect-buffer-verbose
         (message "Buffer \"%s\" has live process; not killing."
                  (buffer-name)))
    (and protect-buffer-bury-p
         (eq (current-buffer)
             (window-buffer (selected-window)))
         (bury-buffer))
    nil)
   (t t)))

(add-hook 'kill-buffer-query-functions 'protect-buffer-from-kill)
(add-hook 'kill-buffer-query-functions 'protect-process-buffer-from-kill)

(provide 'protbuf)

;;; protbuf.el ends here

;;; buffer-extension.el --- Some enhanced functions for buffer manipulate.

;; Filename: buffer-extension.el
;; Description: Some enhanced functions for buffer manipulate.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-02-07 21:57:21
;; Version: 0.1
;; Last-Updated: 2009-02-07 21:57:21
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/buffer-extension.el
;; Keywords: buffer
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;;
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
;; Some enhanced functions for buffer manipulate.
;;

;;; Installation:
;;
;; Put buffer-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'buffer-extension)
;;
;; No need more.

;;; Change log:
;;
;; 2009/02/07
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require

(require 'basic-toolkit)

;;; Code:

(defun kill-current-mode-buffers ()
  "Kill all buffers that major mode same with current mode."
  (interactive)
  (kill-special-mode-buffers-internal major-mode))

(defun kill-current-mode-buffers-except-current ()
  "Kill all buffers that major mode same with current mode.
And don't kill current buffer."
  (interactive)
  (kill-special-mode-buffers-internal major-mode t))

(defun kill-special-mode-buffers ()
  "Kill all buffers that major mode that user given."
  (interactive)
  (let (mode-list)
    (dolist (element (buffer-list))
      (set-buffer element)
      (unless (member (symbol-name major-mode) mode-list)
        (add-to-ordered-list 'mode-list (symbol-name major-mode))))
    (kill-special-mode-buffers-internal (intern-soft (completing-read "Mode: " mode-list)))))

(defun kill-special-mode-buffers-internal (mode &optional except-current-buffer)
  "Kill all buffers that major MODE same with special.
If option EXCEPT-CURRENT-BUFFER is `non-nil',
kill all buffers with MODE except current buffer."
  (interactive)
  (let ((current-buf (current-buffer))
        (count 0))
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (when (and (equal major-mode mode)
                 (or (not except-current-buffer)
                     (not (eq current-buf buffer))))
        (incf count)
        (kill-buffer buffer)))
    (message "Killed %s buffer%s" count (if (> count 1) "s" ""))))

(defun kill-all-buffers-except-current ()
  "Kill all buffers except current buffer."
  (interactive)
  (let ((current-buf (current-buffer)))
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (unless (eq current-buf buffer)
        (kill-buffer buffer)))))

(defun kill-other-window-buffer ()
  "Kill the buffer in other window."
  (interactive)
  (other-window +1)
  (kill-this-buffer)
  (other-window -1))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (rename-file name new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

(defun move-buffer-file (dir)
  "Move both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (copy-file filename newname 1)
      (delete-file filename)
      (set-visited-file-name newname)
      (set-buffer-modified-p nil)
      t)))

(defun buffer-order-next-mark (arg)
  "Jump to next mark."
  (interactive "p")
  (when (mark)
    (let* ((p (point))
           (m (mark))
           (n p)
           (count (if (null arg) 1 arg))
           (abscount (abs count))
           (rel
            (funcall
             (if (< 0 count) 'identity 'reverse)
             (sort (cons (cons 0 p)
                         (cons (cons (- m p) m)
                               (if mark-ring
                                   (mapcar (lambda (mrm)
                                             (cons (- mrm p) mrm))
                                           mark-ring)
                                 nil)))
                   (lambda (c d) (< (car c) (car d))))))
           (cur rel))
      (while (and (numberp (caar cur)) (/= (caar cur) 0))
        (setq cur (cdr cur)))
      (while (and (numberp (caadr cur)) (= (caadr cur) 0))
        (setq cur (cdr cur)))
      (while (< 0 abscount)
        (setq cur (cdr cur))
        (when (null cur) (setq cur rel))
        (setq abscount (- abscount 1)))
      (when (number-or-marker-p (cdar cur))
        (goto-char (cdar cur))))))

(defun buffer-order-prev-mark (arg)
  "Jump to previous mark."
  (interactive "p")
  (buffer-order-next-mark
   (or (- arg) -1)))

(defun copy-buffer-file-name-as-kill(choice)
  "Copy the buffer-file-name to the kill-ring"
  (interactive "cCopy Buffer Name (F) Full, (D) Directory, (N) Name")
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (cond ((eq choice ?f)
           (setq new-kill-string name))
          ((eq choice ?d)
           (setq new-kill-string (file-name-directory name)))
          ((eq choice ?n)
           (setq new-kill-string (file-name-nondirectory name)))
          (t (message "Quit")))
    (when new-kill-string
      (message "%s copied" new-kill-string)
      (kill-new new-kill-string))))

(defun try-to-switch-buffer (name)
  "Just switch to buffer when found some buffer named NAME."
  (if (get-buffer name)
      (switch-to-buffer name)
    (message "Haven't found buffer named `%s`." name)))

(provide 'buffer-extension)

;;; buffer-extension.el ends here

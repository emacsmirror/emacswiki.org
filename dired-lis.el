;;; dired-lis.el --- Letter isearch in dired-mode

;; Copyright (C) 2009 ahei

;; Author: ahei <ahei0802@126.com>
;; Keywords: dired letter isearch

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This library dired-lis (dired letter isearch) let you isearch in `dired-mode'.
;; You only need press letter like in TC(Total commander), not need press some
;; keys to active isearch(in dired-isearch, or in dired-aux in emacs23, you
;; need call M-x dired-isearch-forward or M-x dired-isearch-filenames to
;; active isearch, but in dired-lis minor mode, when you type 0..9, or a..z
;; , or A..Z(which you can customize), the isearch mode active automatically,
;; and the letter is insert into isearch input area automatically.
;; If you do not need letter isearch in `dired-mode', you can call C-u M-x dired-lis-isearch-forward-always?,
;; then isearch-mode active always even if you press enter in sub directory
;; until you press C-g or other keys to abort isearch.

;; Letter isearch in dired-mode.  Copy dired-lis.el to your
;; load-path and add to your .emacs:

;; (require 'dired-lis)

;; Then toggle letter isearch with M-x dired-lcs-mode.  To enable
;; letter isearch in all dired-mode buffers, use M-x global-dired-lcs-mode.

;;; History:

;; 25/10/2009 - initial version 1.0.

;;; Code:

(require 'dired-isearch)

(defgroup dired-lis nil "Minor mode for making letter isearch in `dired-mode'.")

(defcustom dired-lis-isearch-command 'dired-isearch-forward
  "Default isearch command."
  :type 'function
  :group 'dired-lis)
(defcustom dired-lis-isearch-command 'dired-isearch-forward
  "Default isearch command."
  :type 'function
  :group 'dired-lis)
(defcustom dired-lis-find-file-command 'diredp-find-file-reuse-dir-buffer
  "Command when press return in `dired-mode'."
  :type 'function
  :group 'dired-lis)
(defcustom dired-lis-isearch-exit-command 'isearch-exit
  "Command when press return in `isearch-mode'."
  :type 'function
  :group 'dired-lis)
(defcustom dired-lis-isearch-up-directory-command 'c-electric-delete
  "Command in function `isearch-mode' when press \\[c-electric-delete]."
  :type 'function
  :group 'dired-lis)

(defcustom dired-lis-mode-hook nil
  "*Hook called when dired-lis minor mode is activated."
  :type 'hook
  :group 'dired-lis)

(defvar dired-lis-mode-map (make-keymap) "Keymap for letter isearch in `dired-mode'.")

(defvar dired-lis-letter-list nil "Letter list which bind to `dired-lis-isearch-command'.")
(defvar dired-lis-last-isearch-command nil "Last isearch command in `dired-mode'.")
(defvar dired-lis-isearch-always nil "ISearch always in `dired-mode'.")

(defun dired-lis-get-letter-list()
  "Get letter list to bind to `isearch-command' in `dired-mode'."
  (let (i)
    (setq i ?0)
    (while (<= i ?9)
      (setq dired-lis-letter-list (append dired-lis-letter-list (list i)))
      (setq i (1+ i)))
    (setq i ?A)
    (while (<= i ?Z)
      (setq dired-lis-letter-list (append dired-lis-letter-list (list i)))
      (setq i (1+ i)))
    (setq i ?a)
    (while (<= i ?z)
      (setq dired-lis-letter-list (append dired-lis-letter-list (list i)))
      (setq i (1+ i)))))
(defun dired-lis-bind-letter()
  "Bind letter to `isearch-command' in `dired-mode'."
  (let ((map dired-lis-mode-map))
    (dolist (i dired-lis-letter-list)
      (define-key map (vector i) dired-lis-isearch-command))))

(dired-lis-get-letter-list)
(dired-lis-bind-letter)

(defun dired-lis-yank-char ()
  "Insert char to isearch input area."
  (let ((letter last-command-event))
    (if (and (equal major-mode 'dired-mode) (memq letter dired-lis-letter-list))
        (isearch-yank-string (char-to-string letter)))))

;;;###autoload
(define-minor-mode dired-lis-mode
  "Toggle letter isearch in `dired-mode'.

  \\{dired-lis-mode-map}
Entry to this mode calls the value of `dired-lis-mode-hook'
if that value is non-nil.  \\<dired-lis-mode-map>"
  :lighter " LIS"
  (unless (equal major-mode 'dired-mode)
    (error "Current major-mode is not dired-mode"))
  (if dired-lis-mode
      (setq hook-action 'add-hook)
    (setq hook-action 'remove-hook))
  (funcall hook-action 'isearch-mode-hook 'dired-lis-yank-char)
  (dired-lis-bind-letter)
  (if dired-lis-mode
      (run-hooks 'dired-lis-mode-hook)))

(defun dired-lis-on ()
  "Turn on function `dired-lis-mode'."
  (if (equal major-mode 'dired-mode)
      (dired-lis-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-dired-lis-mode dired-lis-mode dired-lis-on)

(defmacro dired-lis-def-isearch-command (fun-name isearch-command search-always)
  "Make definition dired-lis-isearch command."
  `(defun ,fun-name ()
     (interactive)
     (setq dired-lis-last-isearch-command ,isearch-command)
     (setq dired-lis-isearch-always ,search-always)
     (call-interactively ,isearch-command)))

(defmacro dired-lis-def-isearch-command-with-arg (fun-name isearch-command)
  "Make definition dired-lis-isearch command."
  `(defun ,fun-name (&optional search-always)
     (interactive "P")
     (setq dired-lis-last-isearch-command ,isearch-command)
     (setq dired-lis-isearch-always search-always)
     (call-interactively ,isearch-command)))

(dired-lis-def-isearch-command-with-arg dired-lis-isearch-forward-always? 'dired-isearch-forward)
(dired-lis-def-isearch-command-with-arg dired-lis-isearch-backward-always? 'dired-isearch-backward)
(dired-lis-def-isearch-command-with-arg dired-lis-isearch-forward-regexp-always? 'dired-isearch-forward-regexp)
(dired-lis-def-isearch-command-with-arg dired-lis-isearch-backward-regexp-always? 'dired-isearch-backward-regexp)
(dired-lis-def-isearch-command dired-lis-isearch-forward-temp 'dired-isearch-forward nil)
(dired-lis-def-isearch-command dired-lis-isearch-forward-always 'dired-isearch-forward t)
(dired-lis-def-isearch-command dired-lis-isearch-backward-temp 'dired-isearch-backward nil)
(dired-lis-def-isearch-command dired-lis-isearch-backward-always 'dired-isearch-backward t)

(defun dired-lis-isearch-exit-or-find-file ()
  "According to current `major-mode' and `dired-lis-isearch-always',
execute `isearch-abort' or `diredp-find-file-reuse-dir-buffer'."
  (interactive)
  (if (not (equal major-mode 'dired-mode))
      (call-interactively dired-lis-isearch-exit-command)
    (let ((file (dired-get-file-for-visit)))
      ;; First open file or directory
      (if dired-lis-isearch-always
          (call-interactively dired-lis-find-file-command)
        (ignore-errors
          (call-interactively dired-lis-find-file-command)))
      ;; if search-always and is directory
      ;; continue isearch
      (if (and dired-lis-isearch-always (file-directory-p file))
          (progn
            ;; TODO: when add following statement commented,
            ;; this command will not work well, why?
            ;; (isearch-abort)
            (call-interactively dired-lis-last-isearch-command))
        (isearch-abort)))))

(defun dired-lis-isearch-done-del-or-up-directory ()
  "According to current `major-mode' and `dired-lis-isearch-always',
execute `isearch-done' or `dired-up-directory-same-buffer'."
  (interactive)
  (if (equal major-mode 'dired-mode)
      (progn
        (dired-up-directory-same-buffer)
        (if dired-lis-isearch-always
            (call-interactively dired-lis-last-isearch-command)
          (isearch-done)))
    (isearch-done)
    (call-interactively dired-lis-isearch-up-directory-command)))

(define-key isearch-mode-map (kbd "RET") 'dired-lis-isearch-exit-or-find-file)
;; (define-key isearch-mode-map (kbd "C-h") 'dired-lis-isearch-done-del-or-up-directory)

(provide 'dired-lis)

(provide 'dired-lis)

;;; dired-lis.el ends here

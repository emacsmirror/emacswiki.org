;;; etags-u.el - (auto)updating TAGS file using etags
;;
;; From anonymous for anonymous
;;
;; This program is free software. It comes without any warranty, to
;; the extent permitted by applicable law. You can redistribute it
;; and/or modify it under the terms of the Do What The Fuck You Want
;; To Public License, Version 2, as published by Sam Hocevar. See
;; http://sam.zoy.org/wtfpl/COPYING for more details.
;;
;; Copy of the license text:
;;
;;             DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;                     Version 2, December 2004
;;
;;  Copyright (C) 2004 Sam Hocevar <sam@hocevar.net>
;;
;;  Everyone is permitted to copy and distribute verbatim or modified
;;  copies of this license document, and changing it is allowed as long
;;  as the name is changed.
;;
;;             DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
;;
;;   0. You just DO WHAT THE FUCK YOU WANT TO.
;;
;; Commentary:
;;
;; This package provides `etags-u-update-tags-file' command. It uses
;; `tags-file-name' variable. To change it, use M-x `visit-tags-table'
;; command.
;; It also provides minor mode `etags-u-mode'. In this minor mode, C-x
;; C-s run `etags-u-update-tags-file' automatically
;;
;; Usage:
;;
;; Add this one to your .emacs file:
;; (require 'etags-u)
;;
;; M-x visit-tags-table
;; M-x etags-u-mode
;; <at this point file is ALREADY added to TAGS file>
;; <make changes>
;; C-x C-s
;; <changes updated in TAGS file>
;;
;; You can also add (etags-u-mode) to major mode hooks in your .emacs
;; file.
;; Example:
;; (add-hook 'c-mode-hook
;;   '(lambda()
;;      (etags-u-mode t)))
;;
;; You can automatically enable etags-u-mode per file, using 
;; Local Variables in a file
;; Example:
;; ;; Local variables:
;; ;; eval: (setq etags-u-mode t)
;; ;; End:
;;
;; References:
;; http://www.emacswiki.org/emacs/etags-u.el -- self-reference
;; http://stackoverflow.com/questions/548414/how-to-programmatically-create-update-a-tags-file-with-emacs -- another solutions
;; http://www.emacswiki.org/emacs/BuildTags -- another solutions
;;
;;; Code:

(defconst etags-u-revative-names t)
(defun etags-u-update-tags-file()
  "Updates current file in current TAGS file. Current tags file is `tags-file-name'.
If file was not in TAGS file, it is created."
  (interactive)
  (flet ((message (&rest args) args)) ;; do not send junk to minibuffer
    (etags-u-remove-from-tags-file)
    (etags-u-append-to-tags-file))
  nil)

(defun etags-u-append-to-tags-file()
  "Appends to current file to current TAGS file. Current tags file is `tags-file-name'.
Warning, it's not update function. If file was already tagged, it will create dublicate of current file."
  (unless (and (stringp tags-file-name) (file-exists-p tags-file-name))
    (error "`tags-file-name' does not point to TAGS file. Use m-x visit-tags-table to set it."))
  (save-excursion
    (let ((file (buffer-file-name)))
      (unless file (error "No buffer-file!"))
      (set-buffer (find-file-noselect tags-file-name t))
      (find-buffer-visiting tags-file-name)
      (flet ((y-or-n-p (&rest args) t) (yes-or-no-p (&rest args) t))
        (shell-command (format "etags -a %s" (if etags-u-revative-names (file-relative-name file) file)) (get-buffer-create " *etags-u*") (get-buffer-create " *etags-u*")))))
  nil)

(defun etags-u-remove-from-tags-file (&optional file)
  "Removes current file from current TAGS file. Current tags file is `tags-file-name'."
  (interactive)
  (unless (and (stringp tags-file-name) (file-exists-p tags-file-name))
    (error "`tags-file-name' does not point to TAGS file. Use m-x visit-tags-table to set it."))
  (save-excursion
    (unless file (setq file (buffer-file-name)))
    (unless file (error "No buffer-file!"))
    (assert (file-exists-p file) "File not exist: %S" file)
    (set-buffer (find-file-noselect tags-file-name))
    (goto-char (point-min))
    (while (etags-u-test-and-remove file))
    (save-buffer))
  nil)

(defun etags-u-test-and-remove (file)
  "Started in TAGS file; T if we need to quit."
  (let (start filename-in-tags-file)
    (if
      (search-forward " \n" nil t)
      (let ((beg (point)))
        (setq start (match-beginning 0))
        (search-forward ",")
        (goto-char (match-beginning 0))
        (setq filename-in-tags-file
          (buffer-substring-no-properties beg (point)))
        (when (file-equal-p
                file
                filename-in-tags-file)
          (goto-char
            (if (search-forward " " nil t)
              (match-beginning 0)
              (point-max)))
          (delete-region start (point)))
        t)
      nil)))

(defvar etags-u-map (make-sparse-keymap))
(define-minor-mode etags-u-mode
  "docstring"
  :init-value nil
  :lighter " etags-U"
  :keymap etags-u-map
  (progn
    (if etags-u-mode (etags-u-update-tags-file))))

(defadvice save-buffer (after etags-u)
  "Autoupdate TAGS file"
  (if etags-u-mode (etags-u-update-tags-file)))
(ad-activate 'save-buffer)

(defadvice delete-file (before etags-u)
  "Delete entries from TAGS file. Cuz dired works with `delete-file' too, works nice with dired!"
  (etags-u-remove-from-tags-file filename))
(ad-activate 'delete-file)

(provide 'etags-u)

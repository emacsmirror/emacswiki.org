;;; grep-buffers.el --- grep through buffers (a la 'moccur')

;; Copyright (C) 2007-2009  Scott Frazer

;; Author: Scott Frazer <frazer.scott@gmail.com>
;; Maintainer: Scott Frazer <frazer.scott@gmail.com>
;; Created: 01 Jan 2007
;; Version: 2.2
;; Keywords: grep buffers

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
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

;; This code lets you grep through all loaded buffers that have a file
;; associated with them.  It's similar to 'moccur' and it's many variants, but
;; uses the standard compilation-mode interface, i.e. next-error,
;; previous-error, etc. all work.

(defvar grep-buffers-buffer-name "*grep-buffers*"
  "grep-buffers buffer name.")

(defvar grep-buffers-name-regexps-to-ignore (list "TAGS$")
  "*Name regexps to ignore")

(defvar grep-buffers-regexp-history nil
  "Regexp history for grep-buffers.")

(defvar grep-buffers-match-index -1
  "Current match index.")

;; Grep buffers

(defun grep-buffers ()
  "Grep buffers that have file names associated with them."
  (interactive)
  (let ((buffers (sort (buffer-list)
                       (function (lambda (elt1 elt2)
                                   (string< (downcase (buffer-name elt1))
                                            (downcase (buffer-name elt2)))))))
        (regexp (grep-buffers-symbol-at-point)))
    (setq regexp (read-string (format "grep buffers for [%s]: " regexp)
                              nil 'grep-buffers-regexp-history regexp))
    (add-to-list 'grep-buffers-regexp-history regexp)
    (get-buffer-create grep-buffers-buffer-name)
    (save-excursion
      (set-buffer grep-buffers-buffer-name)
      (erase-buffer)
      (display-buffer grep-buffers-buffer-name)
      (insert (format "grep buffers for '%s' ...\n\n" regexp))
      (mapcar (lambda (x)
                (when (grep-buffers-do-this-one x)
                  (set-buffer x)
                  (save-excursion
                    (save-match-data
                      (goto-char (point-min))
                      (while (re-search-forward regexp nil t)
                        (let ((line (count-lines 1 (point)))
                              (substr (buffer-substring (point-at-bol)
                                                        (point-at-eol))))
                          (save-excursion
                            (set-buffer grep-buffers-buffer-name)
                            (insert (format "%s:%d:%s\n" x line substr))))
                        (goto-char (point-at-eol)))))))
              buffers)
      (set-buffer grep-buffers-buffer-name)
      (goto-char (point-max))
      (insert "\ngrep finished\n")
      (if (< emacs-major-version 22)
          (progn
            (compilation-mode)
            (set (make-local-variable 'compilation-parse-errors-function)
                 'grep-buffers-parse-matches))
        (grep-mode)
        (grep-buffers-parse-matches nil nil)
        (setq grep-buffers-match-index -1)
        (setq overlay-arrow-position nil)
        (setq next-error-function 'grep-buffers-next-match))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (setq buffer-read-only nil)
      (force-mode-line-update))))

(defun grep-buffers-symbol-at-point ()
  (save-excursion
    (buffer-substring (progn (skip-syntax-backward "w_") (point))
                      (progn (skip-syntax-forward "w_") (point)))))

(defun grep-buffers-do-this-one (buf)
  (let ((name (buffer-file-name buf))
        (case-fold-search nil)
        match)
  (when name
    (mapcar
     (lambda (x) (setq match (or match (string-match x name))))
     grep-buffers-name-regexps-to-ignore)
    (not match))))

;; Parse matches

(defun grep-buffers-parse-matches (limit-search find-at-least)
  "Parse the grep buffer for matches.
See variable `compilation-parse-errors-function' for interface."
  (save-excursion
    (set-buffer grep-buffers-buffer-name)
    (goto-char (point-min))
    (setq compilation-error-list nil)
    (while (re-search-forward "^\\(.+?\\):\\([0-9]+?\\):\\(.+?\\)$" nil t)
      (let ((buffer-of-match (match-string 1))
            (line-of-match (string-to-number (match-string 2))))
        (setq compilation-error-list
              (nconc compilation-error-list
                     (list (cons
                            (save-excursion
                              (beginning-of-line)
                              (point-marker))
                            (save-excursion
                              (set-buffer buffer-of-match)
                              (goto-line line-of-match)
                              (beginning-of-line)
                              (point-marker))))))))))

;; Next match (Emacs 22)

(defun grep-buffers-next-match (&optional arg reset)
  (let (match-info)
    (if reset
        (setq grep-buffers-match-index 0)
      (if (= arg 0)
          (setq grep-buffers-match-index (- (count-lines (point-at-bol) (point-min)) 2))
        (setq grep-buffers-match-index (+ grep-buffers-match-index arg)))
      (when (< grep-buffers-match-index 0)
        (setq grep-buffers-match-index 0))
      (when (> grep-buffers-match-index (length compilation-error-list))
        (setq grep-buffers-match-index (length compilation-error-list))))
    (setq match-info (elt compilation-error-list grep-buffers-match-index))
    (pop-to-buffer (marker-buffer (car match-info)))
    (goto-char (marker-position (car match-info)))
    (setq overlay-arrow-position (point-marker))
    (pop-to-buffer (marker-buffer (cdr match-info)))
    (goto-char (marker-position (cdr match-info)))))

(provide 'grep-buffers)

;;; grep-buffers.el ends here

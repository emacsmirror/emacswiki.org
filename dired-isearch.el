;;; dired-isearch.el --- isearch in Dired

;; Copyright (C) 2006 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.3

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
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:

;; Do isearch in Dired but match only at file names.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;           (require 'dired-isearch)

;; Recommended keybindings:

;; (define-key dired-mode-map (kbd "C-s") 'dired-isearch-forward)
;; (define-key dired-mode-map (kbd "C-r") 'dired-isearch-backward)
;; (define-key dired-mode-map (kbd "ESC C-s") 'dired-isearch-forward-regexp)
;; (define-key dired-mode-map (kbd "ESC C-r") 'dired-isearch-backward-regexp)

;;; Code:

;;; User Interfaces

(defun dired-isearch-forward (&optional regexp-p no-recursive-edit)
  "In Dired, run `isearch-forward' but match only at file names."
  (interactive)
  (let ((isearch-search-fun-function 'dired-isearch-search-fun-function))
    (isearch-forward regexp-p no-recursive-edit)))

(defun dired-isearch-backward (&optional regexp-p no-recursive-edit)
  "In Dired, run `isearch-backward' but match only at file names."
  (interactive)
  (let ((isearch-search-fun-function 'dired-isearch-search-fun-function))
    (isearch-backward regexp-p no-recursive-edit)))

(defun dired-isearch-forward-regexp (&optional not-regexp no-recursive-edit)
  "In Dired, run `isearch-forward-regexp' but match only at file names."
  (interactive)
  (let ((isearch-search-fun-function 'dired-isearch-search-fun-function))
    (isearch-forward-regexp not-regexp no-recursive-edit)))

(defun dired-isearch-backward-regexp (&optional not-regexp no-recursive-edit)
  "In Dired, run `isearch-backward-regexp' but match only at file names."
  (interactive)
  (let ((isearch-search-fun-function 'dired-isearch-search-fun-function))
    (isearch-backward-regexp not-regexp no-recursive-edit)))

 
;;; Low Level Functions

(defun dired-isearch-search-fun-function ()
  "Return the isearch function in Dired."
  (cond
       (isearch-word
        (if isearch-forward 'dired-word-search-forward 'dired-word-search-backward))
       (isearch-regexp
        (if isearch-forward 'dired-re-search-forward 'dired-re-search-backward))
       (t
        (if isearch-forward 'dired-search-forward 'dired-search-backward))))

(defun dired-search-forward (string &optional bound noerror count)
  "In Dired, run `search-forward' but match only at file names."
  (let ((point (search-forward string bound noerror count)))
    (catch 'return
      (while point
        (when (get-text-property (1- point) 'help-echo)
          (throw 'return point))
        (setq point (search-forward string bound noerror count))))))

(defun dired-search-backward (string &optional bound noerror count)
  "In Dired, run `search-backward' but match only at file names."
  (let ((point (search-backward string bound noerror count)))
    (catch 'return
      (while point
        (when (get-text-property point 'help-echo)
          (throw 'return point))
        (setq point (search-backward string bound noerror count))))))

(defun dired-re-search-forward (regexp &optional bound noerror count)
  "In Dired, run `re-search-forward' but match only at file names."
  (let ((point (re-search-forward regexp bound noerror count)))
    (catch 'return
      (while point
        (when (get-text-property (1- point) 'help-echo)
          (throw 'return point))
        (setq point (re-search-forward regexp bound noerror count))))))

(defun dired-re-search-backward (regexp &optional bound noerror count)
  "In Dired, run `re-search-backward' but match only at file names."
  (let ((point (re-search-backward regexp bound noerror count)))
    (catch 'return
      (while point
        (when (get-text-property point 'help-echo)
          (throw 'return point))
        (setq point (re-search-backward regexp bound noerror count))))))

(defun dired-word-search-forward (string &optional bound noerror count)
  "In Dired, run `word-search-forward' but match only at file names."
  (let ((point (word-search-forward string bound noerror count)))
    (catch 'return
      (while point
        (when (get-text-property (1- point) 'help-echo)
          (throw 'return point))
        (setq point (word-search-forward string bound noerror count))))))

(defun dired-word-search-backward (string &optional bound noerror count)
  "In Dired, run `word-search-backward' but match only at file names."
  (let ((point (word-search-backward string bound noerror count)))
    (catch 'return
      (while point
        (when (get-text-property point 'help-echo)
          (throw 'return point))
        (setq point (word-search-backward string bound noerror count))))))

(provide 'dired-isearch)

;;; dired-isearch.el ends here

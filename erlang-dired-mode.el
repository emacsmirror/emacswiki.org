;;; erlang-dired.el --- erlang dired mode    -*- coding:utf-8 -*-

;; Description: erlang dired mode
;; Created: 2011-12-20 22:41
;; Last Updated: Joseph 2011-12-20 23:11:15 星期二
;; Author: Joseph(纪秀峰)  jixiuf@gmail.com
;; Maintainer:  Joseph(纪秀峰)  jixiuf@gmail.com
;; Keywords: erlang dired Emakefile
;; URL: http://www.emacswiki.org/emacs/erlang-dired-mode.el
;; X-URL:git://github.com/jixiuf/erlang-dired-mode.git

;; Copyright (C) 2011, 纪秀峰, all rights reserved.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `inferior-erlang-emake'
;;    run make:all(load) in project root of erlang application.
;;  `erlang-dired-emake'
;;    Compile Erlang module in current buffer.
;;  `erlang-dired-mode'
;;    Erlang application development minor mode.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:

(require 'erlang)

(eval-when-compile
  (defvar comment-indent-hook)
  (defvar dabbrev-case-fold-search)
  (defvar tempo-match-finder)
  (defvar compilation-menu-map)
  (defvar next-error-last-buffer))

(eval-when-compile
  (if (or (featurep 'bytecomp)
          (featurep 'byte-compile))
      (progn
        (cond ((string-match "Lucid\\|XEmacs" emacs-version)
               (put 'comment-indent-hook 'byte-obsolete-variable nil)
               ;; Do not warn for unused variables
               ;; when compiling under XEmacs.
               (setq byte-compile-warnings
                     '(free-vars unresolved callargs redefine))))
        (require 'comint)
        (require 'tempo)
        (require 'compile))))

(defun erlang-root ()
  "Look for Emakefile file to find project root of erlang application."
  (locate-dominating-file default-directory "Emakefile"))

(defun inferior-erlang-emake (arg)
  "run make:all(load) in project root of erlang application."
  (interactive "P")
  (save-some-buffers)
  (inferior-erlang-prepare-for-input)
  (let* ((default-dir default-directory)
         end)
    (with-current-buffer inferior-erlang-buffer
      (compilation-forget-errors))

    (inferior-erlang-send-command
     (format "cd(\"%s\")." ( erlang-root)) nil)
    (sit-for 0)
    (inferior-erlang-wait-prompt)

    (inferior-erlang-send-command
     "make:all([load])." nil)
    (sit-for 0)
    (inferior-erlang-wait-prompt)

    (setq end (inferior-erlang-send-command
               (format "cd(\"%s\")." default-dir) nil))
    (sit-for 0)
    (inferior-erlang-wait-prompt)
    (with-current-buffer inferior-erlang-buffer
      (setq compilation-error-list nil)
      (set-marker compilation-parsing-end end))
    (setq compilation-last-buffer inferior-erlang-buffer)))


(defun erlang-dired-emake ()
  "Compile Erlang module in current buffer."
  (interactive)
  (call-interactively 'inferior-erlang-emake))

(defvar erlang-dired-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-z") 'erlang-shell-display)
    (define-key map (kbd "C-c C-l") 'erlang-compile-display)
    (define-key map (kbd "C-c C-k") 'erlang-dired-emake) ;run make:all([load])
    map))

;; (define-key erlang-dired-mode-map (kbd "C-z s") 'erlang-dired-emake)
;;;###autoload
(define-minor-mode erlang-dired-mode
  "Erlang application development minor mode."
  nil
  " ErlDir"
  erlang-dired-mode-map)

;;;###autoload
(defun erlang-dired-mode-fun()
  (when (erlang-root) (erlang-dired-mode t)))

;;;###autoload
(add-hook 'dired-mode-hook 'erlang-dired-mode-fun)

(provide 'erlang-dired-mode)
;;; erlang-dired.el ends here

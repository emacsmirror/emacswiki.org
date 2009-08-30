;;; re-builder+.el --- Re-builder+

;; Filename: re-builder+.el
;; Description: Re-builder+
;; Author: Kenichirou Oyama <k1lowxb@gmail.com>
;; Maintainer: Kenichirou Oyama <k1lowxb@gmail.com>
;; Copyright (C) 2009, Kenichirou Oyama, all rights reserved.
;; Copyright (C) 2009, 101000code/101000LAB, all rights reserved.
;; Created: 2009-04-08
;; Version: 0.0.2
;; URL: http://www.emacswiki.org/emacs/download/re-builder+.el
;; Keywords: regexp, re-builder
;; Compatibility: GNU Emacs 22 ~
;;
;; Features that might be required by this library:
;;
;; `re-builder'
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
;; Extensions to `re-builder.el'.
;;
;; You can match the regular-expression to target region when you use
;; this extension.
;; And it provides some commands.

;; Thanks to elim for advice about symbol-function.

;;; Installation:
;;
;; Put re-builder+.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 're-builder+)
;;
;; No need more.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `re-builder+'
;;    Construct a regexp interactively+.
;;  `reb+-replace-regexp'
;;    re-builder to replace-regexp.
;;  `reb+-query-replace-regexp'
;;    re-builder to query-replace-regexp.
;;

;;; Change log:
;;
;; 2009/04/08
;;   * Kenichirou Oyama:
;;      * Bug fix.
;;
;; 2009/04/08
;;   * Kenichirou Oyama:
;;      * First released.

;;; Todo:
;; Refactor code.

;;; Require
(require 're-builder)

;;; Code:

(defvar reb+-region-beginning nil)
(defvar reb+-region-end nil)
(defvar reb+-enabled nil)

(defun re-builder+ ()
  "Construct a regexp interactively+."
  (interactive)
  (setq reb+-region-beginning (point-min))
  (setq reb+-region-end (point-max))
  (unless (not mark-active)
    (setq reb+-region-beginning (region-beginning))
    (setq reb+-region-end (region-end))
    (setq reb+-enabled t))
  (re-builder))

(defadvice reb-quit (around reb-quit-around activate activate)
  (setq reb+-enabled nil)
  ad-do-it)

(defadvice reb-update-overlays (around reb-update-overlays-around activate)
  (let ((point-min-def (symbol-function 'point-min))
        (point-max-def (symbol-function 'point-max)))
    (if (not reb+-enabled)
        ad-do-it
      (setq point-min-def (symbol-function 'point-min))
      (setq point-max-def (symbol-function 'point-max))
      (defun point-min () reb+-region-beginning)
      (defun point-max () reb+-region-end)
      ad-do-it
      (fset 'point-min point-min-def)
      (fset 'point-max point-max-def))))

(defadvice reb-next-match (around reb-next-match-around activate)
  (let ((point-min-def (symbol-function 'point-min))
        (point-max-def (symbol-function 'point-max)))
    (if (not reb+-enabled)
        ad-do-it
      (setq point-min-def (symbol-function 'point-min))
      (setq point-max-def (symbol-function 'point-max))
      (defun point-min () reb+-region-beginning)
      (defun point-max () reb+-region-end)
      ad-do-it
      (fset 'point-min point-min-def)
      (fset 'point-max point-max-def))))

(defadvice reb-prev-match (around reb-prev-match-around activate)
  (let ((point-min-def (symbol-function 'point-min))
        (point-max-def (symbol-function 'point-max)))
    (if (not reb+-enabled)
        ad-do-it
      (setq point-min-def (symbol-function 'point-min))
      (setq point-max-def (symbol-function 'point-max))
      (defun point-min () reb+-region-beginning)
      (defun point-max () reb+-region-end)
      ad-do-it
      (fset 'point-min point-min-def)
      (fset 'point-max point-max-def))))

(defun reb+-get-regexp ()
  "get current regexp"
  (reb-update-regexp)
  (let ((re (with-output-to-string
              (print (reb-target-binding reb-regexp)))))
    (substring re 2 (- (length re) 2))))

(defun reb+-replace-regexp ()
  "re-builder to replace-regexp."
  (interactive)
  (let* ((regexp (reb+-get-regexp))
         (to-string (read-string (concat "Query " regexp " to: "))))
    (reb-quit)
    (switch-to-buffer reb-target-buffer)
    (replace-regexp regexp to-string nil reb+-region-beginning reb+-region-end)))

(defun reb+-query-replace-regexp ()
  "re-builder to query-replace-regexp."
  (interactive)
  (let* ((regexp (reb+-get-regexp))
         (to-string (read-string (concat "Query " regexp " to: "))))
    (reb-quit)
    (switch-to-buffer reb-target-buffer)
    (query-replace-regexp regexp to-string nil reb+-region-beginning reb+-region-end)))

(define-key reb-mode-map "\M-%" 'reb+-query-replace-regexp)

(load "re-builder.el" t)

(provide 're-builder+)

;;; re-builder+.el ends here

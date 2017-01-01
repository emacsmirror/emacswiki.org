;;; vc-hooks+.el --- Extensions to `vc-hooks.el'.
;;
;; Filename: vc-hooks+.el
;; Description: Extensions to `vc-hooks.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1999-2017, Drew Adams, all rights reserved.
;; Created: Mon Jun 19 10:51:38 2000
;; Version: 20.0
;; Last-Updated: Sun Jan  1 11:55:56 2017 (-0800)
;;           By: dradams
;;     Update #: 94
;; URL: http://www.emacswiki.org/vc-hooks+.el
;; Keywords: version control
;; Compatibility: GNU Emacs 20.x
;;
;; Features that might be required by this library:
;;
;;   `ring', `vc', `vc-hooks'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `vc-hooks.el'.
;;
;; Note: This code is quite old, and is likely obsolete now.  You
;;       might find it useful in some way to mine - or not. ;-)
;;
;; -------------------------------------------------------------------
;;
;;  The following global bindings are made here:
;;
;;  `C-x v ='    `vc-ediff'
;;  `C-x v .     `vc-status-here'
;;  `C-x 4 v .   `vc-status-here-other-window'
;;  `C-x 5 v .   `vc-status-here-other-frame'
;;  `C-x v d     `vc-status-here'
;;  `C-x 4 v d   `vc-status-here-other-window'
;;  `C-x 5 v d   `vc-status-here-other-frame'
;;  `C-x v C-r'  `vc-rename-file'
;;
;;  The following binding is made here for mode `vc-dired-mode':
;;
;;  `C-='        `vc-ediff'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'vc-hooks)
(require 'vc) ;; vc-dired-mode
(autoload 'vc-ediff "vc+" "Compare file versions using `ediff'." t)
(autoload 'vc-status-here "vc+"
  "Show version-control status of files in current directory." t)
(autoload 'vc-status-here-other-window "vc+"
  "Show version-control status of files in current directory." t)
(autoload 'vc-status-here-other-frame "vc+"
  "Show version-control status of files in current directory." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;; Key bindings

;;; `vc-prefix-map' is defined in `vc-hooks.el'.
;;; `vc-dired-mode-map' is defined in `vc.el'.

(define-key vc-prefix-map [?\C-r] 'vc-rename-file)

;; Switch standard `vc-diff' bindings.
(define-key vc-prefix-map [?\C-=] 'vc-diff)
(define-key vc-prefix-map [?=] 'vc-ediff)
;; Do this in `vc+.el', not here:
;; (define-key vc-dired-mode-map [?\"] 'vc-ediff)

(when (fboundp 'menu-bar-ediff-menu)    ; Defined in `ediff-hook.el'.
  (define-key menu-bar-ediff-menu [ediff-revision]
    '("File with Revision..." . vc-ediff)))

(define-key vc-prefix-map "." 'vc-status-here)

(define-key ctl-x-4-map "v." 'vc-status-here-other-window)
(define-key ctl-x-4-map "vd" 'vc-directory-other-window)

(define-key ctl-x-5-map "v." 'vc-status-here-other-frame)
(define-key ctl-x-5-map "vd" 'vc-directory-other-frame)


;;; Menu bar bindings
(define-key-after vc-menu-map [ediff-revision]
  '("Compare Version" . vc-ediff) 'vc-update-change-log)
(put 'vc-ediff 'menu-enable
     '(or vc-mode
          (and (boundp 'vc-dired-mode) vc-dired-mode)
          (eq 'dired-mode major-mode)))

(define-key vc-menu-map [vc-diff] '("Compare Version Using Diff" . vc-diff))
(put 'vc-diff 'menu-enable
     '(or vc-mode
          (and (boundp 'vc-dired-mode) vc-dired-mode)
          (eq 'dired-mode major-mode)))

(define-key-after vc-menu-map [vc-status-here]
  '("Files Here" . vc-status-here) 'separator1)
(define-key vc-menu-map [vc-directory] '("Files Below" . vc-directory))

(put 'vc-annotate 'menu-enable
     '(or (eq (vc-buffer-backend) 'CVS)
          (and (boundp 'vc-dired-mode) vc-dired-mode)
          (eq 'dired-mode major-mode)))
(put 'vc-cancel-version 'menu-enable
     '(or vc-mode
          (and (boundp 'vc-dired-mode) vc-dired-mode)
          (eq 'dired-mode major-mode)))
(put 'vc-insert-headers 'menu-enable
     '(or vc-mode (and (boundp 'vc-dired-mode) vc-dired-mode)))
(put 'vc-print-log 'menu-enable
     '(or vc-mode
          (and (boundp 'vc-dired-mode) vc-dired-mode)
          (eq 'dired-mode major-mode)))
;; Standard Emacs has this, for vc-register:
;; '(and buffer-file-name (not vc-mode))
(put 'vc-register 'menu-enable
     '(not vc-mode)) ; vc-dired-mode is OK (e.g. Unregistered).
(put 'vc-rename-file 'menu-enable
     '(or vc-mode
          (and (boundp 'vc-dired-mode) vc-dired-mode)
          (eq 'dired-mode major-mode)))
(put 'vc-revert-buffer 'menu-enable
     '(or vc-mode
          (and (boundp 'vc-dired-mode) vc-dired-mode)
          (eq 'dired-mode major-mode)))
(put 'vc-toggle-read-only 'menu-enable
     '(or vc-mode (and (boundp 'vc-dired-mode) vc-dired-mode)))
(put 'vc-update-change-log 'menu-enable
     '(or (eq (vc-buffer-backend) 'RCS)
          (and (boundp 'vc-dired-mode) vc-dired-mode)
          (eq 'dired-mode major-mode)))
(put 'vc-version-other-window 'menu-enable
     '(or vc-mode
          (and (boundp 'vc-dired-mode) vc-dired-mode)
          (eq 'dired-mode major-mode)))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'vc-hooks+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vc-hooks+.el ends here

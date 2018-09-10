;;; init-company-mode.el --- Company-mode configuration

;; Filename: init-company-mode.el
;; Description: Company-mode configuration
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-20 09:56:57
;; Version: 1.1
;; Last-Updated: 2018-09-11 01:00:24
;;           By: Andy Stewart
;; URL:
;; Keywords: company-mode
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
;; Company-mode configuration
;;

;;; Installation:
;;
;; Put init-company-mode.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-company-mode)
;;

;;; Change log:
;;
;; 2018/09/11
;;      * Split lsp configuration to `init-lsp.el'.
;;
;; 2018/07/24
;;      * Add command to install python completion backend.
;;
;; 2018/07/23
;;      * Add `company-elisp' backend when load emacs-lisp mode.
;;
;; 2018/07/16
;;      * Don't downcase completion result from dabbrev.
;;
;; 2018/07/12
;;      * Customize dabbrev backend, to make company can completion any words in all buffer like `dabbrev-expand'.
;;      * Default add `company-files' backend.
;;
;; 2018/07/07
;;      * Add `company-css' into `company-backends'.
;;
;; 2018/07/06
;;      * Fix ruby mode load error.
;;      * Fix python mode load error.
;;      * Use `exec-path-from-shell' avoid LSP can't found server bin path.
;;
;; 2018/07/05
;;      * Config company and company-lsp fronted.
;;      * Make company works with posframe.
;;      * Add LSP mode support.
;;      * Add support for python and rubyl
;;
;; 2008/10/20
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

(require 'lazy-set-key)
(require 'company)
(require 'company-posframe)
(require 'company-yasnippet)
(require 'company-dabbrev)
(require 'company-files)
(require 'desktop)

;;; Code:

;; Config for company mode.
(global-company-mode)
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)
(setq company-show-numbers nil)

;; ;; Customize company backends.
(push 'company-files company-backends)

;; Let desktop.el not record the company-posframe-mode
(company-posframe-mode 1)
(push '(company-posframe-mode . nil)
      desktop-minor-mode-table)

;; Add `company-elisp' backend for elisp.
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (require 'company-elisp)
             (push 'company-elisp company-backends)))

;; Key settings.
(lazy-unset-key
 '("TAB")
 company-mode-map)                      ;unset default keys

(lazy-unset-key
 '("M-p" "M-n" "M-1"
   "M-2" "M-3" "M-4"
   "M-5" "M-6" "M-7"
   "M-8" "M-9" "M-0"
   "C-m")
 company-active-map)

(lazy-set-key
 '(
   ("TAB" . company-complete-selection)
   ("M-h" . company-complete-selection)
   ("M-H" . company-complete-common)
   ("M-w" . company-show-location)
   ("M-s" . company-search-candidates)
   ("M-S" . company-filter-candidates)
   ("M-n" . company-select-next)
   ("M-p" . company-select-previous)
   )
 company-active-map)

;; Add yasnippet support for all company backends.
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(provide 'init-company-mode)

;;; init-company-mode.el ends here

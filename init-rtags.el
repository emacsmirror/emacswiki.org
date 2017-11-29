;;; init-rtags.el --- Init for rtags

;; Filename: init-rtags.el
;; Description: Init for rtags
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2017-11-28 20:09:51
;; Version: 0.1
;; Last-Updated: 2017-11-28 20:09:51
;;           By: Andy Stewart
;; URL:
;; Keywords: rtags
;; Compatibility: 27.0.50
;;
;; Features that might be required by this library:
;;
;; `window-extension'
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
;; Init for rtags
;;

;;; Installation:
;;
;; Put init-rtags.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-rtags)
;;
;; No need more.

;;; Change log:
;;
;; 2017/11/28
;;      First released.
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
(require 'window-extension)

;;; Code:

;; rtags compile steps:
;;
;; git clone --recursive https://github.com/Andersbakken/rtags.git
;; cd rtags
;; mkdir build
;; cd build
;; cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ..
;; make
;; sudo make install
;;
(dolist (hook (list
               'c-mode-common-hook
               'c-mode-hook
               'c++-mode-hook
               ))
  (add-hook hook
            (lambda ()
              (require 'rtags)
              (require 'helm-config)
              (require 'ac-rtags)
              (require 'flycheck)
              (require 'flycheck-rtags)

              ;; Run rtags daemon to provide tags request service.
              (rtags-start-process-unless-running)

              ;; Just kill window and buffer, don't break stack position.
              (setq rtags-bury-buffer-function 'delete-current-buffer-and-window)

              ;; Split window force at below.
              (setq rtags-split-window-function 'split-window-below)

              ;; Enable flycheck.
              (flycheck-mode)

              ;; Flycheck setup.
              (flycheck-select-checker 'rtags)
              (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
              (setq-local flycheck-check-syntax-automatically nil)

              ;; If your project is build with cmake,
              ;; you need use command "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ." to generate file `compile_commands.json'
              ;; then use command "rc -J json_file_directory" to index C/C++ project tag index.
              ;;
              ;; If your project is build with qmake,
              ;; you need use command "bear --append make" to generate `compile_commands.json' file after command "qmake .."
              ;; then use command "rc -J json_file_directory" to index C/C++ project tag index.
              (defun rtags-find-references-at-point+ ()
                (interactive)
                (rtags-find-references-at-point)
                ;; Switch window after poup rtag window.
                (other-window 1)
                )

              (lazy-set-key
               '(
                 ("C-8" . rtags-find-symbol-at-point)
                 ("C-9" . rtags-find-references-at-point+)
                 ("M-," . rtags-location-stack-back)
                 ("M-." . rtags-location-stack-forward)
                 ("M-'" . rtags-display-summary)
                 ("C-." . rtags-rename-symbol)
                 ("M-s-j" . flycheck-next-error)
                 ("M-s-k" . flycheck-previous-error)
                 )
               c-mode-base-map
               )
              )))

(provide 'init-rtags)

;;; init-rtags.el ends here

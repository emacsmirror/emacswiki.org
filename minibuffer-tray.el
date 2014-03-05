;;; minibuffer-tray.el --- Tray widget that stick right-edge of minibuffer

;; Filename: minibuffer-tray.el
;; Description: Tray widget that stick right-edge of minibuffer
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-01-26 01:00:18
;; Version: 0.1
;; Last-Updated: 2014-01-26 01:00:18
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/minibuffer-tray.el
;; Keywords:
;; Compatibility: GNU Emacs 24.3.50.1
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
;; Tray widget that stick right-edge of minibuffer
;;
;; Because i hide mode-line in Deepin-Emacs.
;; And i still need see informations, such as time, row/column, editable, sometimes.
;;
;; Since Emacs don't allowed split minibuffer window,
;; i use epc and pyqt implement tray widget that stick right-edge of minibuffer window,
;; for display information i want see.
;;

;;; Installation:
;;
;; NOTE: just minibuffer-tray.el can't work, you need install below depends first:
;;       * PyQt5:       http://www.riverbankcomputing.co.uk/software/pyqt/intro
;;       * Python-Xlib: https://pypi.python.org/pypi/python-xlib
;;       * Python-EPC:  https://github.com/tkf/python-epc
;;       
;; Detail description please look: http://www.emacswiki.org/emacs/MiniBufferTray
;;
;; Then put minibuffer-tray.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'minibuffer-tray)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET minibuffer-tray RET
;;

;;; Change log:
;;
;; 2014/01/26
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

(require 'epc)
(when noninteractive
  (load "subr")
  (load "byte-run"))
(eval-when-compile (require 'cl))

;;; Code:

(defvar minibuffer-tray-python-file (expand-file-name "tray.py" (file-name-directory load-file-name)))

(defvar minibuffer-tray-epc
  (epc:start-epc (or (getenv "PYTHON") "python")
                 (list minibuffer-tray-python-file)))

(defvar minibuffer-tray-height 20)

(defun minibuffer-tray-get-emacs-xid ()
  (frame-parameter nil 'window-id))

(defun minibuffer-tray-get-window-allocation ()
  (let* ((window-edges (window-inside-pixel-edges (minibuffer-window)))
         (x (nth 0 window-edges))
         (y (nth 1 window-edges))
         (w (- (nth 2 window-edges) x))
         (h (- (nth 3 window-edges) y)))
    (list x y w h)))

(defun minibuffer-tray-get-line-number ()
  (count-lines (point-min) (point-max)))

(defun minibuffer-tray-show ()
  (interactive)
  (let* ((window-allocation (minibuffer-tray-get-window-allocation))
         (x (nth 0 window-allocation))
         (y (nth 1 window-allocation))
         (w (nth 2 window-allocation))
         (h (nth 3 window-allocation)))
    (epc:call-deferred minibuffer-tray-epc 'init (list (minibuffer-tray-get-emacs-xid) minibuffer-tray-height))
    (epc:call-deferred minibuffer-tray-epc 'show ())
    (epc:call-deferred minibuffer-tray-epc 'set_minibuffer_allocation (list x y w h))
    )
  )

(defun minibuffer-tray-hide ()
  (interactive)
  (epc:call-deferred minibuffer-tray-epc 'hide ())
  )

(defun minibuffer-tray-update-cursor-pos ()
  (epc:call-deferred minibuffer-tray-epc 'update_pos (list (line-number-at-pos) (current-column) (minibuffer-tray-get-line-number)))
  )

(defun minibuffer-tray-monitor-frame-change ()
  (let* ((window-allocation (minibuffer-tray-get-window-allocation))
         (x (nth 0 window-allocation))
         (y (nth 1 window-allocation))
         (w (nth 2 window-allocation))
         (h (nth 3 window-allocation))
         )
    (epc:call-deferred minibuffer-tray-epc 'set_minibuffer_allocation (list x y w h))
    ))

(define-minor-mode minibuffer-tray-mode
  :global t
  :group 'minibuffer-tray-mode
  (if minibuffer-tray-mode
      (progn
        (minibuffer-tray-show)
        (add-hook 'window-configuration-change-hook #'minibuffer-tray-monitor-frame-change)
        )
    (minibuffer-tray-hide)
    (remove-hook 'window-configuration-change-hook 'minibuffer-tray-monitor-frame-change))
  )

(epc:define-method minibuffer-tray-epc
                   'message
                   (lambda (&rest args) (message "%S" args)))

(epc:define-method minibuffer-tray-epc
                   'update-cursor-pos
                   (lambda (&rest args)
                     (minibuffer-tray-update-cursor-pos)))

(provide 'minibuffer-tray)

;;; minibuffer-tray.el ends here

;;; minibuffer-tray.el --- Tray widget that stick right-edge of minibuffer

;; Filename: minibuffer-tray.el
;; Description: Tray widget that stick right-edge of minibuffer
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-01-26 01:00:18
;; Version: 0.5
;; Last-Updated: 2017-10-20 10:47:34
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
;;       * python-dbus
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
;; (minibuffer-tray-mode 1)
;;
;; No need more.

;;; Customize:
;;
;; `minibuffer-tray-hide-mode-line' default is t to hide mode-line.
;; `minibuffer-tray-height' the height of minibuffer-tray.
;; `minibuffer-tray-name' the name of minibuffer-tray buffer.
;;
;; All of the above can customize by:
;;      M-x customize-group RET minibuffer-tray RET
;;

;;; Change log:
;;
;; 2017/10/20
;;      * Add `minibuffer-tray-stop-kill-buffer' hook to avoid minibuffer tray's buffer kill by command `kill-buffer'.
;;
;; 2014/10/11
;;      * Add customize options.
;;
;; 2014/10/10
;;      * Don't query user minibuffer-tray process when emacs exit.
;;      * Add remove mode-line code in header comment.
;;
;; 2014/10/08
;;      * Use DBus instead Python-EPC as communication between Emacs and PyQt process.
;;      Elisp and python code much simple after switch to DBus implementation.
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

(require 'dbus)

;;; Code:

(defvar minibuffer-tray-python-file (expand-file-name "minibuffer_tray.py" (file-name-directory load-file-name)))

(defvar minibuffer-tray-process nil)

(defcustom minibuffer-tray-name "*minibuffer tray*"
  "Name of minibuffer-tray buffer."
  :type 'string
  :group 'minibuffer-tray)

(defcustom minibuffer-tray-height 20
  "Height of minibuffer-tray."
  :type 'int
  :group 'minibuffer-tray)

(defcustom minibuffer-tray-hide-mode-line t
  "Hide mode-line.
Default is t."
  :type 'boolean
  :set (lambda (symbol value)
         (set symbol value)
         (if value
             (custom-set-faces
              '(mode-line ((t (:background "#3E0303" :foreground "#3E0303" :height 1))))
              '(mode-line-highlight ((t (:height 1))))
              '(mode-line-inactive ((t (:background "gray10" :foreground "gray10" :height 1))))
              )
           ))
  :group 'minibuffer-tray)

(defun minibuffer-tray-call (method &rest args)
  (with-demoted-errors "minibuffer-tray-call ERROR: %s"
    (apply 'dbus-call-method
           :session                 ; use the session (not system) bus
           "com.deepin.minibuffer_tray"  ; service name
           "/com/deepin/minibuffer_tray" ; path name
           "com.deepin.minibuffer_tray"  ; interface name
           method args)))

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
    (minibuffer-tray-call "show")
    (minibuffer-tray-call "set_minibuffer_allocation" x y w h)
    )
  )

(defun minibuffer-tray-hide ()
  (interactive)
  (minibuffer-tray-call "hide")
  )

(defun minibuffer-tray-update-cursor-pos ()
  (minibuffer-tray-call "update_pos" (line-number-at-pos) (current-column) (minibuffer-tray-get-line-number))
  )

(defun minibuffer-tray-monitor-frame-change ()
  (let* ((window-allocation (minibuffer-tray-get-window-allocation))
         (x (nth 0 window-allocation))
         (y (nth 1 window-allocation))
         (w (nth 2 window-allocation))
         (h (nth 3 window-allocation))
         )
    (minibuffer-tray-call "set_minibuffer_allocation" x y w h)
    ))

(defun minibuffer-tray-start-process ()
  (setq minibuffer-tray-process
        (apply 'start-process
               minibuffer-tray-name
               minibuffer-tray-name
               "python" (list minibuffer-tray-python-file (minibuffer-tray-get-emacs-xid) (format "%s" minibuffer-tray-height))))
  (add-hook 'kill-buffer-query-functions 'minibuffer-tray-stop-kill-buffer)
  (set-process-query-on-exit-flag minibuffer-tray-process nil)
  (set-process-sentinel
   minibuffer-tray-process
   #'(lambda (process event)
       (message (format "%s %s" process event))
       ))
  )

(defun minibuffer-tray-stop-kill-buffer ()
  "Don't kill minibuffer tray buffer but burry buffer."
  (cond
   ((equal (buffer-name (current-buffer)) minibuffer-tray-name)
    (progn
      (bury-buffer)
      nil))
   (t)
   ))

(defun minibuffer-tray-stop-process ()
  (delete-process minibuffer-tray-process)
  )

(defun minibuffer-tray-enable ()
  (progn
    (minibuffer-tray-start-process)
    (minibuffer-tray-show)
    (add-hook 'window-configuration-change-hook #'minibuffer-tray-monitor-frame-change)
    (add-hook 'post-command-hook 'minibuffer-tray-update-cursor-pos)
    )
  )

(defun minibuffer-tray-disable ()
  (minibuffer-tray-hide)
  (remove-hook 'window-configuration-change-hook 'minibuffer-tray-monitor-frame-change)
  (remove-hook 'post-command-hook 'minibuffer-tray-update-cursor-pos)
  (minibuffer-tray-stop-process)
  )

(define-minor-mode minibuffer-tray-mode
  :global t
  :group 'minibuffer-tray-mode
  (if minibuffer-tray-mode
      (minibuffer-tray-enable)
    (minibuffer-tray-disable))
  )

(provide 'minibuffer-tray)

;;; minibuffer-tray.el ends here

;;; webkit.el --- Running WebKit browser in Emacs

;; Filename: webkit.el
;; Description: Running WebKit browser in Emacs
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-01-02 21:51:17
;; Version: 0.1
;; Last-Updated: 2014-01-02 21:51:17
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/webkit.el
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
;; Running WebKit browser in Emacs
;;

;;; Installation:
;;
;; NOTE: just webkit.el can't work, you need install below depends first:
;;       * PyQt5:       http://www.riverbankcomputing.co.uk/software/pyqt/intro
;;       * Python-Xlib: https://pypi.python.org/pypi/python-xlib
;;       * Python-EPC:  https://github.com/tkf/python-epc
;;       
;; Detail description please look: http://www.emacswiki.org/emacs/WebKit 
;;
;; Then put webkit.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'webkit)
;;
;; Quick start:
;; 
;; M-x webkit-open-url
;; 

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET webkit RET
;;

;;; Change log:
;;
;; 2014/01/02
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

(defcustom webkit-mode-hook '()
  "WebKit mode hook."
  :type 'hook
  :group 'webkit-mode)

(defvar webkit-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used by `webkit-mode'.")

(define-derived-mode webkit-mode text-mode "WebKit"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'webkit-mode)
  (setq mode-name "WebKit")
  (set (make-local-variable 'buffer-id) (webkit-generate-id))
  (use-local-map webkit-mode-map)
  (run-hooks 'webkit-mode-hook))

(defun webkit-get-window-allocation (&optional window)
  (let* ((window-edges (window-inside-pixel-edges window))
         (x (nth 0 window-edges))
         (y (nth 1 window-edges))
         (w (- (nth 2 window-edges) x))
         (h (- (nth 3 window-edges) y))
         )
    (list x y w h)))

(defun webkit-get-emacs-xid ()
  (frame-parameter nil 'window-id))

(defun webkit-generate-id ()
  (replace-regexp-in-string "\n" "" (shell-command-to-string "uuidgen")))

(defvar pyepc-file (expand-file-name "browser.py" (file-name-directory load-file-name)))

(defvar pyepc-browser
  (let* ((browser
          (epc:start-epc (or (getenv "PYTHON") "python")
                         (list pyepc-file))))
    (epc:call-deferred browser 'init (list (webkit-get-emacs-xid)))
    browser))

(defvar webkit-buffer-dict (make-hash-table :test 'equal))

(defvar webkit-title-length 30)

(defvar webkit-tab-index 0)

(defun webkit-create-buffer (url)
  (setq webkit-tab-index (+ webkit-tab-index 1))
  (let ((webkit-buffer (generate-new-buffer (concat (truncate-string-to-width url webkit-title-length)))))
    (with-current-buffer webkit-buffer
      (webkit-mode)
      (set (make-local-variable 'buffer-url) url)
      (puthash buffer-id webkit-buffer webkit-buffer-dict)
      )
    webkit-buffer))

(defun webkit-change-buffer-title (id title)
  (let* ((buffer (gethash id webkit-buffer-dict)))
    (with-current-buffer buffer
      (rename-buffer (truncate-string-to-width title webkit-title-length)))
    )
  )

(defun webkit-open-url (url)
  (interactive "sURL: ")
  (let* ((buffer (webkit-create-buffer url))
         (url-parts (split-string url "://"))
         )
    (unless (member (nth 0 url-parts) (list "http" "https" "ftp" "file"))
      (if (= (length url-parts) 1)
          (setq url (concat "http://" (nth 0 url-parts))))
      )
    (with-current-buffer buffer
      (let* ((window-allocation (webkit-get-window-allocation))
             (x (nth 0 window-allocation))
             (y (nth 1 window-allocation))
             (w (nth 2 window-allocation))
             (h (nth 3 window-allocation))
             (view-id (webkit-generate-id)))
        (epc:call-deferred pyepc-browser 'create_buffer (list buffer-id url w h))
        ))
    (switch-to-buffer buffer)
    ))

(defun webkit-monitor-window-change (&rest _)
  (let ((view-infos)
        (selected-buffer (window-buffer (selected-window))))
    (dolist (window (window-list))
      (setq buffer (window-buffer window))
      (with-current-buffer buffer
        (if (string= "webkit-mode" (format "%s" major-mode))
            (let* ((window-allocation (webkit-get-window-allocation window))
                   (x (nth 0 window-allocation))
                   (y (nth 1 window-allocation))
                   (w (nth 2 window-allocation))
                   (h (nth 3 window-allocation))
                   )
              (add-to-list 'view-infos (list buffer-id x y w h))
              ))))
    (epc:call-deferred pyepc-browser 'update_views (list view-infos))

    (with-current-buffer selected-buffer
      (if (string= "webkit-mode" (format "%s" major-mode))
          (let* ((window-allocation (webkit-get-window-allocation (selected-window)))
                 (w (nth 2 window-allocation))
                 (h (nth 3 window-allocation))
                 )
            (epc:call-deferred pyepc-browser 'adjust_size (list buffer-id w h))
            )))
    ))

(defun webkit-monitor-buffer-kill ()
  (with-current-buffer (buffer-name)
    (if (string= "webkit-mode" (format "%s" major-mode))
        (progn
          (epc:call-deferred pyepc-browser 'remove_buffer (list buffer-id))
          (remhash buffer-id webkit-buffer-dict)))))

(defun webkit-focus-browser-view ()
  (interactive)
  (with-current-buffer (current-buffer)
    (if (string= "webkit-mode" (format "%s" major-mode))
        (let* ((window-allocation (webkit-get-window-allocation (get-buffer-window (current-buffer))))
               (x (nth 0 window-allocation))
               (y (nth 1 window-allocation))
               (w (nth 2 window-allocation))
               (h (nth 3 window-allocation))
               )
          (epc:call-deferred pyepc-browser 'focus_view (list buffer-id x y w h))
          (message "Focus view: %S" buffer-id)
          )
      )))

(defadvice switch-to-buffer (after webkit-switch-to-buffer-advice activate)
  (webkit-focus-browser-view))

(defadvice other-window (after webkit-other-window-advice activate)
  (webkit-focus-browser-view))

(add-hook 'window-configuration-change-hook #'webkit-monitor-window-change)
(add-hook 'kill-buffer-hook #'webkit-monitor-buffer-kill)

(epc:define-method pyepc-browser
                   'message
                   (lambda (&rest args) (message "%S" args)))

(epc:define-method pyepc-browser
                   'open-url
                   (lambda (&rest args)
                     (webkit-open-url (nth 0 args))
                     ))

(epc:define-method pyepc-browser
                   'change-buffer-title
                   (lambda (&rest args)
                     (webkit-change-buffer-title (nth 0 args) (nth 1 args))
                     ))

(epc:define-method pyepc-browser
                   'focus-browser-view
                   (lambda (&rest args)
                     (webkit-focus-browser-view)
                     ))

(provide 'webkit)

;;; webkit.el ends here

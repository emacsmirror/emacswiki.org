;;; layout-restore.el
;; --- keep window configuration as layout and restore it simply.

;; Copyleft (C) Vektor

;; Emacs Lisp Archive Entry
;; Filename:      layout-restore.el
;; Version:       0.4
;; Keywords:      convenience window-configuration layout
;; Author:        Vektor (also Veldrin@SMTH)
;; Maintainer:    Vektor
;; Description:   keep window configuration as layout and restore it.
;; Compatibility: Emacs21.3 Emacs-CVS-21.3.50
;; URL:           http://www.emacswiki.org/cgi-bin/wiki/LayoutRestore

(defconst layout-restore-version "0.4"
  "LayoutRestore version number. The latest version is available from
http://www.emacswiki.org/cgi-bin/wiki/LayoutRestore")

;; NOTE: Read the commentary below for how to use this package.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;
;;
;; -------------
;; Background:
;; -------------
;;
;; Sometimes I use multi windows to do my job, and when i
;; switch to other buffer and go back, the original layout is
;; gone.  I have to set it up again.
;; I was very annoyed about this so I tried other packages to
;; help me out.  I found `WinnerMode', which always did
;; something I didn't want, and `TaskMode', which seems too
;; powerful to be used simply.  So I wrote this by myself.
;; Actually this package is my first emacs extention package
;; and I don't really know if I have made it well.  But I think
;; this package is already quite useable.
;;
;;
;;
;; -------------
;; Commands:
;; -------------
;;
;; `layout-save-current' save the current window-configuration
;; as layout so that when next time you switch back to this
;; buffer, the layout will be brought back automatically.  You
;; can also manually use `layout-restore' to restore the layout.
;; When you feel a layout is no more needed, switch to the
;; buffer where saved it and use `layout-delete-current' to
;; delete this layout.  These codes are simple and well
;; documented, you can easily hack it by yourself.
;;
;;
;;
;; -------------
;; Set it up:
;; -------------
;;
;; To start using this package, add following lines to your emacs
;; startup file.
;; ---------------------------------------------------------------
;; (require 'layout-restore)
;; ;; save layout key
;; (global-set-key [?\C-c ?l] 'layout-save-current)
;; ;; load layout key
;; (global-set-key [?\C-c ?\C-l ?\C-l] 'layout-restore)
;; ;; cancel(delete) layout key
;; (global-set-key [?\C-c ?\C-l ?\C-c] 'layout-delete-current)
;; ---------------------------------------------------------------
;; Change the keybindings to whatever you like.
;;
;;
;;
;; -----------------
;; Detailed example:
;; -----------------
;;
;; Let's suppose you are working in buffer A and the emacs now
;; looks like:
;;
;; +-------+
;; |   A_  |
;; +---+---+  (layout 1)
;; | B | C |
;; +---+---+
;;
;; Now for some reason, you want to switch to other buffer and
;; do some other thing, these make your emacs looks like:
;;
;; +---+---+
;; |   |   |
;; | D_| E |  (layout 2)
;; |   |   |
;; +---+---+
;;
;; Now you are working in buffer D and want to switch back to
;; buffer A.  Usually you can do this simply by `C-x b RET'.
;; But what about buffer B and buffer C?  They won't be back
;; automatically when you switch to buffer A.  So your emacs
;; looks like below:
;;
;; +---+---+
;; |   |   |
;; | A_| E |
;; |   |   |
;; +---+---+
;;
;; Well, I am sure this is NOT what you want.
;;
;; By using this package, when you are in buffer A of layout 1,
;; press `C-c l' to remember this layout. Then switch to buffer
;; D to your work.  Now when you switch back to buffer A, the
;; buffer B and C will be brought back automatically, and be
;; placed exactly as where they were.
;;
;; If you want, you can alsa remember layout 2 when you are in
;; buffer D.  simply press `C-c l' to remember it, then you can
;; switch between layout 1 and layout 2 easily when you switch
;; between buffer A and D.
;;
;; To unmemorise a layout, simply press `C-c C-l C-c' in the
;; buffer where you press `C-c l' before.
;;
;;
;;
;; -----------
;; Contact me:
;; -----------
;;
;; Any question, or advice, please mail to
;;
;; XYZvektorXYZ@XYZyeahXYZ.net
;;
;; remove all XYZ from above address to get the real one.
;;

;;; Code:




(require 'advice)


(defvar layout-configuration-alist nil
  "This list contains  window configuration to restore for certain
buffer correlated layout. Each element of this list is a list itself,
it consists of 'active buffer of this layout', 'window-configuration
of this layout', '(buffer . buffer-name) cons of this layout'.")

(defvar layout-accept-buffer-by-name t
  "This variable decide whether we'll accept a different buffer which have
the same name in case we could find the original buffer. Useful when we want
to keep a layout after close one of its buffer and reopen it.")

(defvar layout-verbose t
  "Print verbose message.")

(defvar layout-restore-old-window-point nil
  "Restore the window point at the old place where layout recorded it.")

(defvar layout-restore-after-switchbuffer t
  "If we should restore layout after `switch-buffer'.")

(defvar layout-restore-after-killbuffer t
  "If we should restore layout after `kill-buffer'.")

(defvar layout-restore-after-otherwindow nil
  "If we should restore layout after `other-window', which normally invoked
by C-x o.")


(defun layout-save-current ()
  "Save the current layout, add a list of current layout to
layout-configuration-alist."
  (interactive)
  (let ((curbuf (current-buffer))
        (curwincfg (current-window-configuration))
        layoutcfg)
    (setq layoutcfg (list curbuf curwincfg))
    (dolist (window (window-list nil 0))
      ;; (window-list) maybe contain a minibuffer, append (nil 0) to avoid
      (setq layoutcfg
            (append layoutcfg
                    (list (cons (window-buffer window)
                                (buffer-name (window-buffer window)))))))
    (dolist (locfg layout-configuration-alist)
      (if (eq curbuf (car locfg))
          (setq layout-configuration-alist
                (delq locfg layout-configuration-alist))))
    (setq layout-configuration-alist
          (cons layoutcfg layout-configuration-alist)))
  (if layout-verbose (message "Current layout saved.")))
  

(defun layout-restore (&optional BUFFER)
  "Restore the layout related to the buffer BUFFER, if there is such a layout
saved in `layout-configuration-alist', and update the layout if necessary."
  (interactive)
  (if (not BUFFER) (setq BUFFER (current-buffer)))
  (let (wincfg
        buflist
        (new-point (not layout-restore-old-window-point))
        new-point-list
        buffer-changed-p
        bufname-changed-p
        new-buffer-cons-list
        (restorep t))
    (dolist (locfg layout-configuration-alist)
      (when (eq BUFFER (car locfg))
        (setq wincfg (cadr locfg))
        (setq buflist (cddr locfg))))
    (when wincfg
      (dolist (bufcons buflist)
        (if (buffer-live-p (car bufcons))
            (if (not (string= (buffer-name (car bufcons))
                              (cdr bufcons)))
                
                (setq bufname-changed-p t
                      new-buffer-cons-list
                      (append new-buffer-cons-list
                              (list (cons (car bufcons)
                                          (buffer-name (car bufcons)))))
                      new-point-list (append new-point-list
                                             (list (save-excursion
                                                     (set-buffer (car bufcons))
                                                     (point)))))
              (setq new-buffer-cons-list (append new-buffer-cons-list (list bufcons))
                    new-point-list (append new-point-list
                                           (list (save-excursion
                                                   (set-buffer (car bufcons))
                                                   (point))))))
          (if (not layout-accept-buffer-by-name)
              (setq buffer-changed-p t
                    restorep nil)
            ;; accept reopened buffer by name, if any
            (progn
              (setq buffer-changed-p t)
              (let ((rebuf (get-buffer (cdr bufcons))
                           ))
                (if (not rebuf)
                    (setq restorep nil)
                  (setq new-buffer-cons-list
                        (append new-buffer-cons-list
                                (list (cons rebuf (cdr bufcons))))
                        new-point-list
                        (append new-point-list
                                (list (save-excursion
                                        (set-buffer rebuf)
                                        (point)))))))))))
      (when restorep
        (set-window-configuration wincfg)
        (dolist (window (window-list nil 0))
          (set-window-buffer window (caar new-buffer-cons-list))
          (setq new-buffer-cons-list (cdr new-buffer-cons-list))
          (when new-point
            (set-window-point window (car new-point-list))
            (setq new-point-list (cdr new-point-list))))
        (if (or bufname-changed-p buffer-changed-p)
            (layout-save-current))
        (if layout-verbose (message "Previous saved layout restored.")))
      )))

(defun layout-delete-current (&optional BUFFER)
  "Delete the layout information from `layout-configuration-alist'
if there is an element list related to BUFFER."
  (interactive)
  (if (not BUFFER) (setq BUFFER (current-buffer)))
  (dolist (locfg layout-configuration-alist)
    (when (eq BUFFER (car locfg))
      (setq layout-configuration-alist
            (delq locfg layout-configuration-alist))
      (if layout-verbose (message "Layout about this buffer deleted.")))
    ))

(defun layout-unique-point-in-same-buffer-windows (&optional BUFFER)
  "Make identical opint in all windows of a same buffer."
  (if (not BUFFER) (setq BUFFER (current-buffer)))
  (dolist (locfg layout-configuration-alist)
    (when (eq BUFFER (car locfg))
      (save-excursion
        (set-buffer BUFFER)
        (let ((wlist (get-buffer-window-list BUFFER 0)))
          (dolist (window wlist)
            (set-window-point window (point))))))))
        

(defadvice switch-to-buffer (around layout-restore-after-switch-buffer (BUFFER))
  "Unique window point before `switch-to-buffer', and restore possible layout
after `switch-to-buffer'."
  (layout-unique-point-in-same-buffer-windows)
  ad-do-it
  (if layout-restore-after-switchbuffer 
      (layout-restore)))

(defadvice kill-buffer (after layout-restore-after-kill-buffer (BUFFER))
  "Restore possible layout after `kill-buffer' funcall."
  (if layout-restore-after-killbuffer
      (layout-restore)))

(defadvice other-window (after layout-restore-after-other-window (ARG))
  "Restore possible layout after `other-window' funcall."
  (if layout-restore-after-otherwindow
      (layout-restore)))

(ad-activate 'switch-to-buffer)
(ad-activate 'kill-buffer)
(ad-activate 'other-window)


(provide 'layout-restore)

;;; layout-restore.el ends here.

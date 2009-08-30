;;; winpoint.el --- Remember buffer positions per-window, not per buffer

;; Copyright (C) 2006  Jorgen Schaefer
;; Copyright (C) 2009  Andy Stewart

;; Filename: winpoint.el
;; Description: Remember buffer positions per-window, not per buffer
;; Author: Jorgen Schaefer <forcer@forcix.cx>
;; Maintainer: Jorgen Schaefer <forcer@forcix.cx>
;;             Andy Stewart <lazycat.manatee@gmail.com>
;; Created: 2006
;; Version: 1.3
;; Last-Updated: 2009-01-13 12:01:14
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/winpoint.el
;; Keywords: winpoint
;; Compatibility: GNU Emacs 23.0.60.1
;;                GNU Emacs 22
;;                GNU Emacs 21
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

;;; Commentary:
;;
;; When two windows view the same buffer at the same time, and one
;; window is switched to another buffer and back, point is now the
;; same as in the other window, not as it was before we switched away.
;; This mode tries to work around this problem by storing and
;; restoring per-window positions for each buffer.
;;

;;; Installation:
;;
;; Put winpoint.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'winpoint)
;;
;; And add below to enable `winpoint'.
;;
;;      (window-point-remember-mode 1)
;;

;;; Customize:
;;
;; `winpoint-non-restore-buffer-list'
;;
;; This option contain a buffer that don't want to restore point.
;;
;; For example, setup *Group* buffer like below:
;;
;; (setq winpoint-non-restore-buffer-list '("*Group*"))
;;

;;; Change log:
;;
;; 2009/01/13
;;   * Andy Stewart:
;;      * Make code compatibility with Emacs 23.
;;      * Add new option `winpoint-non-restore-buffer-list'
;;        to avoid some buffer restored window point.
;;
;; 2006
;;   * Jorgen Schaefer:
;;      * First released.
;;

;;; Require


;;; Code:

(defgroup winpoint nil
  "Remember buffer positions per-window, not per buffer."
  :group 'window)

(defcustom winpoint-non-restore-buffer-list
  '()
  "The buffer list that don't restore point."
  :type 'list
  :group 'winpoint)

(defvar winpoint-frame-windows nil
  "The current frame's windows and their buffers.
This is an alist mapping windows to their current buffers.")
;; Emacs 23 not need below setup anymore.
(when (> emacs-major-version 22)
  (eval-when-compile
    (with-no-warnings
      (make-variable-frame-local 'winpoint-frame-windows))))

(defvar winpoint-frame-positions nil
  "The current frame's windows and their associated buffer positions.
This is an alist mapping windows to an alist mapping buffers to
their stored point marker.")
;; Emacs 23 not need below setup anymore.
(when (> emacs-major-version 22)
  (eval-when-compile
    (with-no-warnings
      (make-variable-frame-local 'winpoint-frame-positions))))

(defalias 'window-point-remember-mode 'winpoint-mode)
(define-minor-mode winpoint-mode
  "Remember positions in a buffer per-window, not per-buffer.
That is, when you have the same buffer open in two different
windows, and you switch the buffer in one window and back again,
the position is the same as it was when you switched away, not
the same as in the other window."
  :global t
  (cond
   (winpoint-mode
    (add-hook 'post-command-hook 'winpoint-remember)
    (add-hook 'window-configuration-change-hook
              'winpoint-remember-configuration))
   (t
    (remove-hook 'post-command-hook 'winpoint-remember)
    (remove-hook 'window-configuration-change-hook
                 'winpoint-remember-configuration))))

(defun winpoint-remember ()
  "Remember the currently visible buffer's positions.
This should be put on `post-command-hook'."
  (walk-windows (lambda (win)
                  (let ((buf (window-buffer win)))
                    (winpoint-put win
                                  buf
                                  (window-point win))))))

(defun winpoint-remember-configuration ()
  "This remembers the currently shown windows.
If any buffer wasn't shown before, point in that window is
restored.
If any window isn't shown anymore, forget about it."
  (winpoint-clean)
  (setq winpoint-frame-windows
        (mapcar (lambda (win)
                  (let ((old (assq win winpoint-frame-windows))
                        (newbuf (window-buffer win)))
                    (when (and old
                               (not (eq (cdr old)
                                        newbuf)))
                      (winpoint-restore win))
                    (cons win newbuf)))
                (window-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs 21 compatibility
(eval-when-compile
  (when (not (fboundp 'with-selected-window))
    (defmacro with-selected-window (window &rest body)
      "Execute the forms in BODY with WINDOW as the selected window."
      `(save-selected-window
         (select-window ,window)
         ,@body))))

(defun winpoint-restore (win)
  "Restore point in the window WIN."
  (with-selected-window win
    (let ((point (winpoint-get win (current-buffer))))
      (when (and point
                 (not (winpoint-match-non-restore-buffer (buffer-name))))
        (goto-char point)))))

(defun winpoint-match-non-restore-buffer (bufname)
  "Function document"
  (let (buffer-match-p)
    (catch 'match
      (dolist (element winpoint-non-restore-buffer-list)
        (when (string-equal element bufname)
          (setq buffer-match-p t)
          (throw 'match "Match non restored buffer."))))
    buffer-match-p))

(defun winpoint-get (win buf)
  "Return a cons cell of the stored point of BUF in WIN."
  (let ((window-entry (assq win winpoint-frame-positions)))
    (when window-entry
      (let ((buffer (assq buf (cdr window-entry))))
        (when buffer
          (cdr buffer))))))

(defun winpoint-put (win buf point)
  "Store POINT as the current point for BUF in WIN."
  (let ((window-entry (assq win winpoint-frame-positions)))
    (if window-entry
        (let ((buffer (assq buf (cdr window-entry))))
          (if buffer
              (set-marker (cdr buffer) point buf)
            (setcdr window-entry
                    (cons `(,buf . ,(set-marker (make-marker)
                                                point
                                                buf))
                          (cdr window-entry)))))
      (setq winpoint-frame-positions
            (cons `(,win . ((,buf . ,(set-marker (make-marker)
                                                 point
                                                 buf))))
                  winpoint-frame-positions)))))

(defun winpoint-clean ()
  "Remove unknown windows."
  (let ((windows (window-list)))
    (setq winpoint-frame-positions
          (filter-map! (lambda (entry)
                         (let ((bufs (filter! (lambda (buf+pos)
                                                (buffer-live-p (car buf+pos)))
                                              (cdr entry))))
                           (when (not (null bufs))
                             (cons (car entry)
                                   bufs))))
                       winpoint-frame-positions))))

(defun filter-map! (fun list)
  "Map FUN over LIST, retaining all non-nil elements."
  (while (and list
              (not (setcar list (funcall fun (car list)))))
    (setq list (cdr list)))
  (when list
    (let ((list list))
      (while (cdr list)
        (if (not (setcar (cdr list)
                         (funcall fun (cadr list))))
            (setcdr list (cddr list))
          (setq list (cdr list))))))
  list)

(defun filter! (pred list)
  "Return all elements of LIST for which PRED returns non-nil.
This modifies LIST whenever possible."
  (while (and list
              (not (funcall pred (car list))))
    (setq list (cdr list)))
  (when list
    (let ((list list))
      (while (cdr list)
        (if (not (funcall pred (cadr list)))
            (setcdr list (cddr list))
          (setq list (cdr list))))))
  list)

(provide 'winpoint)

;;; winpoint.el ends here

;;; LocalWords:  winpoint Jorgen Schaefer buf newbuf defmacro bufname bufs pos
;;; LocalWords:  pred

;;; docmap.el - display document map window
;;; $Id: docmap.el,v 1.1.1.1 2004/12/21 11:29:52 noir Exp $
;;; Copyright (C) 2004 Hiroyuki KUROSAKI <noir@st.rim.or.jp>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;;
;;; INSTALLATION
;;;
;;; In your ~/.emacs,
;;;   (autoload 'docmap-setup "docmap" nil t)
;;;
;;; USAGE
;;;
;;;   When editing a text, execute M-x docmap-setup . A document
;;; map window appears at the left side.
;;;
;;;   The buffer displayed in the document map window is
;;; an "indirect buffer" (see the Emacs manual) of
;;; the current buffer. It is in outline minor mode.
;;; Changes to either buffer reflect each other.
;;;
;;;   You can select the document map window with the normal
;;; operation, C-x o . In this window, C-c RET means the jump
;;; into the corresponding position of the main window.
;;; To quit, press C-c q .

(require 'outline)

(defvar docmap-window-column 20 "Column size of document map window.")

(defun docmap-setup ()
  "Set up a document map window."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally docmap-window-column)
  (let* ((baseb (current-buffer))
	 (docb (or (docmap-find-docmap-buffer baseb)
		   (docmap-create-docmap-buffer baseb))))
    (switch-to-buffer docb)
    (other-window 1)
    (switch-to-buffer baseb)))

(defun docmap-find-docmap-buffer (basebuf)
  (let ((docb nil) (blist (buffer-list)))
    (while blist
      (let ((b (car blist)))
	(when (and
	       (eq basebuf (buffer-base-buffer b))
	       (string= "*DocMap*" (substring (buffer-name b) 0 8)))
	  (setq docb b)
	  (setq blist nil))
	(setq blist (cdr blist))))
    docb))

(defun docmap-create-docmap-buffer (basebuf)
  (let ((docb
	 (make-indirect-buffer
	  basebuf
	  (concat "*DocMap* (" (buffer-name) ")")
	  t)))
    (save-excursion
      (save-window-excursion
	(set-buffer docb)
	(goto-char (point-min))
	(outline-minor-mode 1)
	(docmap-mode t)
	(hide-body)))
    docb))

(defun docmap-in-docmap-buffer-p ()
  (and (buffer-base-buffer) docmap-mode))

(defun docmap-quit ()
  "Close the document map window."
  (interactive)
  (when (docmap-in-docmap-buffer-p)
    (let ((mp (buffer-modified-p)) (bf (buffer-base-buffer)))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer))
      (other-window 1)
      (switch-to-buffer bf)
      (set-buffer-modified-p mp)
      (delete-other-windows))))

(defun docmap-goto-point ()
  "Go to the point of document map within the main window."
  (interactive)
  (when (docmap-in-docmap-buffer-p)
    (let ((pos (point)) (bf (buffer-base-buffer)))
      (other-window 1)
      (switch-to-buffer bf)
      (goto-char pos))))

(define-minor-mode docmap-mode
  "Toggle docmap mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

This mode is usually for the document map window.
With C-c RET key you can jump into the point on
the main window. C-c q quits the document map."
  nil
  " DocMap"
  '(("\C-c\C-m" . docmap-goto-point)
    ("\C-cq" . docmap-quit))
  :group 'docmap)

(provide 'docmap)
;;; docmap.el ends here

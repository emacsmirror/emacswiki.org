;;; memo-pop.el --- Helps you pop up and pop out a specific file buffer easily.
;;; $Id: memo-pop.el,v 1.3 2009/06/09 06:23:10 kyagi Exp $

;; Copyright (C) 2009 Free Software Foundation, Inc.

;; Author:        Kazuo YAGI <kyagi@1liner.jp>
;; Maintainer:    Kazuo YAGI <kyagi@1liner.jp>
;; Created:       2009-06-03
;; Last-Updated:  $Date: 2009/06/09 06:23:10 $
;; Revision:      $Revision: 1.3 $
;; Keywords:      tools
;; Compatibility: GNU Emacs 23.x

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary;
;;
;; This is a utility which helps you pop up and pop out a specifc file buffer
;; window easily. Just do M-x memo-pop, and it is strongly recommmended
;; to assign one hot-key to this function.
;;
;; I hope this is useful for you, and ENJOY YOUR HAPPY HACKING!
;;

;;; Configration;
;;
;; You just have only to set hot-keys to your most visiting file and
;; type that hot-keys to visit that file and return from it promptly.
;;
;; (require 'memo-pop)
;; (memo-pop-set-key-and-file [f1] "/home/kyagi/memo/memo.txt")
;; (memo-pop-set-key-and-file [f2] "/home/kyagi/memo/dict.txt")
;; (memo-pop-set-key-and-file [f3] (concat "/home/kyagi/diary/"
;;                            (format-time-string "%Y-%m-%d") ".txt"))
;;
;; Besides, you can set the window height, the number for the percentage
;; for selected window.
;;
;; (memo-pop-set-window-height 60)
;;
;; In addition, you can set each emacs mode for your `memo-pop'ed to make it
;; easier to edit them.
;;
;; (push '("memo.txt" . changelog-mode) auto-mode-alist)
;; (push '("dict.txt" . flyspell-mode) auto-mode-alist)
;; (push '("dict.txt" . outline-mode) auto-mode-alist)
;; (push '("dict.txt" . linum-mode) auto-mode-alist)
;;

;;; Update Info;
;;
;; $Log: memo-pop.el,v $
;; Revision 1.3  2009/06/09 06:23:10  kyagi
;; - Enabled saving buffer when poping out the visiting buffer window.
;;
;; Revision 1.2  2009/06/05 08:15:03  kyagi
;; - Improved the function 'memo-pop-set-key-and-file' to handle many combination of key and file.
;;
;; Revision 1.1  2009/06/03 11:33:07  kyagi
;; - Initial revision
;;
;;

;;; Code;

(defvar memo-pop-last-buffer nil)
(defvar memo-pop-last-window nil)
(defvar memo-pop-file-path nil)
(defvar memo-pop-file-buffer nil)
(defvar memo-pop-window-height 60) ; percentage for pop up buffer window height

(defun memo-pop-set-key-and-file (key path)
  (interactive "kInput a key-sequence and return: \nsInput path you want to visit promptly: ")
  (lexical-let ((key key)(path path))
    (global-set-key key (lambda () (interactive)
                          (memo-pop path))))
  (message "%s is assigned to %s now" (key-description key) path))

(defun memo-pop-set-window-height (number)
  (interactive "nInput the number for the percentage of \
selected window height (10-90): ")
  (setq memo-pop-window-height number))

(defun memo-pop (path)
  (interactive)
  (setq memo-pop-file-path path)
  (setq memo-pop-file-buffer (file-name-nondirectory path))
  (if (equal (buffer-name) memo-pop-file-buffer)
      (memo-pop-out)
    (memo-pop-up)))

(defun memo-pop-up ()
  (let ((w (get-buffer-window memo-pop-file-buffer)))
    (if w
        (select-window w)
      (progn
        ; save memo-pop-last-buffer and memo-pop-last-window to return
          (setq memo-pop-last-buffer (buffer-name))
          (setq memo-pop-last-window (selected-window))
          (split-window (selected-window)
                        (round (* (window-height)
                                  (/ (- 100 memo-pop-window-height) 100.0))))
          (other-window 1)
          (if (not (get-buffer memo-pop-file-buffer))
              (find-file memo-pop-file-path)
            (switch-to-buffer memo-pop-file-buffer))))))

(defun memo-pop-out ()
  (save-buffer)
  (delete-window)
  (select-window memo-pop-last-window)
  (switch-to-buffer memo-pop-last-buffer))

(provide 'memo-pop)

;;; memo-pop.el ends here.

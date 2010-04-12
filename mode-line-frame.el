;;;; mode-line-frame.el --- Create information frame like mode-line^
;; $Id: mode-line-frame.el,v 1.3 2010/04/11 22:06:51 rubikitch Exp $

;; Copyright (C) 2010  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: extensions, frames
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/mode-line-frame.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This file offers a frame to show various information. The mode-line
;; and the header-line play this role, but these are too short. This
;; file solves this problem. A separate frame plays this role. The
;; size of it is arbitrary!

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `mode-line-frame-create'
;;    Create mode-line frame.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `mode-line-frame-parameters'
;;    *`frame-parameters' of Mode-line frame.
;;    default = (quote ((title . "mode-line-frame") (name . "mode-line-frame") (cursor-type) (minibuffer) (mode-line)))
;;  `mode-line-frame-format'
;;    *Contents of mode-line-frame buffer. See `mode-line-frame' docstring.
;;    default = (quote ("" display-time-string " " (:eval ...) " " ...))
;;  `mode-line-frame-update-functions'
;;    *Functions to calculate `mode-line-frame-format'.
;;    default = nil

;;; Installation:
;;
;; Put mode-line-frame.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'mode-line-frame)
;; (mode-line-frame-create)
;;
;; Then, the place the mode-line-frame at the bottom of screen by
;; window manager.
;;
;; To utilize this file, you should customize `mode-line-frame-format'
;; and `mode-line-frame-update-functions'.
;;

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET mode-line-frame RET
;;


;;; History:

;; $Log: mode-line-frame.el,v $
;; Revision 1.3  2010/04/11 22:06:51  rubikitch
;; modify `mode-line-frame-parameters'
;;
;; Revision 1.2  2010/04/11 21:46:11  rubikitch
;; modify doc
;;
;; Revision 1.1  2010/04/11 21:44:59  rubikitch
;; Initial revision
;;

;;; Code:

(defvar mode-line-frame-version "$Id: mode-line-frame.el,v 1.3 2010/04/11 22:06:51 rubikitch Exp $")
(eval-when-compile (require 'cl))
(defgroup mode-line-frame nil
  "mode-line-frame"
  :group 'emacs)

(defvar mlf-frame nil)
(defvar mlf-buffer "*mode-line-frame*")
(defcustom mode-line-frame-parameters
  '((title . "mode-line-frame")
    (name . "mode-line-frame")
    (cursor-type . nil)
    (minibuffer . nil)
    (mode-line . nil))
  "*`frame-parameters' of Mode-line frame."
  :type 'sexp  
  :group 'mode-line-frame)
(defcustom mode-line-frame-format
  '(""
    display-time-string " "
    (:eval (prin1-to-string (load-average t))) " "
    "file=%f" " " "line=%l" " " "cols=%c" " " "size=%i" )
  "*Contents of mode-line-frame buffer. See `mode-line-frame' docstring.
It is parsed by `format-mode-line'. "
  :type 'sexp
  :group 'mode-line-frame)

(defcustom mode-line-frame-update-functions nil
  "*Functions to calculate `mode-line-frame-format'."
  :type 'boolean-integer-string-hook  
  :group 'mode-line-frame)

(defun mode-line-frame-create ()
  "Create mode-line frame."
  (interactive)
  (when window-system
    (let ((orig-frame (selected-frame)))
      (unless (and mlf-frame (frame-live-p mlf-frame))
        (setq mlf-frame (make-frame mode-line-frame-parameters)))
      (select-frame mlf-frame)
      (switch-to-buffer mlf-buffer)
      (buffer-disable-undo)
      (setq mode-line-format nil)
      (mlf-updater)
      (select-frame orig-frame))))

(defun mlf-updater ()
  (let ((buf (current-buffer)))
    (with-current-buffer (get-buffer-create mlf-buffer)
      (erase-buffer)
      (setq truncate-lines t)
      (run-hooks 'mode-line-frame-update-functions)
      ;; the CORE of this program!
      (insert (format-mode-line mode-line-frame-format nil nil buf)))))

(defvar mlf-update-last-time 0.0)
(defvar mlf-update-interval 0.9)
(defun mlf-updater-scheduled ()
  (let ((now (float-time)))
    (when (< mlf-update-interval (- now mlf-update-last-time))
      (mlf-updater)
      (setq mlf-update-last-time now))))
(defvar mlf-timer (run-with-timer 0 1 'mlf-updater-scheduled))

(defadvice force-mode-line-update (after mode-line-frame activate)
  (mlf-updater-scheduled))
;; (progn (ad-disable-advice 'force-mode-line-update 'after 'mode-line-frame) (ad-update 'force-mode-line-update))

(provide 'mode-line-frame)

;; Test (save-window-excursion (shell-command (format "emacs-test -l test-minimum -l %s %s &" buffer-file-name buffer-file-name)))
;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "mode-line-frame.el")
;;; mode-line-frame.el ends here

;;; joseph-scroll-screen.el Scroll half screen down or up, and highlight current line.

;; Filename: joseph-scroll-screen.el
;; Description: Scroll half screen down or up, and highlight current line
;; Author: Joseph <jixiuf@gmail.com>
;; Maintainer: Joseph <jixiuf@gmail.com>
;; Copyright (C) 2011~, Joseph, all rights reserved.
;; Created: 2011-03-01
;; Version: 0.1.1
;; URL: http://www.emacswiki.org/joseph-scroll-screen.el
;; Keywords: scroll screen
;; Compatibility: (Test on GNU Emacs 23.2.1).
;;
;;; This file is NOT part of GNU Emacs
;;
;;{{{ License
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
;;}}}
;;{{{ Commentary

;;; Commentary:

;;; Install:
;;
;; Just put joseph-scroll-screen.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;;  (require 'joseph-scroll-screen)
;;  (global-set-key "\C-v" 'joseph-scroll-half-screen-down)
;;  (global-set-key "\M-v" 'joseph-scroll-half-screen-up)
;;
;;  or:
;;
;;  (autoload 'joseph-scroll-half-screen-down "joseph-scroll-screen" "scroll half screen down" t)
;;  (autoload 'joseph-scroll-half-screen-up "joseph-scroll-screen" "scroll half screen up" t)
;;  (global-set-key "\C-v" 'joseph-scroll-half-screen-down)
;;  (global-set-key "\M-v" 'joseph-scroll-half-screen-up)
;;

;;}}}
;;{{{ Commands and Customizable Options

;;
;;; Commands:
;;
;; Below are complete command list:
;;
;;  `joseph-scroll-half-screen-down'
;;    scroll half screen down
;;  `joseph-scroll-half-screen-up'
;;    scroll half screen up
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `joseph-scroll-half-screen-up-hook'
;;
;;    default = nil
;;  `joseph-scroll-half-screen-down-hook'
;;
;;    default = nil
;;  `joseph-scroll-screen-line-num'
;;    after scroll ,point will be keeped on this line of screen
;;    default = 5
;;  `joseph-scroll-highlight-delay'
;;    *How long to highlight the tag.
;;    default = 0.3

;;}}}
;;; Codes
(defcustom  joseph-scroll-half-screen-up-hook nil
  ""
  :type 'hook)
(defcustom  joseph-scroll-half-screen-down-hook nil
  ""
  :type 'hook)
(defcustom joseph-scroll-screen-line-num 5
  "after scroll ,point will be keeped on this line of screen"
  :group 'scroll-screen
  :type 'number)

(defcustom joseph-scroll-highlight-delay 0.3
  "*How long to highlight the tag.
  (borrowed from etags-select.el)"
  :group 'scroll-
  :type 'number)

(defface scroll-highlight-line-face
  '((t (:foreground "white" :background "cadetblue4" :bold t)))
  "Font Lock mode face used to highlight tags.
  (borrowed from etags-select.el)"
  :group 'scroll-screen)

(defface joseph-scroll-highlight-line-face
  '((t (:foreground "white" :background "cadetblue4" :bold t)))
  "Font Lock mode face used to highlight tags.
  (borrowed from etags-select.el)"
  :group 'scroll-screen)

(defun joseph-scroll-highlight (beg end)
  "Highlight a region temporarily.
   (borrowed from etags-select.el)"
  (if (featurep 'xemacs)
      (let ((extent (make-extent beg end)))
        (set-extent-property extent 'face 'joseph-scroll-highlight-line-face)
        (sit-for joseph-scroll-highlight-delay)
        (delete-extent extent))
    (let ((ov (make-overlay beg end)))
      (overlay-put ov 'face 'joseph-scroll-highlight-line-face)
      (sit-for joseph-scroll-highlight-delay)
      (delete-overlay ov))))

(defvar joseph-scroll-screen-previous-point nil)
;;{{{ scroll up down
;;;###autoload'
(defun joseph-scroll-half-screen-down()
  "scroll half screen down"
  (interactive)
  (let ((old-position joseph-scroll-screen-previous-point))
    (setq joseph-scroll-screen-previous-point (point-marker))
    (if (and (not (equal (marker-position old-position) (point)))
             (equal last-command 'joseph-scroll-half-screen-up))
        (goto-char (marker-position old-position))
      (forward-line  (round (/ (frame-height) 1.5) ))
      )
    (when (and (member  major-mode '(dired-mode wdired-mode))
               (equal  (point-max) (point)) )
      (dired-previous-line 1)
      )
    (recenter joseph-scroll-screen-line-num);;keep point on this line.
    (joseph-scroll-highlight (point-at-bol)(1+ (point-at-eol))))
  (run-hooks 'joseph-scroll-half-screen-down-hook))

(defun joseph-scroll-half-screen-up()
  "scroll half screen up"
  (interactive)
  (let ((old-position joseph-scroll-screen-previous-point))
    (setq joseph-scroll-screen-previous-point (point-marker))
    (if (and (not (equal (marker-position old-position) (point)))
             (equal last-command 'joseph-scroll-half-screen-down))
        (goto-char (marker-position old-position))
      (forward-line (- 0 (round (/(frame-height) 1.5)))))
    (when (and (member  major-mode '(dired-mode wdired-mode))
               (equal  (point-min) (point)))
      (dired-next-line 2))
    (recenter joseph-scroll-screen-line-num)
    (joseph-scroll-highlight (point-at-bol)(1+ (point-at-eol))))
  (run-hooks 'joseph-scroll-half-screen-up-hook))

;;}}}

(provide 'joseph-scroll-screen)
;; joseph-scroll-screen.el ends here

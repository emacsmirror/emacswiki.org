;;; gold-ratio-scroll-screen.el -- Scroll half screen down or up, and highlight current line.

;; Filename: gold-ratio-scroll-screen.el
;; Description: Scroll half screen down or up, and highlight current line
;; Author: 纪秀峰 <jixiuf at gmail dot com>
;; Copyright (C) 2011~2013,纪秀峰 , all rights reserved.
;; Created: 2011-03-01
;; Version: 0.2
;; X-URL: git://github.com/jixiuf/gold-ratio-scroll-screen.git
;; Keywords: scroll screen
;;
;;; This file is NOT part of GNU Emacs
;;
;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Install:
;;
;; Just put gold-ratio-scroll-screen.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'gold-ratio-scroll-screen)
;; (global-set-key "\C-v" 'gold-ratio-scroll-screen-down)
;; (global-set-key "\M-v" 'gold-ratio-scroll-screen-up)
;;
;; or:
;;
;; (autoload 'gold-ratio-scroll-screen-down "gold-ratio-scroll-screen" "scroll half screen down" t)
;; (autoload 'gold-ratio-scroll-screen-up "gold-ratio-scroll-screen" "scroll half screen up" t)
;; (global-set-key "\C-v" 'gold-ratio-scroll-screen-down)
;; (global-set-key "\M-v" 'gold-ratio-scroll-screen-up)
;;

;;; Codes

;; (require 'dired)

(declare-function dired-previous-line "dired")
(declare-function dired-next-line "dired")
(autoload 'dired-previous-line "dired")
(autoload 'dired-next-line "dired")

(defgroup gold-ratio-scroll-screen nil
  "scroll screen half down or up."
  :prefix "gold-ratio-scroll-screen"
  :group 'scrolling)
(defcustom gold-ratio-scroll-recenter t
  "recenter or not after scroll"
  :group 'gold-ratio-scroll-screen
  :type 'boolean)

(defcustom gold-ratio-scroll-screen-ratio 1.618
  "forward or backward (frame-height)/this-value lines "
  :group 'gold-ratio-scroll-screen
  :type 'number)

(defcustom gold-ratio-scroll-highlight-flag 'both
  "highlight or not before or after scroll"
  :group 'gold-ratio-scroll-screen
  :type '(choice (const :tag "do not highlight" nil)
                 (const :tag "highlight line before scroll" 'before)
                 (const :tag "highlight line after scroll" 'after)
                 (const :tag "highlight line after scroll" 'both)))

(defcustom gold-ratio-scroll-highlight-delay (cons 0.15 0.1)
  "*How long to highlight the line .
 (borrowed from etags-select.el)"
  :group 'gold-ratio-scroll-screen
  :type 'number)

(defcustom gold-ratio-scroll-screen-up-hook nil
  ""
  :type 'hook)
(defcustom gold-ratio-scroll-screen-down-hook nil
  ""
  :type 'hook)

(defface gold-ratio-scroll-highlight-line-face
  '((t (:background "cadetblue4" :foreground "white" :weight bold)))
  "Font Lock mode face used to highlight line.
 (borrowed from etags-select.el)"
  :group 'gold-ratio-scroll-screen)

(defun gold-ratio-scroll-highlight (beg end delay)
  "Highlight a region temporarily.
  (borrowed from etags-select.el)"
  (if (featurep 'xemacs)
      (let ((extent (make-extent beg end)))
        (set-extent-property extent 'face
                             'gold-ratio-scroll-highlight-line-face)
        (sit-for delay)
        (delete-extent extent))
    (let ((ov (make-overlay beg end)))
      (overlay-put ov 'face 'gold-ratio-scroll-highlight-line-face)
      (sit-for delay)
      (delete-overlay ov))))

(defvar gold-ratio-scroll-screen-previous-point (point-marker))

;;;###autoload
(defun gold-ratio-scroll-screen-down()
  "scroll half screen down"
  (interactive)
  (let ((old-marker gold-ratio-scroll-screen-previous-point)
        (bol-before-jump (point-at-bol))
        (eol-before-jump (1+ (point-at-eol)))
        (scroll-line-cnt (round (/ (frame-height) gold-ratio-scroll-screen-ratio))))
    (setq gold-ratio-scroll-screen-previous-point (point-marker))
    (if (and (not (and (equal (current-buffer) (marker-buffer old-marker))
                       (equal (marker-position old-marker) (point))))
             (equal last-command 'gold-ratio-scroll-screen-up))
        (goto-char (marker-position old-marker))
      (forward-visible-line scroll-line-cnt))
    (when (and (member major-mode '(dired-mode wdired-mode))
               (equal (point-max) (point)))
      (dired-previous-line 1))
    (when gold-ratio-scroll-recenter
      (recenter (+ scroll-line-cnt (/ (- (frame-height) scroll-line-cnt) 2))))
    (when (member gold-ratio-scroll-highlight-flag '(before both))
      (gold-ratio-scroll-highlight
       bol-before-jump eol-before-jump
       (car gold-ratio-scroll-highlight-delay)))
    (when (member gold-ratio-scroll-highlight-flag '(after both))
      (gold-ratio-scroll-highlight
       (point-at-bol) (1+ (point-at-eol))
       (cdr gold-ratio-scroll-highlight-delay)))
    (run-hooks 'gold-ratio-scroll-screen-down-hook)))

;;;###autoload
(defun gold-ratio-scroll-screen-up()
  "scroll half screen up"
  (interactive)
  (let ((old-marker gold-ratio-scroll-screen-previous-point)
        (bol-before-jump (point-at-bol))
        (eol-before-jump (1+ (point-at-eol)))
        (scroll-line-cnt (round (/ (frame-height) gold-ratio-scroll-screen-ratio))))
    (setq gold-ratio-scroll-screen-previous-point (point-marker))
    (if (and  (not (and (equal (current-buffer) (marker-buffer old-marker))
                        (equal (marker-position old-marker) (point))))
              (equal last-command 'gold-ratio-scroll-screen-down))
        (goto-char (marker-position old-marker))
      (forward-visible-line (- 0 scroll-line-cnt)))
    (when (and (member major-mode '(dired-mode wdired-mode))
               (equal (point-min) (point)))
      (dired-next-line 2))
    (when gold-ratio-scroll-recenter
      (recenter (/ (- (frame-height) scroll-line-cnt) 2)))
    (when (member gold-ratio-scroll-highlight-flag '(before both))
      (gold-ratio-scroll-highlight
       bol-before-jump eol-before-jump
       (car gold-ratio-scroll-highlight-delay)))
    (when (member gold-ratio-scroll-highlight-flag '(after both))
      (gold-ratio-scroll-highlight
       (point-at-bol) (1+ (point-at-eol))
       (cdr gold-ratio-scroll-highlight-delay))))
  (run-hooks 'gold-ratio-scroll-screen-up-hook))

(put 'gold-ratio-scroll-screen-up 'scroll-command t)
(put 'gold-ratio-scroll-screen-down 'scroll-command t)

(provide 'gold-ratio-scroll-screen)
;; gold-ratio-scroll-screen.el ends here

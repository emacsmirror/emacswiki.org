;;; scroll-mode-line-mode.el --- 

;; Copyright 2006 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Time-stamp: <Ye Wenbin 2006-10-15 19:43:36>
;; Version: $Id: scroll-mode-line-mode.el,v 0.0 <2006-10-15 15:33:10> ywb Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'scroll-mode-line-mode)
;;   (scroll-mode-line-mode 1)
;; If you want change scroll-mode-line-format and mode-line-format in
;; .emacs, change it before write (scroll-mode-line-mode 1)

;;; Code:

(provide 'scroll-mode-line-mode)
(eval-when-compile
  (require 'cl))

(defvar scroll-mode-line-interval 1)
(defvar scroll-mode-line-step 1 "")
(defvar scroll-mode-line-timer nil)
(defvar scroll-mode-line-offset 0)
(defvar scroll-mode-line-format nil "")
(put 'scroll-mode-line-format 'risky-local-variable t)
(defvar scroll-mode-line-string nil "")
(put 'scroll-mode-line-string 'risky-local-variable t)

(defun scroll-mode-line-handler ()
  ""
  (let ((modestr (format-mode-line scroll-mode-line-format)))
    (setq scroll-mode-line-offset
          (+ scroll-mode-line-offset
             scroll-mode-line-step))
    (if (> scroll-mode-line-offset (length modestr))
        (setq scroll-mode-line-offset 0))
    (setq scroll-mode-line-string
          (format "- %s %s -"
                  (substring modestr scroll-mode-line-offset)
                  (substring modestr 0 scroll-mode-line-offset)))
    (force-mode-line-update)))

(define-minor-mode scroll-mode-line-mode
  "If enabled, the mode line will scroll from left to right"
  :global t
  (if scroll-mode-line-timer
      (cancel-timer scroll-mode-line-timer))
  (let (from to format)
    (if scroll-mode-line-mode
        (setq from 'scroll-mode-line-format
              to 'scroll-mode-line-string)
      (setq from 'scroll-mode-line-string
            to 'scroll-mode-line-format))
    (dolist (elm mode-line-format)
      (setq format (cons (if (eq elm from)
                             to elm)
                         format)))
    (setq-default mode-line-format
                  (nreverse format)))
  (setq scroll-mode-line-offset 0)
  (if scroll-mode-line-mode
      (setq scroll-mode-line-timer
            (run-at-time nil scroll-mode-line-interval
                         'scroll-mode-line-handler))))

;;; use it like this
(unless scroll-mode-line-format
  (setq scroll-mode-line-format
        '("" global-mode-string))
  (setq-default mode-line-format 
                '(#("-" 0 1
                    (auto-composed t help-echo "mouse-1: select (drag to resize), mouse-2: delete others, mouse-3: delete this"))
                  mode-line-mule-info mode-line-modified mode-line-frame-identification
                  mode-line-buffer-identification
                  mode-line-position
                  (vc-mode vc-mode)
                  #("   " 0 3
                    (auto-composed t help-echo "mouse-1: select (drag to resize), mouse-2: delete others, mouse-3: delete this"))
                  mode-line-modes
                  (which-func-mode
                   ("" which-func-format
                    #("--" 0 2
                      (help-echo "mouse-1: select (drag to resize), mouse-2: delete others, mouse-3: delete this"))))
                  scroll-mode-line-format)))

;;; scroll-mode-line-mode.el ends here

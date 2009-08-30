;;; multi-scratch.el --- Multiple scratches manager

;; Filename: multi-scratch.el
;; Description: Multiple scratches manager
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-01-25 01:00:38
;; Version: 0.2
;; Last-Updated: 2009-03-13 16:43:56
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/multi-scratch.el
;; Keywords:
;; Compatibility: GNU Emacs 22 ~ 23
;;
;; Features that might be required by this library:
;;
;; `cl'
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
;; Multiple scratches manager.
;;
;; Below are commands you can use:
;;
;;      `multi-scratch-new'     Create new scratch buffer.
;;      `multi-scratch-next'    Switch to next scratch buffer.
;;      `multi-scratch-prev'    Switch to previous scratch buffer.
;;

;;; Installation:
;;
;; Put multi-scratch.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'multi-scratch)
;;
;; No need more.

;;; Customize:
;;
;; `multi-scratch-buffer-name' The name of scratch buffer.
;; `multi-scratch-try-create' Try to create new scratch buffer
;; when no scratch buffers exist.
;;
;; All of the above can customize by:
;;      M-x customize-group RET multi-scratch RET
;;

;;; Change log:
;;
;; 2009/03/13
;;      * If type `C-u' before command `multi-scratch-new'
;;        don't load `lisp-interaction' mode.
;;
;; 2009/01/25
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
(require 'cl)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup multi-scratch nil
  "Multiple scratch manager."
  :group 'lisp)

(defcustom multi-scratch-buffer-name "multi-scratch"
  "The name of scratch buffer."
  :type 'string
  :group 'multi-scratch)

(defcustom multi-scratch-try-create t
  "Try to create a new scratch buffer when switch.

When use `multi-scratch-next' or `multi-scratch-prev',
switch scratch buffer, and try to create a new
scratch buffer if no scratch buffers exist."
  :type 'boolean
  :group 'multi-scratch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun multi-scratch-new (&optional prefix)
  "Create a new multi-scratch buffer.
Load `lisp-interaction' mode when PREFIX is nil."
  (interactive)
  ;; Set prefix if prefix is nil.
  (or prefix (setq prefix current-prefix-arg))
  ;; Create new scratch.
  (let* ((scratch-buffer (multi-scratch-get-buffer)))
    (set-buffer scratch-buffer)
    ;; Load `lisp-interaction' mode when prefix is nil.
    (unless prefix
      (lisp-interaction-mode))
    ;; Switch scratch buffer
    (switch-to-buffer scratch-buffer)))

(defun multi-scratch-next (&optional offset)
  "Switch to next scratch buffer.
If OFFSET is `non-nil', will switch next OFFSET scratch buffer."
  (interactive "P")
  (multi-scratch-switch 'NEXT (or offset 1)))

(defun multi-scratch-prev (&optional offset)
  "Switch to previous scratch buffer.
If OFFSET is `non-nil', will switch previous OFFSET scratch buffer."
  (interactive "P")
  (multi-scratch-switch 'PREVIOUS (or offset 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun multi-scratch-get-buffer ()
  "Get scratch buffer."
  (let* ((scratch-list-length (length (multi-scratch-list)))          ;get length of scratch list
         (index (if scratch-list-length (1+ scratch-list-length) 1))) ;setup new scratch index
    (with-temp-buffer
      ;; Return buffer
      (get-buffer-create (format "*%s<%s>*" multi-scratch-buffer-name index)))))

(defun multi-scratch-list ()
  "The scratch buffers presently active."
  ;; Autload command `remove-if-not'.
  (autoload 'remove-if-not "cl-seq")
  (sort
   (remove-if-not (lambda (b)
                    (string-match
                     (concat "^\*" multi-scratch-buffer-name)
                     (buffer-name b)))
                  (buffer-list))
   (lambda (a b)
     (< (string-to-number
         (cadr (split-string (buffer-name a) "[<>]")))
        (string-to-number
         (cadr (split-string (buffer-name b)  "[<>]")))))))

(defun multi-scratch-switch (direction offset)
  "Switch to scratch buffer.

If DIRECTION is `NEXT', switch to next scratch buffer.
If DIRECTION is `PREVIOUS', switch to previous scratch buffer.

Default OFFSET is 1.

If option `multi-scratch-try-create' is non-nil,
will create a new scratch buffer
if have any scratch buffer exist."
  (let (scratchs this-buffer)
    (setq scratchs (multi-scratch-list))
    (if (consp scratchs)
        (progn
          (setf (cdr (last scratchs)) scratchs)
          (setq this-buffer (position (current-buffer) (multi-scratch-list)))
          (if this-buffer
              (if (eql direction 'NEXT)
                  (switch-to-buffer (nth (+ this-buffer offset) scratchs))
                (switch-to-buffer (nth (+ (- (length (multi-scratch-list)) offset)
                                          this-buffer) scratchs)))
            (switch-to-buffer (car scratchs))))
      (if multi-scratch-try-create
          (progn
            (multi-scratch-new)
            (message "Create a new `multi-scratch' buffer."))
        (message "Haven't any `multi-scratch' buffer exist.")))))

(provide 'multi-scratch)

;;; multi-scratch.el ends here


;;; LocalWords:  multi scratchs

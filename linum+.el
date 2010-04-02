;;; linum+.el --- Extension of linum

;; Copyright (C) 2010 ahei

;; Author: ahei <ahei0802@gmail.com>
;; Keywords: line number
;; URL: http://www.emacswiki.org/emacs/download/multi-term.el
;; Time-stamp: <2010-04-02 15:28:01 Friday by ahei>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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
;; This extension of linum can smart control width of line number displayed, If
;; line number can be viewed of current buffer is from 1 to 50, then width of
;; line number is 2, and line number can be viewed of current buffer is from 100
;; to 150, then width of line number is 3. and use it, you can customize line
;; number format with linum+-dynamic-format even if linum-format is 'dynamic.
;; For more details, see article Use linum+ smart display line number
;; http://emacser.com/linum-plus.htm

;;; Installation:
;;
;; Copy linum+.el to your load-path and add to your .emacs:
;;
;; (require 'linum+)
;;
;; then use M-x linum-mode to turn on line number displaying

;;; History:
;;
;; 2010-4-1
;;      * initial version 1.0.

;;; Code:

(require 'linum)

(defgroup linum+ nil
  "Extension of `linum-mode'."
  :prefix "linum+-")

;;;###autoload
(defcustom linum+-dynamic-format "%%%dd|"
  "Format used when `linum-format' is dynamic."
  :group 'linum+
  :type 'sexp)

(defun linum-update-window (win)
  "Update line numbers for the portion visible in window WIN."
  (goto-char (window-start win))
  (let* ((line (line-number-at-pos))
         (limit (window-end win t))
         (fmt (cond ((stringp linum-format) linum-format)
                    ((eq linum-format 'dynamic)
                     ;; get format of line number by max line number can be viewed in current buffer
                     (let ((w (length (number-to-string (line-number-at-pos limit)))))
                       (format linum+-dynamic-format w)))))
         (width 0))
    (run-hooks 'linum-before-numbering-hook)
    ;; Create an overlay (or reuse an existing one) for each
    ;; line visible in this window, if necessary.
    (while (and (not (eobp)) (<= (point) limit))
      (let* ((str (if fmt
                      (propertize (format fmt line) 'face 'linum)
                    (funcall linum-format line)))
             (visited (catch 'visited
                        (dolist (o (overlays-in (point) (point)))
                          (when (equal-including-properties
                                 (overlay-get o 'linum-str) str)
                            (unless (memq o linum-overlays)
                              (push o linum-overlays))
                            (setq linum-available (delq o linum-available))
                            (throw 'visited t))))))
        (setq width (max width (length str)))
        (unless visited
          (let ((ov (if (null linum-available)
                        (make-overlay (point) (point))
                      (move-overlay (pop linum-available) (point) (point)))))
            (push ov linum-overlays)
            (overlay-put ov 'before-string
                         (propertize " " 'display `((margin left-margin) ,str)))
            (overlay-put ov 'linum-str str))))
      (forward-line)
      (setq line (1+ line)))
    (set-window-margins win width)))

(provide 'linum+)

;;; linum+.el ends here

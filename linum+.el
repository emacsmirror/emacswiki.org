;;; linum+.el --- Extension of linum

;; Copyright (C) 2010 ahei

;; Author: ahei <ahei0802@gmail.com>
;; Keywords: line number
;; URL: http://code.google.com/p/dea/source/browse/trunk/my-lisps/linum%2B.el
;; Time-stamp: <2010-04-08 10:55:43 Thursday by ahei>

;; This  file is free  software; you  can redistribute  it and/or
;; modify it under the terms of the GNU General Public License as
;; published by  the Free Software Foundation;  either version 3,
;; or (at your option) any later version.

;; This file is  distributed in the hope that  it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR  A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You  should have  received a  copy of  the GNU  General Public
;; License along with  GNU Emacs; see the file  COPYING.  If not,
;; write  to  the Free  Software  Foundation,  Inc., 51  Franklin
;; Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This extension of linum can smart control width of line number
;; displayed, If visible line number  of current buffer is from 1
;; to 50, then width of line number is 2, and visible line number
;; of  current buffer  is from  100 to  150, then  width  of line
;; number is 3. and use  it, you can customize line number format
;; with linum+-dynamic-format  even if linum-format  is 'dynamic.
;; Its                       screenshots                       is
;; http://emacser.com/screenshots/linum+/linum+1.png           and
;; http://emacser.com/screenshots/linum+/linum+2.png  .  For more
;; details,  see article  Use  linum+ smart  display line  number
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
;; 2010-04-04
;;      * Add more format control for linum-format
;; 2010-4-1
;;      * initial version 1.0.

;;; Code:

(require 'linum)

(defgroup linum+ nil
  "Extension of `linum-mode'."
  :prefix "linum+-")

;;;###autoload
(defcustom linum-format 'smart
  "Format used to display line numbers.

+ Either a format string like \"%7d\",
+ or `smart' to smart adapt the width by current max visible line number.
+ or `dynamic' to adapt the width as needed,
+ or a vector with one string element which uesed to generate
  line number format by `format' with argument max visible line number 
  of current buffer, see example `linum+-smart-format'
+ or a list with one string element which uesed to generate
  line number format by `format' with argument max line number of current buffer,
  see example `linum+-dynamic-format'
+ or a function that is called with a line number as its
  argument and should evaluate to a string to be shown on that line.

See also `linum-before-numbering-hook'."
  :group 'linum
  :type 'sexp)

(setq linum-format 'smart)

;;;###autoload
(defcustom linum+-dynamic-format "%%%dd|"
  "Format used to generate line number format when `linum-format' is `dynamic'."
  :group 'linum+
  :type 'sexp)

;;;###autoload
(defcustom linum+-smart-format "%%%dd|"
  "Format used to generate line number format when `linum-format' is `smart'."
  :group 'linum+
  :type 'sexp)

;;;###autoload
(defun linum+-generate-linum-format (format-type limit)
  "Generate line number format by FORMAT-TYPE, LIMIT is `window-end' of win."
  (cond ((stringp format-type) format-type)
        ((or (listp format-type) (vectorp format-type)
             (eq format-type 'dynamic) (eq format-type 'smart))
         (let* ((dynamic-width (or (vectorp format-type) (eq format-type 'smart)))
                (old-format
                 (if (eq format-type 'dynamic)
                     linum+-dynamic-format
                   (if (eq format-type 'smart)
                       linum+-smart-format
                     format-type)))
                (w (length
                    (number-to-string
                     (line-number-at-pos (if dynamic-width limit (point-max))))))
                (new-format
                 (if (listp old-format)
                     (car old-format)
                   (if (vectorp old-format)
                       (aref old-format 0)
                     old-format))))
           (format new-format w)))))

;;;###autoload
(defun linum-update-window (win)
  "Update line numbers for the portion visible in window WIN."
  (goto-char (window-start win))
  (let* ((line (line-number-at-pos))
         (limit (window-end win t))
         (fmt (linum+-generate-linum-format linum-format limit))
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

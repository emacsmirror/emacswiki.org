;;; hide-comnt.el --- Hide/show comments in code.
;;
;; Filename: hide-comnt.el
;; Description: Hide/show comments in code.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2011-2012, Drew Adams, all rights reserved.
;; Created: Wed May 11 07:11:30 2011 (-0700)
;; Version:
;; Last-Updated: Sun Jan  1 16:00:08 2012 (-0800)
;;           By: dradams
;;     Update #: 19
;; URL: http://www.emacswiki.org/cgi-bin/wiki/hide-comnt.el
;; Keywords: comment, hide, show
;; Compatibility: GNU Emacs: 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Hide/show comments in code.
;;
;;
;;  Macros defined here:
;;
;;    `with-comments-hidden'.
;;
;;  Commands defined here:
;;
;;    `hide/show-comments',
;;
;;  User options defined here:
;;
;;    `ignore-comments-flag'.
;;
;;
;;  Put this in your init file (`~/.emacs'):
;;
;;   (require 'hide-comnt)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2011/11/23 dadams
;;     hide/show-comments: Bug fix - ensure CEND is not past eob.
;; 2011/05/11 dadams
;;     Created: moved code here from thing-cmds.el.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


;;;###autoload
(defcustom ignore-comments-flag t
  "Non-nil means macro `with-comments-hidden' hides comments."
  :type 'boolean :group 'matching)


(defmacro with-comments-hidden (start end &rest body)
  "Evaluate the forms in BODY while comments are hidden from START to END.
But if `ignore-comments-flag' is nil, just evaluate BODY,
without hiding comments.  Show comments again when BODY is finished.

See `hide/show-comments', which is used to hide and show the comments.
Note that prior to Emacs 21, this never hides comments."
  (let ((result  (make-symbol "result"))
        (ostart  (make-symbol "ostart"))
        (oend    (make-symbol "oend")))
    `(let ((,ostart  ,start)
           (,oend    ,end)
           ,result)
      (unwind-protect
           (setq ,result  (progn (when ignore-comments-flag
                                   (hide/show-comments 'hide ,ostart ,oend))
                                 ,@body))
        (when ignore-comments-flag (hide/show-comments 'show ,ostart ,oend))
        ,result))))

;;;###autoload
(defun hide/show-comments (&optional hide/show start end)
  "Hide or show comments from START to END.
Interactively, hide comments, or show them if you use a prefix arg.
Interactively, START and END default to the region limits, if active.
Otherwise, including non-interactively, they default to `point-min'
and `point-max'.

Uses `save-excursion', restoring point.

Be aware that using this command to show invisible text shows *all*
such text, regardless of how it was hidden.  IOW, it does not just
show invisible text that you previously hid using this command.

From Lisp, a HIDE/SHOW value of `hide' hides comments.  Other values
show them.

This function does nothing in Emacs versions prior to Emacs 21,
because it needs `comment-search-forward'."
  (interactive
   (cons (if current-prefix-arg 'show 'hide)
         (if (or (not mark-active) (null (mark)) (= (point) (mark)))
             (list (point-min) (point-max))
           (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point))))))
  (when (require 'newcomment nil t)     ; `comment-search-forward', Emacs 21+.
    (unless start (setq start  (point-min)))
    (unless end   (setq end    (point-max)))
    (unless (<= start end) (setq start  (prog1 end (setq end  start))))
    (let ((bufmodp           (buffer-modified-p))
          (buffer-read-only  nil)
          cbeg cend)
      (unwind-protect
           (save-excursion
             (goto-char start)
             (while (and (< start end) (setq cbeg  (comment-search-forward end 'NOERROR)))
               (setq cend  (if (string= "" comment-end)
                               (min (1+ (line-end-position)) (point-max))
                             (search-forward comment-end end 'NOERROR)))
               (when (and cbeg cend)
                 (if (eq 'hide hide/show)
                     (put-text-property cbeg cend 'invisible t)
                   (put-text-property cbeg cend 'invisible nil)))))
        (set-buffer-modified-p bufmodp)))))

;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'hide-comnt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hide-comnt.el ends here

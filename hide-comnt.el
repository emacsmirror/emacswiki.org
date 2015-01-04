;;; hide-comnt.el --- Hide/show comments in code.
;;
;; Filename: hide-comnt.el
;; Description: Hide/show comments in code.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2011-2015, Drew Adams, all rights reserved.
;; Created: Wed May 11 07:11:30 2011 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Thu Jan  1 10:51:15 2015 (-0800)
;;           By: dradams
;;     Update #: 168
;; URL: http://www.emacswiki.org/hide-comnt.el
;; Doc URL: http://www.emacswiki.org/HideOrIgnoreComments
;; Keywords: comment, hide, show
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
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
;;    `hide/show-comments', `hide/show-comments-toggle'.
;;
;;  User options defined here:
;;
;;    `hide-whitespace-before-comment-flag', `ignore-comments-flag'.
;;
;;
;;  Put this in your init file (`~/.emacs'):
;;
;;   (require 'hide-comnt)
;;
;;
;;  Note for Emacs 20: The commands and option defined here DO NOTHING
;;  IN EMACS 20.  Nevertheless, the library can be byte-compiled in
;;  Emacs 20 and `hide-comnt.elc' can be loaded in later Emacs
;;  versions and used there.  This is the only real use of this
;;  library for Emacs 20: it provides macro `with-comments-hidden'.
;;
;;  Note for Emacs 21: It lacks the `comment-forward' function, so we
;;  rely on the `comment-end' variable to determine the end of a
;;  comment. This means that only one type of comment terminator is
;;  supported.  For example, `c++-mode' sets `comment-end' to "",
;;  which is the convention for single-line comments ("// COMMENT").
;;  So "/* */" comments are treated as single-line commentsonly the
;;  first line of such comments is hidden.  The "*/" terminator is not
;;  taken into account.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014/11/05 dadams
;;     hide/show-comments: Thx to Hinrik Sigurosson.
;;       Use comment-forward even for "", so handle setting CEND correctly, e.g., for C++,
;;       where comment-end is "" but multi-line comments are also OK.
;;       Do not hide newline after single-line comments.
;;       hide-whitespace-before-comment-flag non-nil no longer hides empty lines.
;;       Prevent infloop for comment at bol.
;; 2014/02/06 dadams
;;     Added: hide-whitespace-before-comment-flag.
;;     hide/show-comments:
;;       Go to start of comment before calling comment-forward.
;;       Hide whitespace preceding comment, if hide-whitespace-before-comment-flag.
;; 2013/12/26 dadams
;;     hide/show-comments: Update START to comment end or END.
;; 2013/10/09 dadams
;;     hide/show-comments: Use save-excursion.  If empty comment-end go to CBEG.
;;                         Use comment-forward if available.
;; 2012/10/06 dadams
;;     hide/show-comments: Call comment-normalize-vars first.  Thx to Stefan Monnier.
;;     hide/show-comments-toggle: Do nothing if newcomment.el not available.
;; 2012/05/10 dadams
;;     Added: hide/show-comments-toggle.  Thx to Denny Zhang for the suggestion.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defcustom ignore-comments-flag t
  "*Non-nil means macro `with-comments-hidden' hides comments."
  :type 'boolean :group 'matching)

;;;###autoload
(defcustom hide-whitespace-before-comment-flag t
  "*Non-nil means `hide/show-comments' hides whitespace preceding a comment.
It does not hide empty lines (newline chars), however."
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
      (unwind-protect (setq ,result  (progn (when ignore-comments-flag
                                              (hide/show-comments 'hide ,ostart ,oend))
                                            ,@body))
        (when ignore-comments-flag (hide/show-comments 'show ,ostart ,oend))
        ,result))))

;;;###autoload
(defun hide/show-comments (&optional hide/show start end)
  "Hide or show comments from START to END.
Interactively, hide comments, or show them if you use a prefix arg.
\(This is thus *NOT* a toggle command.)

If option `hide-whitespace-before-comment-flag' is non-nil, then hide
also any whitespace preceding a comment.

Interactively, START and END default to the region limits, if active.
Otherwise, including non-interactively, they default to `point-min'
and `point-max'.

Uses `save-excursion', restoring point.

Be aware that using this command to show invisible text shows *ALL*
such text, regardless of how it was hidden.  IOW, it does not just
show invisible text that you previously hid using this command.

From Lisp, a HIDE/SHOW value of `hide' hides comments.  Other values
show them.

This command does nothing in Emacs versions prior to Emacs 21, because
it needs `comment-search-forward'."
  (interactive
   (cons (if current-prefix-arg 'show 'hide)
         (if (or (not mark-active)  (null (mark))  (= (point) (mark)))
             (list (point-min) (point-max))
           (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point))))))
  (when (require 'newcomment nil t)     ; `comment-search-forward', Emacs 21+.
    (comment-normalize-vars)            ; Must call this first.
    (unless start (setq start  (point-min)))
    (unless end   (setq end    (point-max)))
    (unless (<= start end) (setq start  (prog1 end (setq end  start)))) ; Swap.
    (let ((bufmodp           (buffer-modified-p))
          (buffer-read-only  nil)
          cbeg cend)
      (unwind-protect
           (save-excursion
             (goto-char start)
             (while (and (< start end)
                         (save-excursion
                           (setq cbeg  (comment-search-forward end 'NOERROR))))
               (goto-char cbeg)
               (save-excursion
                 (setq cend  (cond ((fboundp 'comment-forward) ; Emacs 22+
                                    (if (comment-forward 1)
                                        (if (= (char-before) ?\n) (1- (point)) (point))
                                      end))
                                   ((string= "" comment-end) (min (line-end-position) end))
                                   (t (search-forward comment-end end 'NOERROR)))))
               (when hide-whitespace-before-comment-flag ; Hide preceding whitespace.
                 (save-excursion
                   (if (fboundp 'looking-back) ; Emacs 22+
                       (when (looking-back "\n?\\s-*" nil 'GREEDY)
                         (setq cbeg  (match-beginning 0)))
                     (while (memq (char-before cbeg) '(?\   ?\t ?\f))
                       (setq cbeg  (1- cbeg)))
                     (when (eq (char-before cbeg) ?\n) (setq cbeg  (1- cbeg)))))
                 ;; First line: Hide newline after comment.
                 (when (and (= cbeg (point-min))  (= (char-after cend) ?\n))
                   (setq cend  (min (1+ cend) end))))
               (when (and cbeg  cend)
                 (put-text-property cbeg cend 'invisible (eq 'hide hide/show)))
               (goto-char (setq start  (or cend  end)))))
        (set-buffer-modified-p bufmodp)))))

(defun hide/show-comments-toggle (&optional start end)
  "Toggle hiding/showing of comments in the active region or whole buffer.
If the region is active then toggle in the region.  Otherwise, in the
whole buffer.

This command does nothing in Emacs versions prior to Emacs 21, because
it needs `comment-search-forward'.

Interactively, START and END default to the region limits, if active.
Otherwise, including non-interactively, they default to `point-min'
and `point-max'.

See `hide/show-comments' for more information."
  (interactive (if (or (not mark-active)  (null (mark))  (= (point) (mark)))
                   (list (point-min) (point-max))
                 (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point)))))
  (when (require 'newcomment nil t) ; `comment-search-forward', Emacs 21+.
    (comment-normalize-vars)     ; Must call this first.
    (if (save-excursion (goto-char start) (and (comment-search-forward end 'NOERROR)
                                               (get-text-property (point) 'invisible)))
        (hide/show-comments 'show start end)
      (hide/show-comments 'hide start end))))

;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'hide-comnt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hide-comnt.el ends here

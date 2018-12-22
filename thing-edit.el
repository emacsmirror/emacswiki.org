;;; thing-edit.el --- Extension thing edit

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Copyright (C) 2014, Arthur Miller <arthur.miller@live.com>, all rights reserved.
;; Created: 2008-06-08 00:42:07
;; Version: 1.3
;; Last-Updated: 2018-12-22 12:48:56
;; URL: http://www.emacswiki.org/emacs/download/thing-edit.el
;; Keywords: thingatpt, edit
;; Compatibility: GNU Emacs 23.0.60.1

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

;; Features that might be required by this library:
;;
;;      `thingatpt'
;;

;;; Commentary:
;;
;; This package is some useful functions that base on `thingatpt.el'.
;; Those function can copy or paste special data object quickly
;; and don't need to move cursor.
;; Just binding your like keystroke to those functions.
;;
;; thing-paste-sexp                paste regular expression around cursor.
;; thing-copy-sexp                 copy regular expression around cursor.
;; thing-replace-sexp              replace regular expression around cursor with content of kill-ring.
;;
;; thing-paste-email               paste email string around cursor
;; thing-copy-email                copy email string around cursor.
;; thing-replace-email             replace email string around cursor with content of kill-ring.
;;
;; thing-paste-filename            paste filename string around cursor.
;; thing-copy-filename             copy filename string around cursor.
;; thing-replace-filename          replace filename string around cursor with content of kill-ring.
;;
;; thing-paste-url                 paste url string around cursor.
;; thing-copy-url                  copy url string around cursor.
;; thing-replace-url               replace url string around cursor with content of kill-ring.
;;
;; thing-paste-word                paste word string around cursor.
;; thing-copy-word                 copy word string around cursor.
;; thing-replace-word              replace word string around cursor with content of kill-ring.
;;
;; thing-paste-symbol              paste symbol string around cursor.
;; thing-copy-symbol               copy symbol string around cursor.
;; thing-replace-symbol            replace symbol string around cursor with content of kill-ring.
;;
;; thing-paste-defun               paste function string around cursor.
;; thing-copy-defun                copy function string around cursor.
;; thing-replace-defun             replace function string around cursor with content of kill-ring.
;;
;; thing-paste-list                paste list string around cursor.
;; thing-copy-list                 copy list string around cursor.
;; thing-replace-list              replace list string around cursor with content of kill-ring.
;;
;; thing-paste-sentence            paste sentence string around cursor.
;; thing-copy-sentence             copy sentence string around cursor.
;; thing-replace-sentence          replace sentence string around cursor with content of kill-ring.
;;
;; thing-paste-whitespace          paste whitespace string around cursor.
;; thing-copy-whitespace           copy whitespace string around cursor.
;; thing-replace-whitespace        replace whitespace string around cursor with content of kill-ring.
;;
;; thing-paste-page                paste page string around cursor.
;; thing-copy-page                 copy page string around cursor.
;; thing-replace-page              replace page string around cursor with content of kill-ring.
;;
;; thing-paste-line                paste current line.
;; thing-copy-line                 copy current line.
;; thing-replace-line              replace current line with content of kill-ring.
;;
;; thing-paste-to-line-end         paste string to end of line.
;; thing-copy-to-line-end          copy string to end of line.
;; thing-replace-to-line-end       replace string to end of line with content of kill-ring.
;;
;; thing-paste-to-line-beginning   paste string to beginning of line.
;; thing-copy-to-line-beginning    copy string to beginning of line.
;; thing-replace-to-line-beginning replace string to beginning of line with content of kill-ring.
;;
;; thing-paste-comment             paste comment.
;; thing-copy-comment              copy comment.
;; thing-replace-comment           replace comment with content of kill-ring.
;;
;; thing-paste-paragrap            paste paragraph around cursor.
;; thing-copy-paragrap             copy paragraph around cursor.
;; thing-replace-paragrap          replace paragraph around cursor with content of kill-ring.
;;
;; thing-paste-parentheses         paste parentheses around cursor.
;; thing-copy-parentheses          copy parentheses around cursor.
;; thing-replace-parentheses       replace parentheses around cursor with content of kill-ring.
;;

;;; Installation:
;;
;; Copy thing-edit.el to your load-path and add to your ~/.emacs
;;
;;      (require 'thing-edit)
;;
;; No more need

;;; Change log:
;;
;; 2018/12/22
;;      * Add docs.
;;
;; 2018/12/20
;;      * Add `thing-replace-xxx' function. These functions can replace current thing with the content of `kill-ring'.
;;
;; 2014/04/09
;;      * Merge Arthur's new functions `thing-copy-paragraph' and `thing-paste-paragraph', thanks!
;;      * Merge Arthur's autoload patch, thanks a lot!
;;
;; 2009/01/13
;;      * Add many functions.
;;
;; 2009/01/09
;;      * Move functions `thing-paste-parentheses' and `thing-copy-parentheses'
;;        to file `thing-edit-extension.el', avoid this package depend `paredit'.
;;
;; 2008/09/26
;;      * Rebuild program framework, make code more clearly.
;;
;; 2008/07/28
;;      * Modified function `lazy-search-mark-object' use paredit's function to
;;        select content between parenthesis.
;;
;; 2008/06/19
;;      * Modified search method of `thing-copy-parentheses' and `thing-paste-parentheses'.
;;
;; 2008/06/08
;;      * Add edit of `url', `filename', `email', `sexp'
;;      * And ignore `defun', `page', `whitespace', `list' `sentence'.
;;      * Add basal function that apply thingatpt library.
;;      * Complete that `word', `symbol', `line', `line-to-beg', `line-to-end'
;;      * Comment is not use `thingatpt', but operate is allied.
;;

;;; Acknowledgments:
;;
;; Not yet
;;

;;; TODO
;;
;; None
;;

;;; Require
(require 'thingatpt)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup thing-edit nil
  "Thing edit."
  :group 'term)

(defcustom thing-edit-show-message-p t
  "Set this option to nil if want thing-edit work silencely.
Default is nil."
  :type 'boolean
  :group 'thing-edit)

(defun thing-edit-internal (object-beg object-end &optional kill-conditional)
  "A fast edit complexes object.
Argument OBJECT-BEG the begin position that object.
Argument OBJECT-END the end position of object.
Optional argument KILL-CONDITIONAL default is do copy handle, if KILL-CONDITIONAL is non-nil do paste handle."
  (interactive)
  (if kill-conditional
      (progn
        (if thing-edit-show-message-p
            (message "%s pasted." (buffer-substring object-beg object-end)))
        (kill-region object-beg object-end))
    (if thing-edit-show-message-p
        (message "%s copied." (buffer-substring object-beg object-end)))
    (kill-ring-save object-beg object-end)))

(defun thing-edit (thing &optional kill-conditional)
  "This function is a simple interface for `thing-edit-internal'.
If `KILL-CONDITIONAL' is non-nil, kill object,
otherwise copy object."
  (save-excursion
    (thing-edit-internal (beginning-of-thing thing)
                         (end-of-thing thing)
                         kill-conditional)))

(defun thing-replace-internal (object-beg object-end)
  "A fast replace complexes object.
Argument OBJECT-BEG the begin position that object.
Argument OBJECT-END the end position of object."
  (interactive)
  (progn
    (goto-char object-beg)
    (delete-char (- object-end object-beg))
    (yank)))

(defun thing-replace (thing)
  "This function is a simple interface for `thing-replace-internal'"
  (save-excursion
    (thing-replace-internal (beginning-of-thing thing)
                          (end-of-thing thing))))

;;;###autoload
(defun thing-paste-sexp ()
  "Paste regular expression at current point."
  (interactive)
  (thing-edit 'sexp t))

;;;###autoload
(defun thing-copy-sexp (kill-conditional)
  "Copy regular expression at current point.
With the universal argument, the text will also be killed."
  (interactive "P")
  (if kill-conditional
      (thing-edit 'sexp t)
    (thing-edit 'sexp)))

;;;###autoload
(defun thing-replace-sexp ()
  "Replace regular expression at current point with the content of kill-ring."
  (interactive)
  (thing-replace 'sexp))

;;;###autoload
(defun thing-paste-email ()
  "Paste email at current point."
  (interactive)
  (thing-edit 'email t))

;;;###autoload
(defun thing-copy-email (kill-conditional)
  "Copy email at current point.
With the universal argument, the text will also be killed"
  (interactive "P")
  (if kill-conditional
      (thing-edit 'email t)
    (thing-edit 'email)))

;;;###autoload
(defun thing-replace-email ()
  "Replace email at current point with the content kill ring."
  (interactive)
  (thing-replace 'email))

;;;###autoload
(defun thing-paste-filename ()
  "Paste filename at current point."
  (interactive)
  (thing-edit 'filename t))

;;;###autoload
(defun thing-copy-filename (kill-conditional)
  "Copy filename at current point.
With the universal argument, the text will also be killed"
  (interactive "P")
  (if kill-conditional
      (thing-edit 'filename t)
    (thing-edit 'filename)))

;;;###autoload
(defun thing-replace-filename ()
  "Replace filename at current point with kill ring."
  (interactive)
  (thing-replace 'filename))

;;;###autoload
(defun thing-paste-url ()
  "Paste url at current point."
  (interactive)
  (thing-edit 'url t))

;;;###autoload
(defun thing-copy-url (kill-conditional)
  "Copy url at current point.
With the universal argument, the text will also be killed"
  (interactive "P")
  (if kill-conditional
      (thing-edit 'url t)
    (thing-edit 'url)))

;;;###autoload
(defun thing-replace-url ()
  "Replace url at current point with kill ring."
  (interactive)
  (thing-replace 'url))

;;;###autoload
(defun thing-paste-word ()
  "Paste words at point."
  (interactive)
  (thing-edit 'word t))

;;;###autoload
(defun thing-copy-word (kill-conditional)
  "Copy words at point.
With the universal argument, the text will also be killed"
  (interactive "P")
  (if kill-conditional
      (thing-edit 'word t)
    (thing-edit 'word)))

;;;###autoload
(defun thing-replace-word ()
  "Replace words at point with kill ring."
  (interactive)
  (thing-replace 'word))

;;;###autoload
(defun thing-paste-symbol ()
  "Paste symbol around point."
  (interactive)
  (thing-edit 'symbol t))

;;;###autoload
(defun thing-copy-symbol (kill-conditional)
  "Copy symbol around point.
 With the universal argument, the text will also be killed"
  (interactive "P")
  (if kill-conditional
      (thing-edit 'symbol t)
    (thing-edit 'symbol)))

;;;###autoload
(defun thing-replace-symbol ()
  "Replace symbol around point with kill ring."
  (interactive)
  (thing-replace 'symbol))

;;;###autoload
(defun thing-paste-line ()
  "Paste current line into Kill-Ring without mark the line."
  (interactive)
  (thing-edit 'line t))

;;;###autoload
(defun thing-copy-line (kill-conditional)
  "Copy current line into Kill-Ring without mark the line.
 With the universal argument, the text will also be killed"
  (interactive "P")
  (if kill-conditional
      (thing-edit 'line t)
    (thing-edit 'line)))

;;;###autoload
(defun thing-replace-line ()
  "Replace current line with kill ring"
  (interactive)
  (thing-replace 'line))

;;;###autoload
(defun thing-copy-paragraph (kill-conditional)
  "Copy current paragraph around the point.
With the universal argument, the text will also be killed"
  (interactive "P")
  (if kill-conditional
      (thing-edit 'paragraph t)
    (thing-edit 'paragraph)))

;;;###autoload
(defun thing-replace-paragraph ()
  "Replace current paragraph around the point with the content of kill ring."
  (interactive)
  (thing-replace 'paragraph))

;;;###autoload
(defun thing-paste-paragraph (&optional kill-conditional)
  "Paste current paragraph around the point"
  (interactive)
  (thing-edit 'paragraph t)
  )

;;;###autoload
(defun thing-paste-defun ()
  "Paste function around point."
  (interactive)
  (thing-edit 'defun t))

;;;###autoload
(defun thing-copy-defun (kill-conditional)
  "Paste function around point.
 With the universal argument, the text will also be killed"
  (interactive "P")
  (if kill-conditional
      (thing-edit 'defun t)
    (thing-edit 'defun)))

;;;###autoload
(defun thing-replace-defun ()
  "Replace function around point with the content of kill ring."
  (interactive)
  (thing-replace 'defun))

;;;###autoload
(defun thing-paste-list ()
  "Paste list around point."
  (interactive)
  (thing-edit 'list t))

;;;###autoload
(defun thing-copy-list (kill-conditional)
  "Paste list around point.
 With the universal argument, the text will also be killed"
  (interactive "P")
  (if kill-conditional
      (thing-edit 'list t)
    (thing-edit 'list)))

;;;###autoload
(defun thing-replace-list ()
  "Replace list around point with the content of kill ring."
  (interactive)
  (thing-replace 'list))

;;;###autoload
(defun thing-paste-sentence ()
  "Paste sentence around point."
  (interactive)
  (thing-edit 'sentence t))

;;;###autoload
(defun thing-copy-sentence (kill-conditional)
  "Paste sentence around point.
 With the universal argument, the text will also be killed"
  (interactive "P")
  (if kill-conditional
      (thing-edit 'sentence t)
    (thing-edit 'sentence)))

;;;###autoload
(defun thing-replace-sentence ()
  "Replace sentence around point with the content of currnt line."
  (interactive)
  (thing-replace 'sentence))

;;;###autoload
(defun thing-paste-whitespace ()
  "Paste whitespace around point."
  (interactive)
  (thing-edit 'whitespace t))

;;;###autoload
(defun thing-copy-whitespace (kill-conditional)
  "Paste whitespace around point.
 With the universal argument, the text will also be killed"
  (interactive "P")
  (if kill-conditional
      (thing-edit 'whitespace t)
    (thing-edit 'whitespace)))

;;;###autoload
(defun thing-replace-whitespace ()
  "Replace whitespace around point with the content of currnt line."
  (interactive)
  (thing-replace 'whitespace))

;;;###autoload
(defun thing-paste-page ()
  "Paste page around point."
  (interactive)
  (thing-edit 'page t))

;;;###autoload
(defun thing-copy-page (kill-conditional)
  "Paste page around point.
 With the universal argument, the text will also be killed"
  (interactive "P")
  (if kill-conditional
      (thing-edit 'page t)
    (thing-edit 'page)))

;;;###autoload
(defun thing-replace-page ()
  "Replace page around point with the content of currnt line."
  (interactive)
  (thing-replace 'page))

;; Below function is not base on thingatpt, but it's effect like above function.
;; So i add to this package.
;;;###autoload
(defun thing-paste-to-line-end ()
  "Paste content from current point to line end."
  (interactive)
  (thing-copy-to-line-end t))

;;;###autoload
(defun thing-copy-to-line-end (&optional kill-conditional)
  "Copy content from current point to line end.
If `KILL-CONDITIONAL' is non-nil, kill object,
otherwise copy object."
  (interactive)
  (save-excursion
    (thing-edit-internal (point)
                         (line-end-position)
                         kill-conditional)))

;;;###autoload
(defun thing-paste-to-line-beginning ()
  "Paste content from current point to line beginning."
  (interactive)
  (thing-copy-to-line-beginning t))

;;;###autoload
(defun thing-copy-to-line-beginning (&optional kill-conditional)
  "Copy content from current point tot line beginning.
If `KILL-CONDITIONAL' is non-nil, kill object,
otherwise copy object."
  (interactive)
  (save-excursion
    (thing-edit-internal (line-beginning-position)
                         (point)
                         kill-conditional)))

;;;###autoload
(defun thing-paste-comment ()
  "Paste the comment around line.
If mark is active, it can paste all comment that in mark."
  (interactive)
  (thing-copy-comment t))

;;;###autoload
(defun thing-copy-comment (&optional kill-conditional)
  "Copy the comment around line.
If mark is active, it can copy all comment that in mark.
If `KILL-CONDITIONAL' is non-nil, kill object,
otherwise copy object."
  (interactive)
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (when mark-active
      (setq beg (region-beginning))
      (setq end (region-end))
      (deactivate-mark))
    (save-excursion
      (setq end (copy-marker end))
      (goto-char beg)
      (while (< (point) end)
        (if (comment-search-forward end t)
            (if kill-conditional
                (call-interactively 'comment-kill)
              (call-interactively 'comment-copy))
          (goto-char end))))))

(defun thing-paste-parentheses ()
  "Paste content in match parentheses."
  (interactive)
  (thing-copy-parentheses t))

(defun thing-copy-parentheses (kill-conditional)
  "Copy content in match parentheses.
If `KILL-CONDITIONAL' is non-nil, kill object,
otherwise copy object."
  (interactive "P")
  (save-excursion
    (if (thing-edit-in-string-p)
        (thing-edit-internal
         (1+ (car (thing-edit-string-start+end-points)))
         (cdr (thing-edit-string-start+end-points))
         kill-conditional)
      (thing-edit-internal
       (progn
         (backward-up-list)
         (forward-char +1)
         (point))
       (progn
         (up-list)
         (forward-char -1)
         (point))
       kill-conditional))))

(defun thing-replace-parentheses ()
  "Replace content in match parentheses with the content of currnt line."
  (interactive)
  (save-excursion
    (if (thing-edit-in-string-p)
        (thing-replace-internal
         (1+ (car (thing-edit-string-start+end-points)))
         (cdr (thing-edit-string-start+end-points)))
      (thing-replace-internal
       (progn
         (backward-up-list)
         (forward-char +1)
         (point))
       (progn
         (up-list)
         (forward-char -1)
         (point))))))

(defun thing-edit-in-string-p (&optional state)
  (or (nth 3 (or state (thing-edit-current-parse-state)))
      (and
       (eq (get-text-property (point) 'face) 'font-lock-string-face)
       (eq (get-text-property (- (point) 1) 'face) 'font-lock-string-face))
      (and
       (eq (get-text-property (point) 'face) 'font-lock-doc-face)
       (eq (get-text-property (- (point) 1) 'face) 'font-lock-doc-face))
      ))

(defun thing-edit-string-start+end-points (&optional state)
  "Return a cons of the points of open and close quotes of the string.
The string is determined from the parse state STATE, or the parse state
  from the beginning of the defun to the point.
This assumes that `thing-edit-in-string-p' has already returned true, i.e.
  that the point is already within a string."
  (save-excursion
    (let ((start (nth 8 (or state (thing-edit-current-parse-state)))))
      (goto-char start)
      (forward-sexp 1)
      (cons start (1- (point))))))

(defun thing-edit-current-parse-state ()
  "Return parse state of point from beginning of defun."
  (let ((point (point)))
    (beginning-of-defun)
    (parse-partial-sexp (point) point)))

(provide 'thing-edit)

;;; thing-edit.el ends here

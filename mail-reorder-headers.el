;;; mail-reorder-headers.el -- Re-arrange mail headers into pleasing order.
 
;;; Copyright (C) 1992, 1993, 1995 Noah S. Friedman
 
;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: mail, extensions
;; Created: 1992
 
;; $Id: mail-reorder-headers.el,v 1.4 1995/11/17 00:11:41 friedman Exp $
 
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
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.
 
;;; Commentary:
;;; Code:
 
(require 'rfc822)
(require 'fmailutils)
 
;;;###autoload
(defvar mail-reorder-headers-preferred
  '("From" "To" "Cc" "Bcc" "Fcc" "Subject" "Date" "Reply-To")
  "*A list of headers that are to be given decreasing priority, in the
order they are listed, when sorting.")
 
;;;###autoload
(defvar mail-reorder-headers-address-headers
  '("To" "Cc" "Bcc" "Resent-To" "Reply-To")
  "*A list of headers that are commonly used to supply address lists.")
 
;;;###autoload
(defvar mail-reorder-headers-delete nil
  "*A list of headers to delete entirely.")
 
;;;###autoload
(defvar mail-reorder-headers-concat-identical-headers t
  "*If non-`nil', concatenate the contents of any identical header names
which are listed in `mail-reorder-headers-address-headers'.  If nil,
headers will be sorted but not concatenated.")
 
;;;###autoload
(defvar mail-reorder-headers-use-rfc822 nil
  "*If non-`nil', canonicalize address headers (remove newlines, excess
spaces, and comments), add commas in the right places, etc.  based on
whether the header is a member of `mail-reorder-headers-address-headers'.")
 
;;;###autoload
(defun mail-reorder-headers ()
  "Sort mail headers according to variable `mail-reorder-headers-preferred',
leaving the order of any other headers at the end in their original order.
 
Headers listed in `mail-reorder-headers-delete' are removed completely.
 
The variable `mail-reorder-header-concat-identical-headers specifies
whether duplicate headers are concatenated.
 
The variable `mail-reorder-headers-use-rfc822' specifies whether to
canonicalize address lists."
  (interactive)
  (let ((preferred-headers (mapcar 'capitalize mail-reorder-headers-preferred))
        (address-headers (mapcar 'capitalize mail-reorder-headers-address-headers))
        saved-headers-alist
        mail-saved-current-header-offset
        (saved-point (point))
        header-names
        (current-header (mail-current-header))
        (case-fold-search t))
    ;; Note the lack of save-excursion here.  We simply save position of
    ;; point on current line (header) so that after deleting and restoring
    ;; headers, the mark is put back where it used to be, i.e. at the same
    ;; offset on the same header it used to be.  Save-excursion isn't
    ;; enough because the mark gets lost when all the lines are deleted.
    ;; (If point wasn't in a header we'll restore it anyway).  We must
    ;; calculate the offset from the beginning of the header, not just the
    ;; current point in the buffer, since the header may move to a
    ;; different absolute line position.
    (cond (current-header
           (goto-char (mail-get-beginning-of-header-line-position))
           (setq mail-saved-current-header-offset (- saved-point (point)))))
 
    ;; Delete headers we never want (useful for deleting headers added by
    ;; VM, such as "Full-Name" headers)
    (let ((list mail-reorder-headers-delete))
      (while list
        (mail-remove-header (car list) 'all-occurrences)
        (setq list (cdr list))))
 
    ;; Get list of remaining header names and sort them so that preferred
    ;; headers have priority.
    (let ((preferred preferred-headers)
          (existing (mapcar 'capitalize (mail-get-header-names 'unique)))
          this)
      (while (and preferred existing)
        (setq this (car preferred))
        (setq preferred (cdr preferred))
        (cond ((member this existing)
               (setq header-names (cons this header-names))
               (setq existing (delete this existing)))))
      (while existing
        (setq header-names (cons (car existing) header-names))
        (setq existing (cdr existing))))
 
    ;; For each header in list, save the name and contents of the header in
    ;; an alist, then delete all occurences of header.  An alist of the
    ;; form ((header contents ...) ...)  is defined.
    ;; Note that header-names is in reverse order from preferred order, so
    ;; the act of consing new header/contents fields onto the alist puts
    ;; the alist in correct order.
    (let ((list header-names)
          header-contents
          header)
      (while list
        (setq header (car list))
        (setq header-contents (mail-get-header-contents header))
        (setq list (cdr list))
        (cond (header-contents
               (setq saved-headers-alist
                     (cons (cons header header-contents)
                           saved-headers-alist))
               (mail-remove-header header 'all-occurences)))))
 
    ;; Now put them back, in order.
    (let ((alist saved-headers-alist)
          header
          contents
          len)
    (while alist
      (setq header (car (car alist)))
      (setq contents (cdr (car alist)))
      (setq len (length contents))
 
      (setq alist (cdr alist))
 
      (if contents
          (if (and mail-reorder-headers-concat-identical-headers
                   (member header address-headers))
              (let ((address-list (mapconcat 'identity contents ", "))
                    final-list
                    final)
                (if mail-reorder-headers-use-rfc822
                    (setq final-list
                          (prog1
                              (rfc822-addresses address-list)
                            ;; That function did some insertion and/or
                            ;; deletion in a temp buffer it made.  Doing so
                            ;; caused last_undo_buffer to be that temp
                            ;; buffer instead of our current buffer.  This
                            ;; means the next insertion or deletion we do
                            ;; will cause an undo boundary.  We don't want
                            ;; that.
                            (let
                                ;; This dynamic let preserves our buffer's
                                ;; undo list as it was outside the let.
                                ((buffer-undo-list nil))
                              ;; Do an insertion to cause this unwanted
                              ;; undo boundary, and to set last_undo_buffer
                              ;; to the current buffer.
                              (insert "fnord")
                              ;; Undo the insertion, leaving the buffer as
                              ;; it was.  Now that last_undo_buffer is the
                              ;; current buffer, later insertions will not
                              ;; cause an undo boundary.
                              (primitive-undo 1 buffer-undo-list))))
                  (setq final-list contents))
                (setq final (mapconcat 'identity final-list ", "))
                (mail-put-unique-header header final 'force-replace))
            (let ((c contents))
              (while c
                (mail-put-header header (car c))
                (setq c (cdr c))))))))
 
    (if current-header
        (progn
          (mail-position-on-field current-header)
          (goto-char (+ (mail-get-beginning-of-header-line-position)
                        mail-saved-current-header-offset)))
      ;; Point wasn't originally in a header.  Put it back where it
      ;; was anyway.
      (goto-char saved-point))))
 
(defun mail-reorder-headers-delete (elt list)
  "Delete by side effect any occurrences of ELT as a member of LIST.
The modified LIST is returned.  Comparison is done with `equal'.
If the first member of LIST is ELT, deleting it is not a side effect;
it is simply using a different list.
Therefore, write `(setq foo (delete element foo))'
to be sure of changing the value of `foo'."
  (let ((p list)
        (l (cdr list)))
    (while l
      (if (equal elt (car l))
          (setcdr p (cdr l))
        (setq p (cdr p)))
      (setq l (cdr l))))
  (if (equal elt (car list))
      (cdr list)
    list))
 
;; Make `delete' an alias if not already defined.
;; This is for emacs 18, which has no builtin delete function.
;; Emacs 18 has no `member' either, but that is taken care of by
;; fmailutils.el.
(or (fboundp 'delete) (fset 'delete 'mail-reorder-headers-delete))
 
;; Make a keybinding for it.
(define-key mail-mode-map "\C-c\C-f\C-r" 'mail-reorder-headers)
 
(provide 'mail-reorder-headers)
 
;;; mail-reorder-headers.el ends here

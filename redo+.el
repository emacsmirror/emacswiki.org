;;; redo+.el --- Redo/undo system for Emacs

;; Copyright (C) 1985, 1986, 1987, 1993-1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems and INS Engineering Corp.
;; Copyright (C) 1997 Kyle E. Jones
;; Copyright (C) 2008, 2009 S. Irie
;; Copyright (C) 2013 HenryVIII

;; Author: Kyle E. Jones, February 1997
;;         S. Irie, March 2008
;; Keywords: lisp, extensions
;; Version: 1.16

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA


;;; Commentary:

;; redo+.el is bug fix and extended version of XEmacs' redo package.

;; Emacs' normal undo system allows you to undo an arbitrary
;; number of buffer changes.  These undos are recorded as ordinary
;; buffer changes themselves.  So when you break the chain of
;; undos by issuing some other command, you can then undo all
;; the undos.  The chain of recorded buffer modifications
;; therefore grows without bound, truncated only at garbage
;; collection time.
;;
;; The redo/undo system is different in two ways:
;;   1. The undo/redo command chain is only broken by a buffer
;;      modification.  You can move around the buffer or switch
;;      buffers and still come back and do more undos or redos.
;;   2. The `redo' command rescinds the most recent undo without
;;      recording the change as a _new_ buffer change.  It
;;      completely reverses the effect of the undo, which
;;      includes making the chain of buffer modification records
;;      shorter by one, to counteract the effect of the undo
;;      command making the record list longer by one.

;;
;; Installation:
;;
;; Save this file as redo+.el, byte compile it and put the
;; resulting redo.elc file in a directory that is listed in
;; load-path.
;;
;; In your .emacs file, add
;;   (require 'redo+)
;; and the system will be enabled.
;;
;; In addition, if you don't want to redo a previous undo, add
;;   (setq undo-no-redo t)
;; You can also use a function `undo-only' instead of `undo'
;; in GNU Emacs 22 or later.


;; History:
;; 2013-04-23  HenryVIII
;;         * Fix for GNU bug report #12581
;;         * Version 1.16
;;
;; 2009-01-07  S. Irie
;;         * Delete unnecessary messages
;;         * Bug fix
;;         * Version 1.15
;;
;; 2008-05-23  S. Irie
;;         * Bug fix
;;         * Version 1.14
;;
;; 2008-05-11  S. Irie
;;         * record unmodified status entry when redoing
;;         * Version 1.13
;;
;; 2008-05-10  S. Irie
;;         * Bug fix
;;         * Version 1.12
;;
;; 2008-05-09  S. Irie
;;         * Bug fix
;;         * Version 1.11
;;
;; 2008-04-02  S. Irie
;;         * undo-no-redo available
;;         * GNU Emacs menu-bar and tool-bar item
;;         * Bug fix
;;         * Version 1.10

;; ToDo:
;;
;; - undo/redo in region

;;; Code:

(defvar redo-version "1.14"
  "Version number for the Redo+ package.")

(defvar last-buffer-undo-list nil
  "The head of buffer-undo-list at the last time an undo or redo was done.")
(make-variable-buffer-local 'last-buffer-undo-list)

(make-variable-buffer-local 'pending-undo-list)

;; Emacs 20 variable
;;(defvar undo-in-progress) ; Emacs 20 is no longer supported.

;; Emacs 21 variable
(defvar undo-no-redo nil)

(defun redo (&optional count)
  "Redo the the most recent undo.
Prefix arg COUNT means redo the COUNT most recent undos.
If you have modified the buffer since the last redo or undo,
then you cannot redo any undos before then."
  (interactive "*p")
  (if (eq buffer-undo-list t)
      (error "No undo information in this buffer"))
  (if (eq last-buffer-undo-list nil)
      (error "No undos to redo"))
  (or (eq last-buffer-undo-list buffer-undo-list)
      ;; skip one undo boundary and all point setting commands up
      ;; until the next undo boundary and try again.
      (let ((p buffer-undo-list))
        (and (null (car-safe p)) (setq p (cdr-safe p)))
        (while (and p (integerp (car-safe p)))
          (setq p (cdr-safe p)))
        (eq last-buffer-undo-list p))
      (error "Buffer modified since last undo/redo, cannot redo"))
  (and (eq (cdr buffer-undo-list) pending-undo-list)
       (error "No further undos to redo in this buffer"))
  ;; This message seems to be unnecessary because the echo area
  ;; is rewritten before the screen is updated.
  ;;(or (eq (selected-window) (minibuffer-window))
  ;;    (message "Redo..."))
  (let ((modified (buffer-modified-p))
        (undo-in-progress t)
        (recent-save (recent-auto-save-p))
        (old-undo-list buffer-undo-list)
        (p buffer-undo-list)
        (q (or pending-undo-list t))
        (records-between 0)
        (prev nil) next)
    ;; count the number of undo records between the head of the
    ;; undo chain and the pointer to the next change.  Note that
    ;; by `record' we mean clumps of change records, not the
    ;; boundary records.  The number of records will always be a
    ;; multiple of 2, because an undo moves the pending pointer
    ;; forward one record and prepend a record to the head of the
    ;; chain.  Thus the separation always increases by two.  When
    ;; we decrease it we will decrease it by a multiple of 2
    ;; also.
    (while p
      (setq next (cdr p))
      (cond ((eq next q)
         ;; insert the unmodified status entry into undo records
         ;; if buffer is not modified.  The undo command inserts
         ;; this information only in redo entries.
         (when (and (not modified) (buffer-file-name))
           (let* ((time (nth 5 (file-attributes (buffer-file-name))))
              (elt (cons (car time) (cadr time))))
         (if (eq (car-safe (car prev)) t)
             (setcdr (car prev) elt)
           (setcdr prev (cons (cons t elt) p)))))
         (setq next nil))
        ((null (car next))
         (setq records-between (1+ records-between))))
      (setq prev p
        p next))
    ;; don't allow the user to redo more undos than exist.
    ;; only half the records between the list head and the pending
    ;; pointer are undos that are a part of this command chain.
    (setq count (min (/ records-between 2) count)
      p (primitive-undo (1+ count) buffer-undo-list))
    (if (eq p old-undo-list)
        nil ;; nothing happened
      ;; set buffer-undo-list to the new undo list.  if has been
      ;; shortened by `count' records.
      (setq buffer-undo-list p)
      ;; primitive-undo returns a list without a leading undo
      ;; boundary.  add one.
      (undo-boundary)
      ;; now move the pending pointer backward in the undo list
      ;; to reflect the redo.  sure would be nice if this list
      ;; were doubly linked, but no... so we have to run down the
      ;; list from the head and stop at the right place.
      (let ((n (- records-between count)))
        (setq p (cdr old-undo-list))
        (while (and p (> n 0))
      (setq p (cdr (memq nil p))
            n (1- n)))
        (setq pending-undo-list p)))
    (and modified (not (buffer-modified-p))
     (delete-auto-save-file-if-necessary recent-save))
    (or (eq (selected-window) (minibuffer-window))
        (message "Redo!"))
    (setq last-buffer-undo-list buffer-undo-list)))

(defun undo (&optional arg)
  "Undo some previous changes.
Repeat this command to undo more changes.
A numeric argument serves as a repeat count."
  (interactive "*p")
  (let ((modified (buffer-modified-p))
        (recent-save (recent-auto-save-p)))
    ;; This message seems to be unnecessary because the echo area
    ;; is rewritten before the screen is updated.
    ;;(or (eq (selected-window) (minibuffer-window))
    ;;    (message "Undo..."))
    (let ((p buffer-undo-list)
      (old-pending-undo-list pending-undo-list))
      (or (eq last-buffer-undo-list buffer-undo-list)
      ;; skip one undo boundary and all point setting commands up
      ;; until the next undo boundary and try again.
      (progn (and (null (car-safe p)) (setq p (cdr-safe p)))
         (while (and p (integerp (car-safe p)))
           (setq p (cdr-safe p)))
         (eq last-buffer-undo-list p))
      (progn (undo-start)
         ;; get rid of initial undo boundary
         (undo-more 1)
         (not undo-no-redo))
      ;; discard old redo information if undo-no-redo is non-nil
      (progn (if (car-safe last-buffer-undo-list)
             (while (and p (not (eq last-buffer-undo-list
                        (cdr-safe p))))
               (setq p (cdr-safe p)))
           (setq p last-buffer-undo-list))
         (if p (setcdr p old-pending-undo-list)))
      ))
    (undo-more (or arg 1))
    ;; Don't specify a position in the undo record for the undo command.
    ;; Instead, undoing this should move point to where the change is.
    ;;
    ;;;; The old code for this was mad!  It deleted all set-point
    ;;;; references to the position from the whole undo list,
    ;;;; instead of just the cells from the beginning to the next
    ;;;; undo boundary.  This does what I think the other code
    ;;;; meant to do.
    (let* ((p buffer-undo-list)
       (list (cons nil p))
       (prev list))
      (while (car p)
        (if (integerp (car p))
        (setcdr prev (cdr p))
      (setq prev p))
        (setq p (cdr p)))
      (setq buffer-undo-list (cdr list)))
    (and modified (not (buffer-modified-p))
     (delete-auto-save-file-if-necessary recent-save)))
  (or (eq (selected-window) (minibuffer-window))
      (message "Undo!"))
  (setq last-buffer-undo-list buffer-undo-list))

;; Modify menu-bar and tool-bar item of GNU Emacs
(unless (featurep 'xemacs)
  ;; condition to undo
  (mapc (lambda (map)
          (let ((elem (copy-sequence (cdr (assq 'undo menu-bar-edit-menu)))))
            (setcar (cdr (memq :enable elem))
                    '(and (not buffer-read-only)
                          (consp buffer-undo-list)
                          (or (not (or (eq last-buffer-undo-list
                                           buffer-undo-list)
                                       (eq last-buffer-undo-list
                                           (cdr buffer-undo-list))))
                              (listp pending-undo-list))))
            elem))
        (append (list menu-bar-edit-menu)
                (if window-system (list tool-bar-map))))
  ;; redo's menu-bar entry
  (define-key-after menu-bar-edit-menu [redo]
    '(menu-item "Redo" redo
                :enable
                (and
                 (not buffer-read-only)
                 (not (eq buffer-undo-list t))
                 (not (eq last-buffer-undo-list nil))
                 (or (eq last-buffer-undo-list buffer-undo-list)
                     (let ((p buffer-undo-list))
                       (and (null (car-safe p)) (setq p (cdr-safe p)))
                       (while (and p (integerp (car-safe p)))
                         (setq p (cdr-safe p)))
                       (eq last-buffer-undo-list p)))
                 (not (eq (cdr buffer-undo-list) pending-undo-list)))
                :help "Redo the most recent undo")
    'undo)
  ;; redo's tool-bar icon
  (when window-system
    (tool-bar-add-item-from-menu
     'redo "redo" nil
     :visible '(not (eq 'special (get major-mode 'mode-class))))
    (define-key-after tool-bar-map [redo]
      (cdr (assq 'redo tool-bar-map)) 'undo)
    ;; use gtk+ icon if Emacs23
    (if (boundp 'x-gtk-stock-map)
        (setq x-gtk-stock-map
              (cons '("etc/images/redo" . "gtk-redo") x-gtk-stock-map)))
    ;; update tool-bar icon immediately
    (defun redo-toolbar-update (&optional bgn end lng)
      (interactive)
      (set-buffer-modified-p (buffer-modified-p)))
    (add-hook 'after-change-functions 'redo-toolbar-update))
  )

(provide 'redo+)

;;; redo+.el ends here

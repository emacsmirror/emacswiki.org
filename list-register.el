;;; list-register.el --- List register
;; -*- Mode: Emacs-Lisp -*-

;;  $Id: list-register.el,v 2.2 2008/02/12 09:17:09 akihisa Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Install:

;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;;     (require 'list-register)

;; The latest version of this program can be downloaded from
;; http://www.bookshelf.jp/elc/list-register.el

;; M-x list-register


;;; Commentary:
;;

;;; Code:
(defvar list-register-buffer "*reg Output*")
(defvar list-register-edit-buffer "*Edit Register*")

;; internal
(defvar list-register-mode-map nil)
(defvar list-register-edit-mode-map nil)
(defvar list-register-parent-buffer nil)
(defvar list-register-edit-reg nil)

;; util
(defun current-line ()
  "Return the vertical position of point..."
  (1+ (count-lines 1 (point))))

(defun max-line ()
  "Return the vertical position of point..."
  (save-excursion
    (goto-char (point-max))
    (current-line)))

(or list-register-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map "e"
        (function list-register-edit-text))
      (define-key map "\C-m"
        (function list-register-insert))
      (define-key map "q"
        (function list-register-quit))
      (define-key map "p"
        (function previous-line))
      (define-key map "n"
        (function next-line))
      (define-key map "+"
        (function list-register-increment))
      (define-key map "-"
        (function list-register-decrement))

      (setq list-register-mode-map map)))

(or list-register-edit-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\C-c\C-c"
        (function list-register-edit-quit))
      (define-key map "\C-c\C-q"
        (function list-register-edit-cancel))
      (define-key map "\C-c\C-s"
        (function list-register-edit-set-register))
      (setq list-register-edit-mode-map map)))

(defun list-register-quit ()
  "Exit *list-register* buffer."
  (interactive)
  (set-buffer list-register-parent-buffer)
  (condition-case ()
      (delete-window (get-buffer-window list-register-buffer))
    (error ))
  (kill-buffer list-register-buffer))

(defun list-register-change-number (num)
  "Add number of register to NUM."
  (let (reg str)
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward "^[ \n]*\\([^\n:]+\\):[ \n]*\\([^:\n]+\\):.+$"
                             (line-end-position) t)
          (progn
            (setq reg (buffer-substring
                       (match-beginning 1) (match-end 1)))
            (setq str (buffer-substring
                       (match-beginning 2) (match-end 2)))))
      (if (string-match "num" str)
          (increment-register num (string-to-char reg))
        (message "Register does not contain a number!"))))
  (list-register-review))

(defun list-register-increment (num)
  "Add number of register to NUM."
  (interactive "nIncrement Num: ")
  (list-register-change-number num))

(defun list-register-decrement (num)
  "Subtract NUM from number of register."
  (interactive "nDecrement Num: ")
  (list-register-change-number (* -1 num)))

;; edit register
(defun list-register-edit-text-do (reg)
  "Make the buffer to edit text of REG."
  (switch-to-buffer (get-buffer-create list-register-edit-buffer))
  (erase-buffer)

  (list-insert-register (string-to-char reg))
  (kill-all-local-variables)
  (make-local-variable 'list-register-edit-reg)
  (setq list-register-edit-reg reg)

  (use-local-map list-register-edit-mode-map))

(defun list-register-edit-text ()
  "Edit text of register of current line."
  (interactive)
  (let (reg str)
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward
           "^[ \n]*\\([^\n:]+\\):[ \n]*\\([^:\n]+\\):.+$"
           (line-end-position) t)
          (progn
            (setq reg (buffer-substring
                       (match-beginning 1) (match-end 1)))
            (setq str (buffer-substring
                       (match-beginning 2) (match-end 2)))))
      (if (string-match "[0-9]+" str)
          (list-register-edit-text-do reg)
        (message "Register does not contain a text!")))))

(defun list-register-edit-quit ()
  "Exit the buffer to edit the register."
  (interactive)
  (set-register
   (string-to-char list-register-edit-reg)
   (buffer-substring (point-min) (point-max)))
  ;;(delete-window (get-buffer-window list-register-edit-buffer))
  (kill-buffer list-register-edit-buffer)
  (switch-to-buffer list-register-buffer)
  (list-register-review))

(defun list-register-edit-set-register (char)
  "Save text of a register to another register (CHAR)."
  (interactive "cCopy to register: ")
  (set-register
   char
   (buffer-substring (point-min) (point-max)))
  ;;(delete-window (get-buffer-window list-register-edit-buffer))
  (kill-buffer list-register-edit-buffer)
  (switch-to-buffer list-register-buffer)
  (list-register-review))

(defun list-register-edit-cancel ()
  "Cancel to edit a register."
  (interactive)
  (kill-buffer list-register-edit-buffer)
  (switch-to-buffer list-register-buffer))

(defun list-register-insert ()
  "Insert text of a register."
  (interactive)
  (let (reg str)
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward
           "^[ \n]*\\([^\n:]+\\):[ \n]*\\([^:\n]+\\):.+$"
           (line-end-position) t)
          (progn
            (setq reg (buffer-substring
                       (match-beginning 1) (match-end 1)))
            (setq str (buffer-substring
                       (match-beginning 2) (match-end 2)))))

      (set-buffer list-register-parent-buffer)
      (cond
       ((or
         (string-match "file" str)
         (string-match "conf" str)
         (string-match "pos" str))
        (list-jump-to-register (string-to-char reg)))
       ((or
         (string-match "num" str)
         (string-match "[0-9]+" str))
        (list-insert-register (string-to-char reg))
        (condition-case ()
            (delete-window (get-buffer-window list-register-buffer))
          (error ))
        (kill-buffer list-register-buffer))))))

(defun list-register-print-text (arg)
  "Print the text of register of ARG."
  (interactive "p")
  (let ((x (get-register arg)) (w (- (window-width) 15))
        str strtmp lines prev)
    (setq str (split-string x "\n"))
    (setq strtmp str)
    (setq lines (format "%4d" (length str)))
    (setq str (mapconcat (lambda (y) y) str " "))
    (if (string-match "^[ \t]*$" str)
        ()
      (insert (format "%s: %s\n" lines
                      (truncate-string-to-width
                       (mapconcat (lambda (y) y) strtmp
                                  "^J") w)))
      (setq prev str))))

(defun list-register ()
  "List contents of register."
  (interactive)
  (let ((lst register-alist) val reg st (pbuf (current-buffer))
        lines)
    (with-output-to-temp-buffer list-register-buffer
      (set-buffer list-register-buffer)
      (kill-all-local-variables)

      (make-local-variable 'list-register-parent-buffer)
      (setq list-register-parent-buffer pbuf)

      (use-local-map list-register-mode-map)

      (princ "List of register\n")
      (setq st (point))
      (while lst
        (setq reg (car lst))
        (setq lst (cdr lst))
        (princ
         (concat
          ;;"-------------------------------------------------\n"
          (format "%3s"
                  (single-key-description (car reg)))
          ":"))
        (setq val (get-register (car reg)))
        (cond
         ((numberp val)
          (insert " num:")
          (insert (int-to-string val))
          (insert "\n"))

         ((markerp val)
          (insert " pos:")
          (let ((buf (marker-buffer val)))
            (if (null buf)
                (insert "a marker in no buffer")
              (insert "a buffer position:")
              (insert (buffer-name buf))
              (insert ", position ")
              (insert (int-to-string (marker-position val)))
              (insert "\n"))))

         ((and (consp val) (window-configuration-p (car val)))
          (insert "conf:a window configuration.\n"))

         ((and (consp val) (frame-configuration-p (car val)))
          (insert "conf:a frame configuration.\n"))

         ((and (consp val) (eq (car val) 'file))
          (insert "file:")
          (prin1 (cdr val))
          (insert ".\n"))

         ((and (consp val) (eq (car val) 'file-query))
          (insert "file:a file-query reference: file ")
          (insert (car (cdr val)))
          (insert ", position ")
          (insert (int-to-string (car (cdr (cdr val)))))
          (insert ".\n"))

         ((consp val)
          (setq lines (format "%4d" (length val)))
          (insert (format "%s: %s\n" lines
                          (truncate-string-to-width
                           (mapconcat (lambda (y) y) val
                                      "^J") (- (window-width) 15)))))
         ((stringp val)
          (list-register-print-text (car reg)))
         (t
          ;;(insert "Garbage:\n")
          ;;(prin1 val))
          )))
      (sort-lines nil st (point-max))
      (setq buffer-read-only t)))
  (pop-to-buffer list-register-buffer))

(defun list-register-review ()
  "Update list-register buffer."
  (let ((pbuf list-register-parent-buffer)
        (cp (current-line)))
    (list-register)
    (next-line (- cp 1))
    (setq list-register-parent-buffer pbuf)))

(defun my-jump-to-register (&optional arg)
  (interactive)
  (let (char)
    (message "Jump to register: ")
    (list-register)
    (setq char (read-char))
    (list-jump-to-register char)))

(defun data-to-resgister (arg)
  (interactive "P")
  (let ((char))
    (message "Copy to register: ")
    (setq char (read-char))
    (cond
     (mark-active
      (if (and
           (not (= (save-excursion
                     (goto-char (region-beginning)) (current-column))
                   (save-excursion
                     (goto-char (region-end)) (current-column))))
           (not (= (save-excursion
                     (goto-char (region-beginning)) (current-line))
                   (save-excursion
                     (goto-char (region-end)) (current-line)))))
          (if (y-or-n-p "Rectangle? ")
              (progn
                (copy-rectangle-to-register
                 char (region-beginning) (region-end) arg))
            (set-register char (buffer-substring
                                (region-beginning) (region-end)))
            (if arg
                (delete-region (region-beginning) (region-end))))))
     (t
      (message "f)rame w)indow p)oint")
      (let ((c (char-to-string (read-char))))
        (cond
         ((string-match "f" c)
          (frame-configuration-to-register char arg))
         ((string-match "w" c)
          (window-configuration-to-register char arg))
         ((string-match "p" c)
          (point-to-register char arg))))))))

(defun list-insert-register (register)
  (push-mark)
  (let ((val (get-register register)))
    (cond
     ((consp val)
      (insert-rectangle val))
     ((stringp val)
      (insert val))
     ((numberp val)
      (princ val (current-buffer)))
     ((and (markerp val) (marker-position val))
      (princ (marker-position val) (current-buffer)))
     (t
      (error "Register does not contain text")))))

(defun list-jump-to-register (register)
  (let ((val (get-register register)))
    (cond
     ((and (consp val) (frame-configuration-p (car val)))
      (set-frame-configuration (car val))
      (goto-char (cadr val)))
     ((and (consp val) (window-configuration-p (car val)))
      (set-window-configuration (car val))
      (goto-char (cadr val)))
     ((markerp val)
      (or (marker-buffer val)
          (error "That register's buffer no longer exists"))
      (switch-to-buffer (marker-buffer val))
      (goto-char val))
     ((and (consp val) (eq (car val) 'file))
      (find-file (cdr val)))
     ((and (consp val) (eq (car val) 'file-query))
      (or (find-buffer-visiting (nth 1 val))
          (y-or-n-p (format "Visit file %s again? " (nth 1 val)))
          (error "Register access aborted"))
      (find-file (nth 1 val))
      (goto-char (nth 2 val)))
     (t
      (error "Register doesn't contain a buffer position or configuration")))))

(provide 'list-register)
;;; list-register.el ends here

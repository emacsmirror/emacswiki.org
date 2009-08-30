;;; isearch-all.el

;; Copyright (C) 2003 Matsushita Akihisa <akihisa@mail.ne.jp>

;; Authors: Matsushita Akihisa <akihisa@mail.ne.jp>
;; Keywords: isearch

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.


;;; Commentary:

;; The latest version of this program can be downloaded from
;; http://www.bookshelf.jp/elc/isearch-all.el

;;; Install:

;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;;     (require 'isearch-all)

;; User Variables
(defvar isearch-all-buffer nil)

(defvar isearch-buffer-name-exclusion-list* '("TAGS" "*Completions*" "*Messages*" ".+\\.obj")
  "Contains a list of regexprs against which each buffer's name is
tested when doing a moccur. Matching buffers are *not* searched for
occurrences. Per default, the TAGS file is excluded.")

(defvar isearch-buffer-name-inclusion-list* '("[^ ].*")
  "Contains a list of regexprs against which each buffer's name is
tested when doing a moccur. *Only* matching buffers are searched for
occurrences. Per default, this var contains only a \".*\"
catchall-regexp.")

;; Variables
(defvar isearch-all-buffer-system nil)
(defvar isearch-all nil)
(defvar isearch-all-buffers nil)
(defvar isearch-all-direction nil)
(defvar isearch-all-buffer-list nil)
(defvar isearch-all-current-buffer nil)

(defun isearch-buffer-in-list-p (buffer-name buffer-name-regexps)
  (cond ((null buffer-name-regexps) nil)
        ((eq (string-match  (car buffer-name-regexps) buffer-name) 0) t)
        (t (isearch-buffer-in-list-p buffer-name (cdr buffer-name-regexps)))))

(defun isearch-filter-buffers (buffer-list)
  (let ((isearch-buffers nil))
    (while buffer-list
      (if (and (isearch-buffer-in-list-p (buffer-name (car buffer-list))
                                         isearch-buffer-name-inclusion-list*)
               (not (isearch-buffer-in-list-p (buffer-name (car buffer-list))
                                              isearch-buffer-name-exclusion-list*)))
          (setq isearch-buffers
                (cons (car buffer-list)
                      isearch-buffers)))
      (setq buffer-list (cdr buffer-list)))
    isearch-buffers))

(defadvice isearch-mode
  (before init-all-buffer-isearch activate)
  (setq isearch-all-buffer-system isearch-all-buffer)
  (setq isearch-all-buffer-list (isearch-filter-buffers (buffer-list)))
  (setq isearch-all-current-buffer (current-buffer)))

(defadvice isearch-forward
  (after init-all-buffer-isearch activate)
  (setq isearch-all t)
  (setq isearch-all-direction 'forward))

(defadvice isearch-backward
  (after init-all-buffer-isearch activate)
  (setq isearch-all t)
  (setq isearch-all-direction 'backward))

(defadvice isearch-repeat
  (before all-buffer-isearch activate)
  (setq isearch-all-direction direction)
  (if (and (not isearch-success)
           isearch-wrapped
           isearch-all
           isearch-all-buffer-system)
      (isearch-all-change-page direction)))

(defadvice isearch-done
  (after end-all-isearch-buffer activate)
  (setq isearch-all nil)
  (setq isearch-all-buffers nil)
  (setq isearch-all-direction nil))

(defadvice isearch-push-state
  (after push-all-isearch-buffer activate)
  (setq isearch-all-buffers
        (cons (current-buffer)
              isearch-all-buffers)))

(defadvice isearch-pop-state
  (before all-isearch-pop-state activate)
  (switch-to-buffer (car isearch-all-buffers))
  (setq isearch-all-buffers (cdr isearch-all-buffers))
  )

(defun isearch-all-change-page (direction &optional buffer)
  (if buffer
      (progn
        (if (string= (buffer-name (car (reverse isearch-all-buffer-list)))
                     (buffer-name (current-buffer)))
            (setq isearch-all-buffer-list (append
                                           (list (car (reverse isearch-all-buffer-list)))
                                           (reverse (cdr (reverse isearch-all-buffer-list))))))
        (switch-to-buffer (buffer-name (car (reverse isearch-all-buffer-list))))
        (setq isearch-all-buffer-list (append
                                       (list (car (reverse isearch-all-buffer-list)))
                                       (reverse (cdr (reverse isearch-all-buffer-list))))))
    (progn
      (if (string= (buffer-name (car isearch-all-buffer-list))
                   (buffer-name (current-buffer)))
          (setq isearch-all-buffer-list (append
                                         (cdr isearch-all-buffer-list)
                                         (list (car isearch-all-buffer-list)))))
      (switch-to-buffer (buffer-name (car isearch-all-buffer-list)))
      (setq isearch-all-buffer-list (append
                                     (cdr isearch-all-buffer-list)
                                     (list (car isearch-all-buffer-list)))))
    )
  (if (eq direction 'forward)
      (goto-char (point-min))
    (goto-char (point-max)))
  (setq isearch-other-end nil)
  (setq isearch-success nil)
  (setq isearch-wrapped nil)
  (setq mark-active nil))

(defun isearch-all-next-page ()
  (interactive)
  (isearch-all-change-page isearch-all-direction)
  (isearch-repeat isearch-all-direction)
  )

(defun isearch-all-prev-page ()
  (interactive)
  (isearch-all-change-page isearch-all-direction t)
  (isearch-repeat isearch-all-direction)
  )

(defun isearch-all-first-page ()
  (interactive)
  (switch-to-buffer isearch-all-current-buffer)
  (let ((cmd (car (reverse isearch-cmds))))
    (setq isearch-string (car cmd)
          isearch-message (car (cdr cmd))
          isearch-success (nth 3 cmd)
          isearch-forward (nth 4 cmd)
          isearch-other-end (nth 5 cmd)
          isearch-word (nth 6 cmd)
          isearch-invalid-regexp (nth 7 cmd)
          isearch-wrapped (nth 8 cmd)
          isearch-barrier (nth 9 cmd)
          isearch-within-brackets (nth 10 cmd)
          isearch-case-fold-search (nth 11 cmd))
    (goto-char (car (cdr (cdr cmd)))))
  (isearch-done))

(defun isearch-all-buffers ()
  (interactive)
  (setq isearch-all-buffer-system t))

(define-key isearch-mode-map "\C-f" 'isearch-all-first-page)
(define-key isearch-mode-map "\C-n" 'isearch-all-next-page)
(define-key isearch-mode-map "\C-p" 'isearch-all-prev-page)
(define-key isearch-mode-map "\C-u" 'isearch-all-buffers)

(provide 'isearch-all)
;;; isearch-all.el ends here

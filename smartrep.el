;;; smartrep.el --- Support sequential operation which omitted prefix keys.

;; Filename: smartrep.el
;; Description: Support sequential operation which omitted prefix keys.
;; Author: myuhe <yuhei.maeda_at_gmail.com>
;; Maintainer: myuhe
;; Copyright (C) :2011, myuhe all rights reserved.
;; Created: :2011-12-19
;; Version: 0.0.1
;; Keywords: convenience
;; URL: https://github.com/myuhe/smartrep.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 0:110-1301, USA.

;;; Commentary:

;;
;; Installation:
;;   Put the smartrep.el to your load-path.
;;   And add to .emacs: (require 'smartrep)

;;; Changelog:
;;
;;

;;; Code:
(eval-when-compile
  (require 'cl))

(defvar smartrep-key-string nil)

(defun smartrep-define-key (keymap prefix alist)
  (mapcar (lambda(x)
            (define-key keymap
              (read-kbd-macro 
               (concat prefix " " (car x))) (smartrep-map alist)))
          alist))

(defun smartrep-map (alist)
  (lexical-let ((lst alist))
    (lambda ()
      (interactive)
      (let ((repeat-repeat-char last-command-event))
        (if (memq last-repeatable-command
                  '(exit-minibuffer
                    minibuffer-complete-and-exit
                    self-insert-and-exit))
            (let ((repeat-command (car command-history)))
              (eval repeat-command))
          (progn
            (run-hooks 'pre-command-hook)
            (smartrep-extract-fun repeat-repeat-char lst)
            (run-hooks 'post-command-hook)))
        (when repeat-repeat-char
          (lexical-let ((undo-inhibit-record-point t))
            (unwind-protect
                (while 
                    (lexical-let ((evt (read-key)))
                      ;; (eq (or (car-safe evt) evt)
                      ;;     (or (car-safe repeat-repeat-char)
                      ;;         repeat-repeat-char))
                      (setq smartrep-key-string evt)
                      (smartrep-extract-char evt lst))
                  (condition-case nil
                      (smartrep-extract-fun smartrep-key-string lst)
                    (error nil))))
            (setq unread-command-events (list last-input-event))))))))

(defun smartrep-extract-char (char alist)
  (car (smartrep-filter char alist)))

(defun smartrep-extract-fun (char alist)
  (funcall
   (eval (cdr (smartrep-filter char alist)))))

(defun smartrep-filter (char alist)
  (assoc 
   char
   (mapcar (lambda (x)
             (cons 
              (if (vectorp (read-kbd-macro (car x)))
                  (aref (read-kbd-macro (car x)) 0)
                (string-to-char (read-kbd-macro (car x))))
              (cdr x)))
           alist)))

(provide 'smartrep)

;;; smartrep.el ends here

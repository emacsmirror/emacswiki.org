;;; auto-mark.el --- Mark automatically

;; Copyright (C) 2008  MATSUYAMA Tomohiro

;; Author: MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
;; Keywords: convenience

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

;; auto-mark push marks automatically when you change kind of command sequence.
;; For example, previous-line and next-line is motion comman, and
;; self-insert-command is edit (insert) command, so auto-mark will push
;; a mark automatically after C-n if you type `C-p C-p C-n f o o',
;; because `C-p C-p C-n' is motion command sequence and `f o o' is
;; edit command sequence.

;; auto-mark will regard a command as motion command if the command causes
;; to move a point, and will regard a command as edit command if the command causes
;; to change the buffer size.

;; In addition, you can specify a kind of command by adding a pair of COMMAND and CLASS
;; into `auto-mark-command-class-alist'. For example, if you want to make auto-mark to
;; regard `goto-line' command as a jump command not move command to push a mark automatically
;; when you goto-line even after move command sequence, add '(goto-line . jump) into the list
;; so that auto-mark can detect command sequence changes.

;; To use this package, please add following code into your .emacs:
;; (require 'auto-mark)
;; (global-auto-mark-mode 1)

;;
;; TODO documentation
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup auto-mark nil
  "Mark automatically."
  :group 'convenience
  :prefix "auto-mark-")

(defcustom auto-mark-command-class-alist
  '((goto-line . jump))
  "A list of (COMMAND . CLASS) for classfying command to CLASS.

COMMAND is a symbol you want to try to classify.
CLASS is a symbol for detecting a border where auro-mark should push mark.

There is pre-defined CLASS:
edit      edit command
move      point move command
ignore    make auto-mark ignore pushing mark"
  :group 'auto-mark
  :type '(alist :key-type symbol :value-type symbol))

(defcustom auto-mark-command-classifiers nil
  "A list of functions classifing command to class.
The function takes one argument COMMAND,
and returns CLASS."
  :group 'auto-mark
  :type '(repeat function))

(defcustom auto-mark-ignore-move-on-sameline t
  "Ignore move on same line."
  :group 'auto-mark
  :type 'boolean)

(defvar auto-mark-previous-buffer-size nil
  "Previous buffer size for detecting changes the buffer.")

(defvar auto-mark-previous-point nil
  "Previous point for detecting moves.")

(defvar auto-mark-command-class nil
  "Current command sequence class.")

(defun auto-mark-classify-command (command)
  (or (cdr-safe (assq command auto-mark-command-class-alist))
      (let (class
            (classifiers auto-mark-command-classifiers))
        (while (and (consp classifiers) (null class))
          (setq class (funcall (car classifiers) command))
          (setq classifiers (cdr classifiers)))
        class)))

(defun auto-mark-pre-command-handle ()
  (setq auto-mark-previous-buffer-size (buffer-size)
        auto-mark-previous-point (point))
  (auto-mark-handle-command-class
   (auto-mark-classify-command this-command)))

(defun auto-mark-post-command-handle ()
  (auto-mark-handle-command-class
   (if (eq 'ignore (auto-mark-classify-command this-command))
       'ignore
     (if (/= auto-mark-previous-buffer-size (buffer-size))
         'edit
       
       (if (or (and auto-mark-ignore-move-on-sameline
                    (/= (line-number-at-pos auto-mark-previous-point)
                        (line-number-at-pos (point))))
               (/= auto-mark-previous-point (point)))
           'move)))))

(defun auto-mark-handle-command-class (class)
  (if (and class
           (not (or (eq class 'ignore)
                    (eq class auto-mark-command-class))))
      (progn
        (push-mark auto-mark-previous-point t nil)
        (setq auto-mark-command-class class))))

(defun auto-mark-mode-maybe ()
  (if (not (minibufferp (current-buffer)))
      (auto-mark-mode 1)))

(require 'easy-mmode)

(define-minor-mode auto-mark-mode
  "AutoMark mode."
  :group 'auto-mark
  (if auto-mark-mode
      (progn
        (make-local-variable 'auto-mark-previous-buffer-size)
        (make-local-variable 'auto-mark-previous-point)
        (make-local-variable 'auto-mark-command-class)
        (setq auto-mark-previous-buffer-size 0
              auto-mark-previous-point (point-min)
              auto-mark-command-class nil)
        (add-hook 'pre-command-hook 'auto-mark-pre-command-handle nil t)
        (add-hook 'post-command-hook 'auto-mark-post-command-handle nil t))
    (remove-hook 'pre-command-hook 'auto-mark-pre-command-handle t)
    (remove-hook 'post-command-hook 'auto-mark-post-command-handle t)))

(define-global-minor-mode global-auto-mark-mode
  auto-mark-mode auto-mark-mode-maybe
  :group 'auto-mark)

(provide 'auto-mark)
;;; auto-mark.el ends here

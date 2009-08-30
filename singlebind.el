;;; singlebind.el --- Bind commands to single characters

;; Copyright (C) 2007  Tamas Patrovics

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; It occured to me that I rarely type single characters without doing
;; something afterwards (like saving the file), so I thought it could
;; be used for convenient single-key command execution.
;;
;; If a self-insert-command is typed after a non self-insert-command
;; then singlebind waits for a short while (singlebind-command-delay)
;; and if there are no new input events then it checks if there is a
;; command assigned to the key which invoked the self-insert-command.
;;
;; If a command is found then the effect of the self-insert-command is
;; canceled and the command is invoked. Here's how to assign a command
;; to a key:
;;
;;    (push '(?g . grep) singlebind-command-map)
;;
;;
;; In read-only buffers singlebind-command-delay is not used and the
;; bound command is invoked immediately.
;;
;;
;; Thanks to Thorsten Bonow for making it work on XEmacs.
;;

;;; Code:

(defvar singlebind-command-map nil)

(defvar singlebind-command-delay 0.5)

(defvar singlebind-insert-after nil
  "*Insert the typed command trigger character only if the user cancels
the command by further typing.")

(defconst singlebind-idle-timer-delay
  (if (featurep 'xemacs)
      0.1
    0))

(put 'singlebind-self-insert 'delete-selection t)

(add-hook 'minibuffer-setup-hook 'singlebind-minibuffer-setup)
(add-hook 'minibuffer-exit-hook 'singlebind-minibuffer-exit)


(defun singlebind-minibuffer-setup ()
  (singlebind-disable))

(defun singlebind-minibuffer-exit ()
  (singlebind-enable))


(defun singlebind-enable ()
  (substitute-key-definition 'self-insert-command
                             'singlebind-self-insert
                             (current-global-map)))

(defun singlebind-disable ()
  (substitute-key-definition 'singlebind-self-insert
                             'self-insert-command
                             (current-global-map)))

(defun singlebind-self-insert (n)
  (interactive "p")
  (if (not (= n 1))
      (self-insert-command n)

    (let ((buffer-modified (buffer-modified-p)))
      (unless (or buffer-read-only
                  singlebind-insert-after)
        (self-insert-command n))
      (if (and (not (eq last-command 'singlebind-self-insert))
               (assoc last-command-char singlebind-command-map))
          (if buffer-read-only
              (singlebind-execute-command-maybe n buffer-modified)

            ;; idle timer is used here to avoid halting the execution
            ;; with sit-for (sit-for here would affect paren
            ;; highlighting)
            (run-with-idle-timer singlebind-idle-timer-delay
                                 nil 'singlebind-wait-for-idle 
                                 n buffer-modified))

        ;; signal error or invoke the bound command if there is any
        (if (or buffer-read-only
                singlebind-insert-after)
            (self-insert-command n))))))


(defun singlebind-wait-for-idle (n buffer-modified)
  (if singlebind-insert-after
      (message "Invoking command `%s' [keep typing to cancel]"
               (cdr (assoc last-command-char singlebind-command-map))))

  (if (sit-for singlebind-command-delay)
      (singlebind-execute-command-maybe n buffer-modified)

    (if singlebind-insert-after
        (self-insert-command n))))


(defun singlebind-execute-command-maybe (n buffer-modified)
  (let ((command (cdr (assoc last-command-char
                             singlebind-command-map))))
    ;; undo didn't seem to work here
    (unless (or buffer-read-only
                singlebind-insert-after)
      (backward-delete-char 1)
      (unless buffer-modified
        (set-buffer-modified-p nil)))

    (call-interactively command)))

(singlebind-enable)


(provide 'singlebind)
;;; singlebind.el ends here

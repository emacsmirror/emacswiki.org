[[zh:interaction-log]]
;;; interaction-log.el --- exhaustive log of interactions with Emacs

;; Copyright (C) 2012-2013 Michael Heerdegen

;; Author: Michael Heerdegen <michael_heerdegen@web.de>
;; Maintainer: Michael Heerdegen <michael_heerdegen@web.de>
;; Created: Dec 29 2012
;; Keywords: convenience
;; Version: 0.1
;; Last-Updated: 2013_04_19
;;           By: michael_heerdegen
;;     Update #: 0

;; Compatibility: GNU Emacs 24

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;;
;;; This package provides a buffer *Emacs Log* showing the last hit
;;; keys and executed commands, messages and file loads in
;;; chronological order.  This enables you to reconstruct the last
;;; seconds of your work with Emacs.
;;;
;;; Installation: Put this file in your load path and byte-compile it.
;;; To start logging automatically at startup, add this to your init
;;; file:
;;;
;;; (require 'interaction-log)
;;; (interaction-log-mode +1)
;;;
;;; You probably will want to have a hotkey for showing the log
;;; buffer, so also add something like
;;;
;;; (global-set-key [f1] (lambda () (interactive) (display-buffer ilog-buffer-name)))
;;;
;;; Usage: Use `interaction-log-mode' to toggle logging.  Enabling the
;;; mode will cause all messages and all pressed keys (along with the
;;; actually executed command and the according buffer) to be logged
;;; in the background.  Also loading of files will be logged - in a
;;; tree-style manner for recursive loads.  If an executed command
;;; causes any buffer to change, it will be highlighted in orange so
;;; you can check if you made changes by accident.  If a command
;;; caused any message to be displayed in the echo area (e.g. if an
;;; error occurred), it is highlighted in red.
;;; 
;;; If you find any bugs or have suggestions for improvement, please
;;; tell me!


;;; Change Log:

;;; Code:

(eval-when-compile (require 'cl))
(require 'view)
(require 'timer)
(require 'font-lock)

;;; Customizable stuff

(defgroup interaction-log nil
  "Emacs Interaction Log."
  :prefix "ilog-"
  :group 'convenience)

(defface ilog-non-change-face '((t (:inherit success)))
  "Face for keys that didn't cause buffer changes."
  :group 'interaction-log)

(defface ilog-change-face '((t (:inherit warning)))
  "Face for keys that caused buffer changes."
  :group 'interaction-log)

(defface ilog-echo-face '((t (:inherit error)))
  "Face for keys that caused text being displayed in the echo area."
  :group 'interaction-log)

(defface ilog-load-face '((t (:inherit 'font-lock-string-face)))
  "Face for lines describing file loads."
  :group 'interaction-log)

(defface ilog-message-face '((t (:inherit shadow)))
  "Face for messages."
  :group 'interaction-log)

(defcustom ilog-tail-mode t
  "When non-nil, let the cursor follow the end of the log buffer.
This is like in *Messages*: if you put the cursor at the end of
the *Emacs Log* buffer, it will stay at the buffer's end when
more stuff is added.
When nil, the cursor will stay at the same text position."
  :group 'interaction-log :type 'boolean)

(defcustom ilog-log-max 1000
  "Maximum number of lines to keep in the *Emacs Log* buffer.
If t, don't truncate the buffer when it becomes large"
  :group 'interaction-log :type '(choice (const  :tag "Unlimited" t)
                                         (number :tag "lines")))


;;; Internal Variables

(defvar ilog-recent-commands nil)

(defvar ilog-changing-log-buffer-p nil
  "Non-nil means buffer changes should not be recorded.
Bound to t  when adding to the log buffer.")

(defvar ilog-last-command-changed-buffer-p nil
  "Whether the last command caused changes to any buffer.")

(defvar ilog-buffer-name "*Emacs Log*"
  "The name used for the log buffer.")

(defvar ilog-recent-commands-messages-marker
  (with-current-buffer (get-buffer-create "*Messages*")
    (let ((marker (point-min-marker)))
      (set-marker-insertion-type marker nil)
      marker))
  "Marking how far we got with copying from *Messages*.")

(defvar ilog-truncation-timer nil)

(defvar ilog-insertion-timer nil)

(defvar ilog-temp-load-hist nil
  "Holding file loads not-yet processed by us.")

;;; User commands

(define-minor-mode interaction-log-mode
  "Global minor mode logging keys, commands, file loads and messages.
Logged stuff goes to the *Emacs Log* buffer."
  :group 'interaction-log
  :lighter nil
  :global t
  (if interaction-log-mode
      (progn
        (add-hook 'after-change-functions #'ilog-note-buffer-change)
        (add-hook 'pre-command-hook       #'ilog-record-this-command)
        (add-hook 'post-command-hook      #'ilog-post-command)
        (setq ilog-truncation-timer (run-at-time 30 30 #'ilog-truncate-log-buffer))
        (setq ilog-insertion-timer (run-with-idle-timer .3 .3 #'ilog-update-log-buffer))
        (message "Interaction Log: started logging in %s" ilog-buffer-name))
    (remove-hook 'after-change-functions #'ilog-note-buffer-change)
    (remove-hook 'pre-command-hook       #'ilog-record-this-command)
    (remove-hook 'post-command-hook      #'ilog-post-command)
    (when (timerp ilog-truncation-timer) (cancel-timer ilog-truncation-timer))
    (setq ilog-truncation-timer nil)
    (when (timerp ilog-insertion-timer) (cancel-timer ilog-insertion-timer))
    (setq ilog-insertion-timer nil)))

;;; Helper funs

(defun ilog-log-file-load (file)
  "Annotate a file load in `ilog-temp-load-hist'."
  (when ilog-recent-commands
    (callf concat (nth 4 (car ilog-recent-commands))
      (ilog-get-last-messages)
      (propertize
       (concat (if load-file-name
                   (concat (file-name-sans-extension (file-name-nondirectory load-file-name))
                           " loaded ")
                 "Loaded ")
               file)
       'load-message t)
      "\n")
    ;; ilog-temp-load-hist
    (push (cons load-file-name file) ilog-temp-load-hist)))

(defun ilog-parse-load-tree ()
  "Calculate load levels according to `ilog-temp-load-hist'.
Save the result in `ilog-temp-load-hist'."
  ;; Or is there a more efficient way to get the load recursion depth?
  (setq ilog-temp-load-hist
        (let* ((last-loaded ())
               parser ; avoid compiler warning for next
               (parser (lambda (accumulated entries)
                         (if (null entries)
                             accumulated
                           (let* ((entry (car entries))
                                  (loaded-directly-p (not (car entry)))
                                  (loaded-by-ancestor-p (and (car entry)
                                                             (member (car entry) last-loaded)))
                                  (last-loaded
                                   (cond
                                    (loaded-directly-p (list (cdr entry)))
                                    (loaded-by-ancestor-p (cons (cdr entry) loaded-by-ancestor-p))
                                    (t             (list* (cdr entry) (car entry) (cdr last-loaded))))))
                             (funcall parser
                                      (cons (1- (length last-loaded)) accumulated)
                                      (cdr entries)))))))
          (funcall parser () ilog-temp-load-hist))))

(add-hook 'after-load-functions #'ilog-log-file-load)

(defun ilog-get-last-messages ()
  "Return a string including the last messages.
This is a multiline string containing all messages that appeared
in *Messages* since the last call of this function."
  (with-current-buffer (get-buffer-create "*Messages*")
    (prog1 (if (< ilog-recent-commands-messages-marker (point-max))
               (buffer-substring ilog-recent-commands-messages-marker (point-max))
             "")
      (move-marker ilog-recent-commands-messages-marker (point-max)))))

(defun ilog-entering-password-p ()
  "Whether the user is currently entering a password."
  (eq (current-local-map) read-passwd-map))

(defun ilog-record-this-command ()
  "Push info about the current command to `ilog-recent-commands'."
  (push (list (if (ilog-entering-password-p) [??] ;hide passwords!
                (apply #'vector
                       (mapcar
                        (lambda (key) (if (consp key) ;; (mouse-... event-data)
                                     (car key)
                                   key))
                        (this-command-keys-vector))))
              (if (ilog-entering-password-p) "(entering-password)" this-command)
              (buffer-name)
              (ilog-get-last-messages)
              ""
              nil nil)
        ilog-recent-commands)
  (setq ilog-temp-load-hist nil))

(defun ilog-post-command ()
  "DTRT after a command was executed.
Goes to `post-command-hook'."
  (when ilog-recent-commands
    (callf concat (nth 4 (car ilog-recent-commands)) (ilog-get-last-messages))
    (setf (nth 5 (car ilog-recent-commands)) ilog-last-command-changed-buffer-p)
    (setq ilog-last-command-changed-buffer-p nil)
    ;; handle load-tree
    (setf (nth 6 (car ilog-recent-commands)) (ilog-parse-load-tree))
    (setq ilog-temp-load-hist nil)))

(defun ilog-update-log-buffer ()
  "Transform and insert pending data into the log buffer."
  (let* ((ilog-buffer
          (or (get-buffer ilog-buffer-name)
              (with-current-buffer (generate-new-buffer ilog-buffer-name)
                (setq truncate-lines t)
                (set (make-local-variable 'scroll-margin) 0)
                (current-buffer))))
         (ilog-buffer-visible-p nil) (wins-to-scroll ()) ateobp)
    (with-current-buffer ilog-buffer
      (when ilog-tail-mode
        (let ((point-max (point-max)))
          (mapc (lambda (frame)
                  (mapc (lambda (win)
                          (when (eq (window-buffer win) ilog-buffer)
                            (setq ilog-buffer-visible-p t)
                            (when (= (window-point win) point-max)
                              (push win wins-to-scroll))))
                        (window-list frame)))
                (frame-list))
          (unless wins-to-scroll (setq ateobp (= (point) (point-max))))))
      (setq ateobp (eobp))
      (let ((ilog-changing-log-buffer-p t) (deactivate-mark nil) (inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (dolist (entry (nreverse ilog-recent-commands))
            (destructuring-bind (key cmd buf pre-mes post-mes chg load-levels) entry
              (insert (if (looking-back "\\`\\|\n") "" "\n")
                      (ilog-format-messages pre-mes)
                      (propertize (key-description key)
                                  'face (case chg
                                          ((t)    'ilog-change-face)
                                          ((echo) 'ilog-echo-face)
                                          (t      'ilog-non-change-face)))
                      " " (format "%s" cmd) " " (format "\"%s\"" buf)
                      (if post-mes "\n")
                      (ilog-format-messages post-mes load-levels))
              (deactivate-mark t)))
          (setq ilog-recent-commands ())))
      (when ilog-tail-mode
        (if ilog-buffer-visible-p
            (dolist (win wins-to-scroll)
              (set-window-point win (point-max)))
          (when ateobp (goto-char (point-max))))))))

(defun ilog-cut-surrounding-newlines (string)
  "Cut all newlines at beginning and end of STRING.
Return the result."
  (when (string-match "\n+\\'" string)
    (setq string (substring string 0 (match-beginning 0))))
  (when (string-match "\\`\n+" string)
    (setq string (substring string (match-end 0))))
  string)

(defun ilog-format-messages (string &optional load-levels)
  "Format and propertize messages in STRING."
  (if (and (stringp string) (not (equal string "")))
      (let ((messages (ilog-cut-surrounding-newlines string)))
        (concat
         (mapconcat
          (lambda (line)
            (let ((load-mesg-p (when (get-text-property 0 'load-message line)
                                 (prog1 (car load-levels)
                                   (callf cdr load-levels)))))
              (concat (if load-mesg-p (make-string load-mesg-p ?\ ) "")
               (propertize line
                           'face (if load-mesg-p 'ilog-load-face 'ilog-message-face)))))
          (split-string messages "\n")
          "\n")
         "\n"))
    ""))

(defun ilog-note-buffer-change (&rest _)
  "Remember that this command changed any buffer.
Also remember whether this command caused any output in the Echo
Area."
  ;; I could alternatively use `command-error-function' for catching
  ;; errors
  (when (and (not ilog-changing-log-buffer-p)
             ilog-recent-commands)
    (if (string-match "\\` \\*Echo Area" (buffer-name))
        (setq ilog-last-command-changed-buffer-p 'echo)
      (setq ilog-last-command-changed-buffer-p (not (minibufferp))))))

(defun ilog-truncate-log-buffer ()
  "Truncate the log buffer to `ilog-log-max' lines."
  (let ((buf (get-buffer ilog-buffer-name)))
    (when (and buf
               (not (eq buf (current-buffer))) ; avoid truncation when log buffer is current
               (numberp ilog-log-max))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-max))
            (forward-line (- ilog-log-max))
            (delete-region (point-min) (point))))))))



(provide 'interaction-log)


;;; interaction-log.el ends here

;;; shell-command-queue.el --- Queue shell commands for execution
;;
;; Copyright (C) 2008 Mathias Dahl
;;
;; Version: 0.1
;; Keywords: processes, shell
;; Author: Mathias Dahl <mathias.rem0veth1s.dahl@gmail.com>

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;;  This package provides a means to add shell commands to a queue for
;;  "sequentual asynchronous execution".  This means that each command
;;  is run asynchronously from Emacs (so that Emacs is not locked up),
;;  but successive commands are not executed before the previous ones
;;  have finished.

;;  If you want to try it out, load this file and evaluate the
;;  following expressions:

;;   (shell-command-queue-clear)                ;; Clear the queue
;;   (shell-command-queue-add "s5" "sleep 5")   ;; Add a simple sleep command
;;   (shell-command-queue-add "s10" "sleep 10") ;; And another one
;;   (shell-command-queue-run)                  ;; Start executing

;;  If a command fails the execution stops and leaves the failing
;;  command as the current command (so that you can look at what it
;;  was and what was wrong with it).  You can try this by adding the
;;  following simple command somewhere in the middle of the queue:

;;   (shell-command-queue-add "test of a failing command" "exit 5")

;;  The output from each command, if any, ends up in the buffer named
;;  by `shell-command-queue-output-buffer'.  A convenience command,
;;  `shell-command-queue-show-output' is available for switching to
;;  this buffer.

;;; History:

;;   * Sun Mar 23 21:35:15 2008, Mathias Dahl, Created.

;;; Code:

(defvar shell-command-queue nil
  "Queue of shell commands to be executed after each other.")

(defvar shell-command-queue-output-buffer
  "*shell-command-queue-output*"
  "Buffer where each shell command's output ends up.")

(defun shell-command-queue-clear ()
  "Clear command queue."
  (interactive)
  (setq shell-command-queue nil))

(defun shell-command-queue-add (name command)
  "Add and give NAME to COMMAND."
  (interactive
   (list (read-string "Give the command a name: ")
         (read-string "Shell command to enqueue: ")))
  (setq shell-command-queue (append shell-command-queue
                                    (list (cons name command)))))

(defun shell-command-queue-run ()
  "Start executing shell commands from queue."
  (interactive)
  (if shell-command-queue
      (let* ((queue-item (car shell-command-queue))
             (name (car queue-item))
             (command (cdr queue-item)))
        (shell-command-queue-log-message
         (format "** Starting command: %s" command))
        (setq proc (start-process (format "shell-command-%s" name)
                                  shell-command-queue-output-buffer
                                  shell-file-name
                                  shell-command-switch
                                  command))
        (set-process-sentinel proc 'shell-command-queue-sentinel))
    (message "Command queue is empty.")))

(defun shell-command-queue-log-message (message)
  "Logs MESSAGE to the output buffer."
  (save-excursion
    (set-buffer (get-buffer-create shell-command-queue-output-buffer))
    (goto-char (point-max))
    (insert (concat "\n" message "\n\n"))))

(defun shell-command-queue-sentinel (process signal)
  "Handle PROCESS signaling SIGNAL."
  (let ((sig (substring signal 0 -1))
        (command (car (cdr (cdr (process-command process))))))
    (cond ((string= sig "finished")
           (pop shell-command-queue)
           (cond (shell-command-queue
                  (shell-command-queue-run))
                 ((null shell-command-queue)
                  (message "Shell command queue, all done.")
                  (shell-command-queue-log-message "** All done."))))
          ((not (string= sig "finished"))
           (message "Shell command queue, *** Command failed.")
           (shell-command-queue-log-message
            "** *** Command failed. Stopping queue execution.")))))

(defun shell-command-queue-list-queue ()
  "List current command queue."
  (interactive)
  (message
   (if shell-command-queue
       (concat "Commands in queue (NAME: COMMAND):\n"
               (mapconcat
                (lambda (x)
                  (format "%s: %s" (car x) (cdr x)))
                shell-command-queue "\n"))
     "Command queue is empty.")))

(defun shell-command-queue-show-output ()
  "Show output."
  (interactive)
  (pop-to-buffer shell-command-queue-output-buffer))

(provide 'shell-command-queue)

;;; shell-command-queue.el ends here

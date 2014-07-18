;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dashbuffer.el
;;; https://github.com/etherald/dashbuffer
;;; run elisp to create an informational buffer in your emacs

;;; to use, say like:
;;; (load "~/elisp/dashbuffer.el")
;;; (add-hook 'before-make-frame-hook 'dashbuffer)

;;; TODO: rationalize update options
;;; TODO: pass function from init file
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq debug-on-error t)

(defcustom dashbuffer-name "*Dashboard*"
  "Name for the dashbuffer buffer."
  :group 'dashbuffer
  :type 'string)

(defcustom dashbuffer-update-interval 10
  "Interval in seconds between dashbuffer updates."
  :group 'dashbuffer
  :type 'integer)

(defcustom dashbuffer-idle-interval 30
  "Interval in seconds for idle wait."
  :group 'dashbuffer
  :type 'integer)

(defcustom dashbuffer-auto-update nil
  "Whether the dashbuffer should update on a timer or display only once."
  :group 'dashbuffer
  :type 'boolean)

(defcustom dashbuffer-start-when-idle nil
  "Whether to update the buffer only after Emacs has been idle for the specified time.
Defaults to true. Otherwise the buffer will update immediately."
  :group 'dashbuffer
  :type 'boolean)

;;; cache ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar dashbuffer-timer nil)
(defvar dashbuffer-itself nil)

;;; entry point ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dashbuffer ()
  "Make a working dashbuffer appear in the Emacs."
  (interactive)
  (if (get-buffer dashbuffer-name)
      (pop-to-buffer dashbuffer-name  nil t)
    (dashbuffer-create))
  (if dashbuffer-start-when-idle
      (run-with-idle-timer dashbuffer-idle-interval t 'dashbuffer-update)))

(defun dashbuffer-update ()
  "Update buffer with newest info. And do it again if we're supposed to."
  ;;  (interactive)
  (dashbuffer-update-once)
  (if dashbuffer-auto-update
      (dashbuffer-reset-timer)))

(defun dashbuffer-update-once ()
  "Update the buffer just once."
  (with-local-quit
    ;;(undo-boundary)
    (save-selected-window
;;      (setq buffer-read-only nil)
      (dashbuffer-write-content dashbuffer-itself)
      (set-buffer-modified-p nil)
      ;;      (setq buffer-read-only t)
      ;;(shrink-dashbuffer)
      )))

(defun dashbuffer-create ()
  "Create the Dashbuffer."
  (setq dashbuffer-itself
        (get-buffer-create dashbuffer-name))
  ;;(display-buffer dashbuffer-name)
  (pop-to-buffer dashbuffer-name)
  (view-buffer dashbuffer-itself)
  (buffer-disable-undo dashbuffer-name)
  (set-window-dedicated-p (get-buffer-window dashbuffer-itself) t)
  (dashbuffer-update-once)
  (if dashbuffer-auto-update
      (dashbuffer-reset-timer))
  (fit-window-to-buffer (get-buffer-window dashbuffer-name))
  (shrink-window-if-larger-than-buffer (get-buffer-window dashbuffer-name)))

(defun dashbuffer-kill-buffer ()
  "Kill the Dashbuffer."
  (kill-buffer dashbuffer-name))

(defun dashbuffer-cleanup ()
  "Cleanup on exit function."
  (dashbuffer-cancel-timer)
  (setq dashbuffer-itself nil)
  (setq dashbuffer-timer nil))

(defun refresh-dashbuffer ()
  (interactive)
  (dashbuffer-update))

(defun kill-dashbuffer ()
  "Kill the Dashbuffer interactively."
  (interactive)
  (dashbuffer-kill-buffer))

(defun kill-dashbuffer-delete-window ()
  "Kill the Dashbuffer and close its window."
  (interactive)
  (delete-window (get-buffer-window dashbuffer-name))
  (kill-dashbuffer))

(defun bury-dashbuffer ()
  "Bury Alive the Dashbuffer."
  (interactive)
  (quit-window nil (get-buffer-window dashbuffer-itself))
  (bury-buffer dashbuffer-itself))

(defun shrink-dashbuffer-window ()
  (interactive)
  (fit-window-to-buffer (get-buffer-window dashbuffer-name))
  (shrink-window-if-larger-than-buffer (get-buffer-window dashbuffer-name)))

(defun dashbuffer-reset-timer ()
  "Reset and restart dashbuffer's timer."
  (dashbuffer-cancel-timer)
  (setq dashbuffer-timer
        (run-at-time dashbuffer-update-interval nil 'dashbuffer-update)))

(defun dashbuffer-cancel-timer ()
  "Cancel the timer and set its holder to nil."
  (if (dashbuffer-timer-exists)
      (progn
        (cancel-timer dashbuffer-timer)
        (setq dashbuffer-timer nil))))

(defun dashbuffer-timer-exists ()
  "Is there Dashbuffer Timer now?"
  (memq dashbuffer-timer timer-list))

(defun dashbuffer-write-line (str)
  "Send some text to the dashbuffer."
  (set-buffer dashbuffer-name)
  (setq buffer-read-only nil)
  ;;(princ (format "%s\n" str) dashbuffer-itself)
  (princ str (get-buffer dashbuffer-name))
  (setq buffer-modified-p nil)
  (setq buffer-read-only t))

(defun dashbuffer-write-content (buf)
  "Write a complete new Dashbuffer."
  ;;(require 'calendar)
  (require 'solar)
  ;;(set-buffer buf)
  (set-buffer (get-buffer dashbuffer-name))
  (setq buffer-read-only nil)
  (erase-buffer)
  (progn
    (dashbuffer-write-line
     (format-time-string "As of %D at %T:\n"))
    (dashbuffer-write-line
     (format "%s\n" (solar-sunrise-sunset-string (calendar-current-date))))
    (dashbuffer-write-line
     (format "System uptime: %s" (shell-command-to-string "uptime")))
    (dashbuffer-write-line
     (format "Emacs (server) has been running for %s\n" (emacs-uptime)))
    (dashbuffer-write-line
     (format "There are %d open buffers\n" (length (buffer-list))))
    (if dashbuffer-auto-update
        (dashbuffer-write-line
         (format "Your lucky number for the next %d seconds is %d\n" dashbuffer-update-interval (random))))
    (dashbuffer-write-line (yow)))
  (goto-char 0)
  (setq buffer-read-only t))

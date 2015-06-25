;;; selection-menu.el --- "generic" menu to choose one string.
;;;
;;; Author: Tomi Ollila -- too ät iki piste fi

;;; License: GPLv2+

;; read-key is available in emacs 23.2 & newer...
(if (fboundp 'read-key)
    (defalias 'selection-menu--read-key 'read-key)
  (defalias 'selection-menu--read-key
    (lambda (msg) (aref (read-key-sequence-vector msg) 0))))

;; popup.el or company.el could give insight how to "improve"
;; key reading (and get mouse events into picture, too)
;; (or maybe mouse events could already be read but how to handle...)

(defun selection-menu--select (ident &optional unread)
  (let ((helpmsg "Type ESC to abort, Space or Enter to select.")
        (buffer-read-only t)
        first last overlay pevent select)
    (forward-line -1)
    (setq last (point))
    (goto-char (point-min))
    (setq first (1+ (point)))
    (save-window-excursion
      (pop-to-buffer (current-buffer))
      (setq mode-name "Selection Menu"
            mode-line-buffer-identification (concat "*" ident "*"))
      (setq overlay (make-overlay (point) (line-end-position)))
      (overlay-put overlay 'face 'highlight)
      (while
          (let ((event (selection-menu--read-key helpmsg)))
            (cond ((or (eq event 'up) (eq event 16))
                   (when (> (point) first)
                     (forward-line -1)
                     (move-overlay overlay (point) (line-end-position)))
                   t)
                  ((or (eq event 'down) (eq event 14))
                   (when (< (point) last)
                     (forward-line)
                     (move-overlay overlay (point) (line-end-position)))
                   t)
                  ((or (eq event 32) (eq event 13) (eq event 'return))
                   (setq select
                         (buffer-substring (1+ (point))
                                           (line-end-position)))
                   nil)
                  ((eq event 'escape)
                   nil)
                  (t (setq pevent event)
                     nil)
                  ))))
    (if (and unread pevent)
        (push pevent unread-command-events))
    (message nil)
    select))

(defun selection-menu (ident items &optional unread)
  "Pops up a buffer listing lines given ITEMS one per line.
Use arrow keys (or C-p/C-n) to select and SPC/RET to select.
Return to parent buffer when any other key is pressed.
In this case if optional UNREAD is non-nil return the
read key back to input queue for parent to consume."
  (if (eq (length items) 0) nil
    (save-excursion
      (with-temp-buffer
        (dolist (item items) (insert " " item "\n"))
        (selection-menu--select ident unread)))))

;;(selection-menu "foo" (list))
;;(selection-menu "foo" (list "a"))
;;(selection-menu "Send mail to:" (list "a" "b" "c" "d" "faaarao") t)
;; test by entering c-x c-e at the end of previous lines

(provide 'selection-menu)

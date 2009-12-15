;;;

(provide 'elscreen-buffer-list)
(require 'elscreen)

;; From escreen.el
(defun buffer-dead-p (buffer)
  (not (static-if (fboundp 'buffer-live-p)
           (buffer-live-p buffer)
         ;; Emacs 18 doesn't have buffer-live-p.
         ;; Killed buffers have no names.
         (buffer-name buffer))))

;; From escreen.el
(defun reorder-buffer-list (new-list)
  "Set buffers in NEW-LIST to be the most recently used, in order."
  (when new-list
    (let (firstbuf buf)
      (while new-list
        (setq buf (car new-list))
        (when (stringp buf)
          (setq buf (get-buffer buf)))
        (unless (buffer-dead-p buf)
          (bury-buffer buf)
          (unless firstbuf
            (setq firstbuf buf)))
        (setq new-list (cdr new-list)))
      (setq new-list (buffer-list))
      (while (not (eq (car new-list) firstbuf))
        (bury-buffer (car new-list))
        (setq new-list (cdr new-list))))))


(defun elscreen-get-buffer-list (screen)
  "Return buffer-list of SCREEN."
  (let ((screen-property (elscreen-get-screen-property screen)))
    (get-alist 'buffer-list screen-property)))

(defun elscreen-set-buffer-list (screen buflist)
  "Set buffer-list of SCREEN to BUFLIST."
  (let ((screen-property (elscreen-get-screen-property screen)))
    (set-alist 'screen-property 'buffer-list buflist)
    (elscreen-set-screen-property screen screen-property)))

(defun elscreen-save-buffer-list (&optional screen)
  "Save the buffer-list order for SCREEN, or current screen"
  (elscreen-set-buffer-list (or screen
                                (elscreen-get-current-screen))
                            (buffer-list)))

(defun elscreen-load-buffer-list (&optional screen)
  "Set emacs' buffer-list order to that of SCREEN, or current screen"
  (reorder-buffer-list (elscreen-get-buffer-list
                        (or screen
                            (elscreen-get-current-screen)))))

(defadvice elscreen-goto-internal (around manage-buffer-list activate)
  "Manage screen-specific buffer lists."
  (when (and elscreen-buffer-list-enabled
             (elscreen-screen-live-p (elscreen-get-previous-screen)))
    (elscreen-save-buffer-list (elscreen-get-previous-screen)))
  ad-do-it
  (when elscreen-buffer-list-enabled
    (elscreen-load-buffer-list (elscreen-get-current-screen))))

(defcustom elscreen-buffer-list-enabled nil
  "Whether to save and load screen-local buffer lists."
  :type 'boolean
  :group 'elscreen)

(defun toggle-elscreen-buffer-list (&optional arg)
  (interactive "P")
  (setq elscreen-buffer-list-enabled
        (cond ((null arg) (not elscreen-buffer-list-enabled))
              (> arg 0)))
  (message "Screen-specific buffer lists %s"
	   (if elscreen-buffer-list-enabled "enabled" "disabled")))

(fset 'enable-elscreen-buffer-list
      (apply-partially 'toggle-elscreen-buffer-list 1))
(fset 'disable-elscreen-buffer-list
      (apply-partially 'toggle-elscreen-buffer-list 0))

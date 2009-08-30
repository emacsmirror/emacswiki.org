;;;; jump-or-exec.el ---- Jump to a buffer or create it.
;;;;
;;;; This is enlightened by jump-or-exec in sawfish.
;;;;
;;;; Usage: (jump-or-exec "buffer name" func-to-create-the-buffer
;;;; func-on-switched-to)
;;;;
;;;; The function jump-or-exec will first search the buffer list with
;;;; buffer-name, it will switch to that buffer when found, and excute func
;;;; when not found. The optional arg onfocused is used to be excuted when the
;;;; buffer is already current buffer.
;;;;
;;;; I don't know how to use regexp when seaching the buffer list, may
;;;; anyone finish it? Thanks!
;;;;
;;;; Versions:
;;;; 2007.02.05  <jay.w.xie AT gmail.com>
;;;;             the first version
;;;;

(defun jump-or-exec (buffer-name func &optional onfocused)
  "jump to a buffer matched by buffer-name, or call function otherwise."
  (interactive)
  (if (or (stringp buffer-name) (bufferp buffer-name))
      (let ((buffer-matched (get-buffer buffer-name)))
        (if (bufferp buffer-matched)
            (if (string= (buffer-name buffer-matched)
                         (buffer-name (current-buffer)))
                (if (functionp onfocused) (funcall onfocused buffer-matched))
              (switch-to-buffer buffer-matched))
          (if (functionp func) (funcall func buffer-name))))
    (if (functionp func) (funcall func buffer-name))))

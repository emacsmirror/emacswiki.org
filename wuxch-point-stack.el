;;; codes for point-stack

(global-set-key [(control \,)] 'wuxch-point-stack-push)
(global-set-key [(control .)] 'wuxch-point-stack-pop)

;; it's better to add face to the point which in stack
(defvar wuxch-point-stack nil)

(defun get-overlay-pos (pos)
  "get-overlay-pos:get overlay position by cursor's position"
  (if (and (is-line-end pos) (not (is-line-begin pos)))
      (- pos 1)
    pos
    )
  )

(defun wuxch-point-stack-push ()
  (interactive)
  (message "Location marked.")
  (let* ((buf (current-buffer))
         (statck-point (point))
         (overlay-pos (get-overlay-pos statck-point))
         (overlay)
         )

    (setq overlay (make-overlay overlay-pos (+ 1 overlay-pos) buf))
    (overlay-put overlay 'face 'bm-face)
    (setq wuxch-point-stack (cons (list buf statck-point) wuxch-point-stack)))
  )

(defun wuxch-point-stack-pop ()
  (interactive)
  (if (null wuxch-point-stack)
      (message "Stack is empty.")
    (progn
      (let ((stack-point)
            (overlay-pos)
            (overlay))

        (switch-to-buffer (caar wuxch-point-stack))
        (setq stack-point (cadar wuxch-point-stack))
        (setq wuxch-point-stack (cdr wuxch-point-stack))

        (setq overlay-pos (get-overlay-pos stack-point))

        (if (setq overlay (wuxch-check-pos-overlay-face overlay-pos 'bm-face))
            (delete-overlay overlay)
          )
        (goto-char stack-point)
        (etags-select-highlight overlay-pos (+ 1 overlay-pos))
        )
      )
    )
  )

(defun wuxch-gothrough-the-whole-buffer-and-check-overlay (specail-face)
  "wuxch-gothrough-the-whole-buffer-and-check-overlay:"
  ;; (if (yes-or-no-p "point of stack changed, redo it from the top of buffer?")
  (progn
    (let ((pos (point-min)))
      (while (and (< pos (point-max))
                  (eq (wuxch-check-pos-overlay-face pos specail-face) nil)
                  )
        (setq pos (+ pos 1))
        )
      (if (< pos (point-max))
          (progn
            (goto-char pos)
            (delete-overlay (wuxch-check-pos-overlay-face pos specail-face))
            )
        )
      )
    )
  ;; )
  )


(defun wuxch-check-pos-overlay-face (pos specail-face)
  "wuxch-check-pos-overlay-face: check the overlay of position,
if the face is equals specail-face, return it, otherwise return
nil"
  (let ((overlays (overlays-at pos))
        (ret nil)
        )
    (while overlays
      (let ((overlay (car overlays)))
        (if (eq (overlay-get overlay 'face) specail-face)
            (setq ret overlay))
        (setq overlays (cdr overlays))
        )
      )
    ret
    )
  )

(provide 'wuxch-point-stack)

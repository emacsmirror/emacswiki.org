;;; function for fingle

(require 'fringe-helper)

;; (setq indicate-empty-lines t)
;; (setq indicate-buffer-boundaries t)

(fringe-helper-define 'wuxch-flagfringe 'center
  "........"
  ".XXXXXX."
  ".XXXXXX."
  ".XXXXXX."
  ".XXXXXX."
  ".X......"
  ".X......"
  ".X......"
  ".X......"
  ".X......"
  )

(defface wuxch-flagfringe-face
  '((t (t :background "gray75"))) ""
  )

(custom-set-faces
 '(wuxch-flagfringe-face ((t (:foreground "yellow"))) t)
 )

(defun fringe-add()
  (interactive)
  ;; (do-fringe-add (point) 'hs-marker 'hs-fringe-face)
  (fringe-helper-insert 'wuxch-flagfringe (point) 'left-fringe 'wuxch-flagfringe-face)
  )

(defun fringe-remove ()
  "fringe-remove:"
  (interactive)
  (do-fringe-remove (point))
  )

(defun do-fringe-add (pos fringe-bitmap fringe-face)
  "fringe-add:"
  (let* ((cur-pos (point))
         (marker-string "*fringe-dummy*")
         (marker-length (length marker-string))
         (ov nil)
         (line-begin-pos))
    (goto-char pos)
    (setq line-begin-pos (line-beginning-position))
    (setq ov (make-overlay line-begin-pos
                           (+ line-begin-pos 1)
                           (current-buffer)))
    (put-text-property 0 marker-length 'display
                       (list 'left-fringe fringe-bitmap fringe-face)
                       marker-string)
    (overlay-put ov 'before-string marker-string)
    (goto-char cur-pos)
    )
  )

(defun do-fringe-remove (pos)
  (let ((cur-pos (point))
        (line-begin-pos)
        (overlay-list)
        (display-prop)
        )
    (goto-char pos)
    (setq line-begin-pos (line-beginning-position))
    (setq overlay-list (overlays-at line-begin-pos))
    (while overlay-list
      (let ((overlay (car overlay-list)))
        (setq display-prop (overlay-get overlay 'before-string))
        (when display-prop
          (delete-overlay overlay)
          )
        (setq overlay-list (cdr overlay-list))
        )
      )
    (goto-char cur-pos)
    )
  )


(provide 'wuxch-fringe)

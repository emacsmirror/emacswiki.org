;;; configures and codes for outline mode
(require 'outline)
(require 'outline+)

;; 在outline mode下使用word-wrap，而不是line-wrap
;; (add-hook 'outline-mode-hook 'word-wrap-mode)

(defun my-outline-mode-hook ()
  ;; outline-face
  ;; (copy-face 'font-lock-builtin-face 'outline-1)
  )

(defun wuxch-outline-all-buffer-fringe ()
  "wuxch-outline-all-buffer-fringe:"
  (interactive)
  (let ((pos (point)))
    (goto-char (point-min))
    (wuxch-outline-fringe)
    (while (outline-next-heading)
      (wuxch-outline-fringe)
      )
    (goto-char pos)
    )
  )

(defun wuxch-outline-fringe ()
  "wuxch-outline-fringe:"
  ;; (interactive)
  ;; (when (outline-on-heading-p)
  (outline-back-to-heading)
  (let ((pos (line-end-position)))
    ;; (message "pos is %d" pos)
    (if (outline-invisible-p pos)
        (progn
          (do-fringe-add (point) 'hs-marker 'hs-fringe-face)
          )
      (progn
        (fringe-remove)
        )
      )
    )
  ;; )
  )

(defun wuxch-outline-view-change-hook ()
  (when (string= major-mode "latex-mode")
    (wuxch-outline-all-buffer-fringe)
    )
  )
(add-hook 'outline-view-change-hook 'wuxch-outline-view-change-hook)

(add-hook 'outline-mode-hook 'my-outline-mode-hook)

(define-key outline-mode-map [(control left)] 'outline-promote)
(define-key outline-mode-map [(control right)] 'outline-demote)
(define-key outline-mode-map [(control x) (left)] 'hide-subtree)
(define-key outline-mode-map [(control x) (right)] 'show-subtree)

(define-key outline-mode-map [(control meta left)] 'hide-sublevels)
(define-key outline-mode-map [(control meta right)] 'show-all)
;; (define-key outline-mode-map [(control down)] 'wuxch-outline-goto-next-entry)
;; (define-key outline-mode-map [(control up)] 'wuxch-outline-goto-previous-entry)

(defun wuxch-outline-goto-next-entry (arg)
  ""
  (interactive "p")
  (hide-entry)
  (outline-next-visible-heading arg)
  (show-entry)
  )

(defun wuxch-outline-goto-previous-entry (arg)
  ""
  (interactive "p")
  (hide-entry)
  (outline-previous-visible-heading arg)
  (show-entry)
  )

(defun wuxch-outline-mark-subtree-extend (&optional arg allow-extend)
  "wuxch-outline-mark-subtree-extend:copy and modify from mark-sexp. wuxch 05/04/2008 11:15:35"
  (interactive "P\np")
  (cond ((and allow-extend
              (or (and (eq last-command this-command) (mark t))
                  (and transient-mark-mode mark-active)))
         (setq arg (if arg (prefix-numeric-value arg)
                     (if (< (mark) (point)) -1 1)))
         (set-mark
          (save-excursion
            (goto-char (mark))
            (outline-next-visible-heading arg)
            (point))))
        (t
         (push-mark
          (save-excursion
            (outline-next-visible-heading (prefix-numeric-value arg))
            (point))
          nil t)))
  )

(provide 'wuxch-outline)

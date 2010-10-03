;;; imenu config file, also for ecb
(add-to-list 'load-path (concat emacs-site-lisp-dir "tree-widget"))

(require 'imenu-tree)

;; (global-set-key [(f3)]  'wuxch-imenu-tree-open)
;; (global-set-key [(meta f3)]  'wuxch-imenu-tree-kill)

(defconst imenu-timer-interval 1)
(defvar imenu-timer-id nil)

(defun wuxch-imenu-tree-open (arg)
  "wuxch-imenu-tree-open:"
  (interactive "P")
  (if (eq imenu-tree-buffer nil)
      (progn
        (let ((buf (current-buffer)))
          (imenu-tree arg)
          (select-window (get-buffer-window buf))
          (open-node-by-function-name)
          (setq imenu-timer-id
                (run-with-idle-timer imenu-timer-interval t
                                     'open-node-by-function-name))
          (make-local-variable 'imenu-timer-id)
          )
        )
    (if (get-buffer-window imenu-tree-buffer)
        (progn
          (message "imenu-tree is already opened")
          )
      (let ((buf (current-buffer)))
        (imenu-tree arg)
        (select-window (get-buffer-window buf))
        (open-node-by-function-name)
        (setq imenu-timer-id
              (run-with-idle-timer imenu-timer-interval t
                                   'open-node-by-function-name))
        (make-local-variable 'imenu-timer-id)
        )
      )
    )
  )

(defun wuxch-imenu-tree-kill ()
  "wuxch-imenu-tree-kill:"
  (interactive)
  (imenu-tree-kill)
  (delete-other-windows)
  (when (timerp imenu-timer-id)
    (cancel-timer imenu-timer-id)
    (setq imenu-timer-id nil)
    (make-local-variable 'imenu-timer-id)
    )
  )

;; (custom-set-variables
;;  '(imenu-tree-auto-update t)
;;  '(imenu-tree-update-interval 1)    ;; second
;;  )

(require 'which-func)
(defun open-node-by-function-name ()
  "open-node-by-function-name:"
  (let ((function-name (car (which-function)))
        (current-win (get-buffer-window))
        (imenu-win (get-buffer-window imenu-tree-buffer))
        )
    (when (and function-name imenu-win)
      (with-selected-window imenu-win
        (let ((root (tree-mode-tree-ap))
              (previous-line-number (line-number-at-pos))
              (ov)(beg)(end))
          (when root
            (goto-char (widget-get root :from))
            )
          (when (re-search-forward (concat "[ ]\\(" function-name "\\)" "$") nil t)
            (setq beg (match-beginning 1))
            (setq end (match-end 1))
            (wuxch-gothrough-the-whole-buffer-and-check-overlay 'font-lock-warning-face)
            (goto-char beg)
            (setq ov (make-overlay beg end
                                   imenu-tree-buffer))
            (overlay-put ov 'face 'font-lock-warning-face)
            ;; (if (not (eq previous-line-number (line-number-at-pos beg)))
            ;;     (recenter-top-bottom)
            ;;   )
            )
          )
        )
      )
    )
  )

(defun imenu-tree-update-timer ()
  "Update and show the tree if needed."
  (imenu-tree-show)
  (when (and imenu-tree
             ;; the tree is visible
             (get-buffer-window imenu-tree-buffer)
             imenu-tree-need-update
             ;; the buffer is not too large
             (not (> (buffer-size) imenu-auto-rescan-maxout)))
    (setq imenu--index-alist nil)
    (imenu--make-index-alist t)
    (let ((tree imenu-tree))
      (with-current-buffer imenu-tree-buffer
        (goto-char (widget-get tree :from))
        (tree-mode-reflesh)))
    (setq imenu-tree-need-update nil))

  (open-node-by-function-name)

  )

(provide 'wuxch-imenu)

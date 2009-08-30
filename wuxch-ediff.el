;;; customize of ediff functions.

(require 'ediff)
(require 'dired)
(require 'dired-aux)

;; To make ediff operate on selected-frame add next:
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; To make ediff to be horizontally split use:
(setq ediff-split-window-function 'split-window-horizontally)

(custom-set-variables
 '(ediff-diff-options "--binary -w"))

(defun wuxch-ediff-keymap-setup-hook()
  (define-key ediff-mode-map "\\" 'ediff-toggle-split)
  (define-key ediff-mode-map "`" 'ediff-swap-buffers)
  (define-key ediff-mode-map [(control down)] 'ediff-next-difference)
  (define-key ediff-mode-map [(control up)] 'ediff-previous-difference)


  (cond (ediff-merge-job
         )
        (ediff-3way-comparison-job
         )
        (t
         ;; 2-way comparison
         (define-key ediff-mode-map [(control right)]  'ediff-copy-A-to-B)
         (define-key ediff-mode-map [(control left)]  'ediff-copy-B-to-A)
         )
        )

  )

(add-hook 'ediff-keymap-setup-hook 'wuxch-ediff-keymap-setup-hook)


(defun wuxch-ediff-meta-buffer-keymap-setup-hook ()
  (define-key ediff-meta-buffer-map "X" 'ediff-unhide-marked-sessions)
  (define-key ediff-meta-buffer-map "s" 'wuxch-ediff-sync-session)
  (define-key ediff-meta-buffer-map "r" 'wuxch-ediff-update)
  )
(add-hook 'ediff-meta-buffer-keymap-setup-hook 'wuxch-ediff-meta-buffer-keymap-setup-hook)

(defun ediff-unhide-marked-sessions()
  "ediff-unhide-marked-sessions:"
  (interactive)
  (ediff-hide-marked-sessions -1)
  )


(defun wuxch-ediff-sync-session()
  "for debug"
  (interactive)
  (let* ((pos (ediff-event-point last-command-event))
         (info (ediff-get-meta-info (current-buffer) pos 'noerror))
         (file-a-name (car (nth 2 info)))
         (file-b-name (car (nth 3 info)))
         )
    (wuxch-ediff-sync-file-by-modify-time file-a-name file-b-name)
    )
  )

(defun wuxch-ediff-sync-file-by-modify-time(file1-name file2-name)
  "sync-file-by-modify-time:"
  (let* ((comp (wuxch-compare-two-file-modify-time file-a-name file-b-name))
         )
    (cond ((= 0 comp)
           (message "do nothing because of the same modify time of tow files")
           )
          ((= 1 comp)
           (progn
             ;; file1 newer than file2
             (message "%s newer, sync with it" file1-name)
             (dired-copy-file file1-name file2-name t)
             )
           )
          ((= -1 comp)
           (progn
             ;; file1 newer than file2
             (message "%s newer, sync with it" file2-name)
             (dired-copy-file file2-name file1-name t)
             )
           )
          )
    )
  )

(defun wuxch-compare-two-file-modify-time(file1-name file2-name)
  "compare-two-file-modify-time: return: 1(file-name-a later) 0(equal) -1 (file-name-b later)"
  (let* ((file-a-modify-time (ediff-file-modtime file1-name))
         (file-b-modify-time (ediff-file-modtime file2-name))
         ;;time is second from sometime. format is (high . low)
         (a-high (nth 0 file-a-modify-time))(a-low (nth 1 file-a-modify-time))
         (b-high (nth 0 file-b-modify-time))(b-low (nth 1 file-b-modify-time))
         (ret)
         )
    (if (> a-high b-high)
        (setq ret 1)
      (progn
        (if (= a-high b-high)
            (progn
              (if (> a-low b-low)
                  (setq ret 1)
                (progn
                  (if (= a-low b-low)
                      (setq ret 0)
                    (setq ret -1)
                    )
                  )
                )
              )
          ;; means a-high < b-high
          (setq ret -1)
          )
        )
      )
    )
  )

(defun ediff-format-date (time)
  (format "%04d-%02d-%02d %s:%s:%s"
          (nth 5 time)                           ; year
          (nth 4 time)                           ; month
          (nth 3 time)                           ; day
          (ediff-fill-leading-zero (nth 2 time)) ; hour
          (ediff-fill-leading-zero (nth 1 time)) ; min
          (ediff-fill-leading-zero (nth 0 time)) ; sec
          ))

(defun wuxch-ediff-update ()
  (interactive)
  (ediff-update-meta-buffer (current-buffer) 'must-redraw)
  )

(defun wuxch-ediff-sync-marked-sessions()
  "wuxch-ediff-sync-marked-sessions:"
  (interactive "P")
  (let ((grp-buf (ediff-get-group-buffer ediff-meta-list))
        (meta-list (cdr ediff-meta-list))
        (numMarked 0)
        active-sessions-exist session-buf elt)
    )
  )


(defun wuxch-ediff2-dwim (arg1 arg2)
  "wuxch-ediff-dwim:"
  (let ((mode1 (buffer-local-value major-mode arg1))
        (mode2 (buffer-local-value major-mode arg2)))
    (cond
     ((and (eq mode1 'dired) (eq mode2 'dired))

      )
     )
    )
  )

(provide 'wuxch-diff)

Here is my (EmilioLopes') version of it, using a functional predicate for more flexible buffer selection:

    ;; (arrange-buffers (lambda (b) (with-current-buffer b (string-match "shell-mode" (symbol-name major-mode)))))
    ;; (arrange-buffers (lambda (b) (with-current-buffer b (string-match "org-mode" (symbol-name major-mode)))))

    (defun display-buffers-matching (predicate)
      "Displays all buffers matching PREDICATE.
    The function PREDICATE will be called with each buffer as its only argument."
      (let ((buffers
             (let (buffers)
               (dolist (buffer (buffer-list) buffers)
                 (when (funcall predicate buffer)
                   (setq buffers (append buffers `(,buffer)))))))
            (rows 1)
            (cols 1))
        (if (= 0 (length buffers))
            (message "No matching buffers.")

          (setq rows (floor (sqrt (length buffers))))
          (setq cols (ceiling (/ (length buffers) (float rows))))

          ;; split windows...
          (delete-other-windows)

          ;;create rows
          (dotimes (i (- rows 1))
            (split-window-vertically))
          ;; create cols
          (dotimes (j rows)
            (other-window -1)
            (dotimes (i (- cols 1))
              (split-window-horizontally)))

          ;; display buffers...
          (dolist (buffer buffers)
            (set-window-buffer nil buffer)
            (other-window 1))

          ;; remove empty windows (if cols*rows > length-of-buffers)
          (if (> (* cols rows) (length buffers))
              (dotimes (i (- (* cols rows) (length buffers)))
                (delete-window)))
          (balance-windows))))

-- EmilioLopes 2012-09-07 12:27 UTC


----

Why don't comment pages link back to the page in question?

-- RyanDavis 2012-09-08 07:11 UTC


----

At the bottom of the page, on the left, is a link back to the page in question. It does not say "Back" etc. It has just the page name. 

It took me a while also to figure this out. Perhaps it could be in a more prominent place, instead of mixed in with 13 other links.

-- DrewAdams 2012-09-08 13:36 UTC


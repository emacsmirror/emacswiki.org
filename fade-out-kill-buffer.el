;;; fade-out-kill-buffer.el -- fade to black when killing buffer
;; Ryan Yeske 20051013

(defun fade-out-kill-buffer (buffer)
  (interactive "bFade kill buffer: ")
  (with-current-buffer buffer
    (let ((str (buffer-substring
		(progn (move-to-window-line 0)
		       (point))
		(progn (move-to-window-line -1)
		       (point-at-eol)))))
      (when (kill-buffer buffer)
	(with-temp-buffer
	  (insert str)
	  (switch-to-buffer (current-buffer))
	  (goto-char (point-min))
	  (setq cursor-type nil)
	  (dotimes (i 20)
	    (put-text-property (point-min) (point-max)
			       'face (list :foreground 
					   (format "gray%d"
                                                  ;(* 5  (1+ i))))) ; for white background
						   (- 100 (* 5 (1+ i))))))
	    (sit-for 0)
	    (sleep-for .01)))))))

;;(global-set-key (kbd "C-x k") 'fade-out-kill-buffer)
(global-set-key (kbd "C-c k") 'fade-out-kill-buffer)

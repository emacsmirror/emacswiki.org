;;; Jump to a random line
; sburke@cpan.org

(defun goto-random-line ()
  "Go to a random line in this buffer."
  ; good for electrobibliomancy.
  (interactive)
  (goto-line (1+ (random (buffer-line-count)))))

(defun buffer-line-count () 
  "Return the number of lines in this buffer."
  (count-lines (point-min) (point-max)))

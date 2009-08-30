;;; prev-next-buffer.el - Surrogates of `next-buffer' and `previous-buffer'

;; prev-next-buffer.el - Provides lightweight surrogates of Emacs 22's
;; functions `next-buffer' and `previous-buffer' to other Emacsen
;; Harmless if said functions are already defined, so inserting
;;
;     (require 'prev-next-buffer)
;;
;; in your .emacs shouldn't cause any incompatibility.
;;
;; Stewarded by dominique@quatravaux.org for Emacs Wiki; copyright might be traceable
;; to some guy in Ecole Normale Superieure (http://www.eleves.ens.fr/) circa 1990 (!).

(if (not (fboundp 'switch-to-other-buffer))
;; Code stolen Xemacs' files.el
(defun switch-to-other-buffer (arg)
  "Switch to the previous buffer.  With a numeric arg, n, switch to the nth
most recent buffer.  With an arg of 0, buries the current buffer at the
bottom of the buffer stack."
  (interactive "p")
  (if (eq arg 0)
      (bury-buffer (current-buffer)))
  (switch-to-buffer
   (if (<= arg 1) (other-buffer (current-buffer))
     (nth (1+ arg) (buffer-list)))))
)

(if (not (fboundp 'next-buffer))
    (defun next-buffer ()
  "Switch to the next buffer in cyclic order."
  (interactive)
  (switch-to-other-buffer 0)))

(if (not (fboundp 'previous-buffer))
     (defun previous-buffer ()
  "Switch to the previous buffer in cyclic order."
  (interactive)
  (while (string-match "\\` "
                       (buffer-name (switch-to-other-buffer
                                     (- (length (buffer-list)) 2)))))))

(provide 'prev-next-buffer)

;;; prev-next-buffer.el ends here

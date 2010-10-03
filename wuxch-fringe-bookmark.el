;;; Filename:wuxch-fringe-bookmark.el
;;; display a little flag on fringe for bookmark

(require 'fringe-helper)

(fringe-helper-define 'fringe-bookmark-flag 'center
  "........"
  "XXXXXXXX"
  "XXXXXXXX"
  "XXXXXXXX"
  "XXXXXXXX"
  "X......."
  "X......."
  "X......."
  "X......."
  "X......."
  )

(defface fringe-bookmark-flag-face
  '((t (t :background "gray75"))) ""
  )

(custom-set-faces
 '(fringe-bookmark-flag-face ((t (:foreground "yellow"))) t)
 )

(defvar fringe-bookmark-repository nil
  "Alist with all fringe bookmark data.")

;; data struct

;; fringe-bookmark-repository:
;; (("wuxch-fringe-bookmark.el"
;;  (85 2273 #<overlay from 2273 to 2273 in wuxch-fringe-bookmark.el>)
;;  (68 1662 #<overlay from 1662 to 1662 in wuxch-fringe-bookmark.el>))
;; ("wuxch-bookmark.el"
;;  (10 258 #<overlay from 258 to 258 in wuxch-bookmark.el>)
;;  (25 907 #<overlay from 907 to 907 in wuxch-bookmark.el>)))

;; markset:
;; ("wuxch-fringe-bookmark.el"
;;  (85 2273 #<overlay from 2273 to 2273 in wuxch-fringe-bookmark.el>)
;;  (68 1662 #<overlay from 1662 to 1662 in wuxch-fringe-bookmark.el>))

;; markitem
;;  (85 2273 #<overlay from 2273 to 2273 in wuxch-fringe-bookmark.el>)

(defun fringe-bookmark-new-markitem ()
  (let ((line (line-number-at-pos))
        (pos (point)))
    (list line
          pos
          (fringe-helper-insert 'fringe-bookmark-flag pos 'left-fringe 'fringe-bookmark-flag-face))
    )
  )


(defun fringe-bookmark-get-markset-by-buffer (buf-name)
  "fringe-bookmark-get-markset-by-buffer:"
  (assoc buf-name fringe-bookmark-repository)
  )

(defun fringe-bookmark-get-markitem-by-line (markset line)
  "fringe-bookmark-get-markitem-by-line:"
  (if markset
      (assoc line markset)
    nil)
  )

(defun fringe-bookmark-put-first-item-to-last (markset)
  (let ((item (car (cdr markset))))
    (setcdr markset (cdr (cdr markset)))
    (setq markset (append markset (list item)))
    (fringe-bookmark-update-markset-of-repository markset)
    )
  )

(defun fringe-bookmark-remove-item-from-markset (markset item)
  "fringe-bookmark-remove-item-from-markset:"
  (let ((new-markset (remove item markset)))
    (fringe-bookmark-update-markset-of-repository new-markset)
    (fringe-helper-remove (car (cdr (cdr item))))
    )
  )

(defun fringe-bookmark-add-item-to-repository (buf-name item)
  "fringe-bookmark-add-item-to-repository:"
  (let ((new-markset (fringe-bookmark-get-markset-by-buffer buf-name)))
    (if (eq new-markset nil)
        (setq new-markset (cons buf-name (list item)))
      (setq new-markset (append new-markset (list item)))
      )
    (fringe-bookmark-update-markset-of-repository new-markset)
    )
  )

(defun fringe-bookmark-update-markset-of-repository (new-markset)
  (let* ((old-markset (fringe-bookmark-get-markset-by-buffer (car new-markset))))
    (setq fringe-bookmark-repository (remove old-markset fringe-bookmark-repository))
    (setq fringe-bookmark-repository (append fringe-bookmark-repository (list new-markset)))
    )
  )

(defun fringe-bookmark-goto-next-bookmark ()
  "goto-next-bookmark:"
  (interactive)
  (let* ((markset (fringe-bookmark-get-markset-by-buffer (buffer-name)))
         (item)
         )
    (setq item (car (cdr markset)))
    (when item
      (goto-char (car (cdr item)))
      (fringe-bookmark-put-first-item-to-last markset)
      )
    )
  )

(defun fringe-bookmark-toggle-bookmark ()
  "toggle-bookmark:"
  (interactive)
  (let* ((current-line (line-number-at-pos))
         (current-pos (point))
         (buf-name (buffer-name))
         (markset (fringe-bookmark-get-markset-by-buffer buf-name))
         (markitem (fringe-bookmark-get-markitem-by-line markset current-line))
         )

    (if markitem
        (fringe-bookmark-remove-bookmark markset markitem)
      (fringe-bookmark-add-bookmark buf-name (fringe-bookmark-new-markitem))
      )
    )
  )

(defun fringe-bookmark-remove-bookmark (markset markitem)
  "remove-bookmark:"
  (fringe-bookmark-remove-item-from-markset markset markitem)
  )

(defun fringe-bookmark-add-bookmark (buf-name new-markitem)
  (fringe-bookmark-add-item-to-repository buf-name new-markitem)
  )

(provide 'wuxch-fringe-bookmark)

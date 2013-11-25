;;; image-dimensions-minor-mode.el
;;
;; Adds the image dimensions in the mode line, when viewing an image.
;;
;; Author: Phil S.
;;
;; Compatibility: GNU Emacs 24

;; Commentary:
;;
;; To also see this information in the frame title, your `frame-title-format'
;; variable might be something like the following:
;;
;; (setq frame-title-format
;;       '(buffer-file-name
;;         ("%b (Emacs) %f" image-dimensions-minor-mode-dimensions)
;;         (dired-directory
;;          (:eval (concat (buffer-name) " (Emacs) " dired-directory))
;;          ("%b (Emacs)"))))

(defvar-local image-dimensions-minor-mode-dimensions nil
  "Buffer-local image dimensions for `image-dimensions-minor-mode'")

(define-minor-mode image-dimensions-minor-mode
  "Displays the image dimensions in the mode line."
  :init-value nil
  :lighter image-dimensions-minor-mode-dimensions
  (require 'image-mode)
  (require 'cl) ;; destructuring-bind
  (when (not image-dimensions-minor-mode-dimensions)
    (let ((image (image-get-display-property)))
      (when image
        (setq image-dimensions-minor-mode-dimensions
              (destructuring-bind (width . height)
                  (image-size image :pixels)
                (format " (%dx%d)" width height)))))))

(add-hook 'image-mode-hook 'image-dimensions-minor-mode)

(provide 'image-dimensions-minor-mode)

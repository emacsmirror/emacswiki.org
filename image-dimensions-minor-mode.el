;;; image-dimensions-minor-mode.el
;;
;; Display the image dimensions in the mode line, when viewing an image.
;;
;; Author: Phil S.
;;
;; Compatibility: GNU Emacs 24.3
;;
;; Installation:
;; (eval-after-load 'image-mode '(require 'image-dimensions-minor-mode))

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

(eval-when-compile (require 'cl-macs))

(declare-function image-get-display-property "image-mode")

(defvar-local image-dimensions-minor-mode-dimensions nil
  "Buffer-local image dimensions for `image-dimensions-minor-mode'")

(define-minor-mode image-dimensions-minor-mode
  "Displays the image dimensions in the mode line."
  :init-value nil
  :lighter image-dimensions-minor-mode-dimensions
  (when (not image-dimensions-minor-mode-dimensions)
    (let ((image (image-get-display-property)))
      (when image
        (setq image-dimensions-minor-mode-dimensions
              (cl-destructuring-bind (width . height)
                  (image-size image :pixels)
                (format " (%dx%d)" width height)))))))

(add-hook 'image-mode-hook 'image-dimensions-minor-mode)

(provide 'image-dimensions-minor-mode)

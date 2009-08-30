;;; visible-mark.el --- Make marks visible.

;;; Commentary:

;; This was hacked together by Jorgen Schäfer
;; And hacked again by Yann Hodique
;; Donated to the public domain. Use at your own risk.

;;; History:
;; 2008-02-21  MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
;;
;;      * visible-mark.el: Added function to inhibit trailing overlay.
;;
;; 2008-01-31  MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
;;
;;      * visible-mark.el: Create formal emacs lisp file from
;;        http://www.emacswiki.org/cgi-bin/wiki/VisibleMark.
;;        Yann Hodique and Jorgen Schäfer are original authors.
;;        Added function to make multiple marks visible.
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup visible-mark nil
  "Show the position of your mark."
  :group 'convenience
  :prefix "visible-mark-")

(defface visible-mark-face
  `((((type tty) (class color))
     (:background "gray" :foreground "white"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "gray"))
    (((class color) (background light))
     (:background "grey80"))
    (t (:background "gray")))
  "Face for the mark."
  :group 'visible-mark)

(defvar visible-mark-overlays nil
  "The overlays used in this buffer.")
(make-variable-buffer-local 'visible-mark-overlays)

(defvar visible-mark-non-trailing-faces nil)

(defcustom visible-mark-inhibit-trailing-overlay t
  "If non-nil, inhibit trailing overlay with underline face."
  :group 'visible-mark
  :type 'boolean)

(defcustom visible-mark-max 1
  "A number of mark to be visible."
  :group 'visible-mark
  :type 'integer)

(defcustom visible-mark-faces nil
  "A list of mark faces."
  :group 'visible-mark
  :type '(repeat face))

(defcustom global-visible-mark-mode-exclude-alist nil
  "A list of buffer names to be excluded"
  :group 'visible-mark
  :type '(repeat regexp))

(defun visible-mark-initialize-faces ()
  (if (and visible-mark-inhibit-trailing-overlay
           (null visible-mark-non-trailing-faces))
      (let (faces)
        (dotimes (i visible-mark-max)
          (let ((face (or (nth i visible-mark-faces) 'visible-mark-face))
                (symbol (intern (format "visible-mark-non-trailing-face%s" i))))
            (copy-face face symbol)
            (set-face-attribute symbol nil
                                :foreground (or (face-attribute face :background) t)
                                :background 'unspecified
                                :underline t)
            (push symbol faces)))
        (setq visible-mark-non-trailing-faces (nreverse faces)))))
                  
(defun visible-mark-initialize-overlays ()
  (mapcar 'delete-overlay visible-mark-overlays)
  (let (overlays)
    (dotimes (i visible-mark-max)
      (let ((overlay (make-overlay (point-min) (point-min))))
        (push overlay overlays)))
    (setq visible-mark-overlays (nreverse overlays))))

(defun visible-mark-move-overlays ()
  "Move the overlay in `visible-mark-overlay' to a new position."
  (let ((marks (cons (mark-marker) mark-ring))
        (overlays visible-mark-overlays))
    (dotimes (i visible-mark-max)
      (let* ((mark (car-safe marks))
             (overlay (car overlays))
             (pos (and mark (marker-position mark))))
        (when pos
          (overlay-put overlay 'face
                       (if (and visible-mark-inhibit-trailing-overlay
                                (save-excursion
                                  (goto-char pos)
                                  (eolp)))
                           (nth i visible-mark-non-trailing-faces)
                         (or (nth i visible-mark-faces) 'visible-mark-face)))
          (move-overlay overlay pos (1+ pos)))
        (setq marks (cdr marks)))
        (setq overlays (cdr overlays)))))

(require 'easy-mmode)

(defun visible-mark-mode-maybe ()
  (when (cond
         ((minibufferp (current-buffer)) nil)
         ((flet ((fun (arg)
                      (if (null arg) nil
                        (or (string-match (car arg) (buffer-name))
                            (fun (cdr arg))))))
            (fun global-visible-mark-mode-exclude-alist)) nil)
         (t t))
    (visible-mark-mode)))

(define-minor-mode visible-mark-mode
  "A mode to make the mark visible."
  nil nil nil
  :group 'visible-mark
  (if visible-mark-mode
      (progn
        (visible-mark-initialize-faces)
        (visible-mark-initialize-overlays)
        (add-hook 'post-command-hook 'visible-mark-move-overlays nil t))
    (mapcar 'delete-overlay visible-mark-overlays)
    (setq visible-mark-overlays nil)
    (remove-hook 'post-command-hook 'visible-mark-move-overlays t)))

(define-global-minor-mode
  global-visible-mark-mode visible-mark-mode visible-mark-mode-maybe
  :group 'visible-mark)

(provide 'visible-mark)
;;; visible-mark.el ends here

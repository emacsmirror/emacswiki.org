;;; highlight-sexp.el
;; Copyright (C) 2011 Grégoire Jadi

;; Author: Grégoire Jadi <gregoire.jadi@gmail.com>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defgroup highlight-sexp nil
  "Highlight sexp"
  :group 'faces
  :group 'matching)

(defvar hl-sexp-overlay
  nil
  "The current overlay.")
(make-variable-buffer-local 'hl-sexp-overlay)

(defcustom hl-sexp-background-color
  "#4b3b4b"
  "*The color used for the background of the highlighted sexp."
  :type 'color
  :group 'highlight-sexp)

(defcustom hl-sexp-foreground-color
  nil
  "*The color used for the foreground of the highlighted sexp"
  :type 'color
  :group 'highlight-sexp)

(make-face 'hl-sexp-face)
(defcustom hl-sexp-face
  nil
  "*The face used for the highlighted sexp."
  :group 'highlight-sexp)

(define-minor-mode highlight-sexp-mode
    "Minor mode to highlight the current zone according to its
    context, i.e. sexp, comment, string."
  nil
  " hl-sexp"
  nil
  (if highlight-sexp-mode
      (progn
        (hl-sexp-create-overlay)
        (add-hook 'post-command-hook 'hl-sexp-highlight nil t))
      (hl-sexp-delete-overlay)
      (kill-local-variable 'hl-sexp-overlay)
      (remove-hook 'post-command-hook 'hl-sexp-highlight t)))

(define-globalized-minor-mode global-highlight-sexp-mode
    highlight-sexp-mode
  (lambda ()
    (highlight-sexp-mode t)))

(defun hl-sexp-delete-overlay ()
  (if hl-sexp-overlay
      (delete-overlay hl-sexp-overlay))
  (setq hl-sexp-overlay nil))

(defun hl-sexp-highlight ()
  (let ((text-property (get-text-property (point) 'face)))
    ;; HACKY HACK: just in case, this avoid to go further.
    (cond ((or (eq text-property 'font-lock-string-face)
                (eq text-property 'font-lock-comment-face))
           (move-overlay hl-sexp-overlay 0 0))
          (t
           (save-excursion
            (ignore-errors
             (let* ((sppss (syntax-ppss))
                    (start (elt sppss 1))
                    (inside-a-string? (elt sppss 3))
                    (inside-a-comment? (elt sppss 4))
                    end)
               ;; 'font-lock-****-face isn't really to be trusted
               (cond ((and start
                           (not inside-a-string?)
                           (not inside-a-comment?))
                      (ignore-errors
                       (setq end (scan-sexps start 1)))
                      (cond (end
                             (move-overlay hl-sexp-overlay (1+ start) (1- end)))
                            (t
                             (move-overlay hl-sexp-overlay (1+ start) (point)))))
                     (t (move-overlay hl-sexp-overlay 0 0))))))))))

(defun hl-sexp-create-overlay ()
  (let (attribute)
    (setq attribute (face-attr-construct 'hl-sexp-face))
    (if hl-sexp-foreground-color
        (setq attribute (plist-put attribute :foreground hl-sexp-foreground-color)))
    (if hl-sexp-background-color
        (setq attribute (plist-put attribute :background hl-sexp-background-color)))
    (setq hl-sexp-overlay (make-overlay 0 0))
    (overlay-put hl-sexp-overlay 'face attribute)))

(provide 'highlight-sexp)

;;; end of highlight-sexp.el

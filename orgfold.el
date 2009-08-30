;;; orgfold.el
;; Copyright (C) 2009 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;
;; Proof of concept implementation of saving folding information in
;; org buffers.
;; 

(require 'cl)


(defvar orgfold-saved-info-tag "saved org fold info: ")



(defun orgfold-before-save ()
  (save-excursion
    (goto-char (point-min))
    (let ((index 0)
          indices)

      (unless (looking-at outline-regexp)
        (outline-next-visible-heading 1))

      (while (not (eobp))
        (unless (some (lambda (o) (overlay-get o 'invisible))
                      (overlays-at (line-end-position)))
          (push index indices))
        (outline-next-visible-heading 1)
        (incf index))

      (setq indices (prin1-to-string (nreverse indices)))

      (goto-char (point-min))
      (if (search-forward orgfold-saved-info-tag nil t)
          (kill-line)

        (goto-char (point-max))
        (insert "\n\n;; don't delete these lines\n"
                orgfold-saved-info-tag))

      (insert indices "\n"))))


(defun orgfold-restore ()
  (save-excursion
    (goto-char (point-min))
    (let ((index 0)
          (indices (if (search-forward orgfold-saved-info-tag nil t)
                       (read (current-buffer)))))
      (if (not indices)
          (message "no saved folding information in the file")

        (show-all)
        (goto-char (point-min))

        (unless (looking-at outline-regexp)
          (outline-next-visible-heading 1))

        (while (and indices
                    (not (eobp)))
          (if (eq index (car indices))
              (pop indices)
            (hide-subtree))

          (outline-next-visible-heading 1)
          (incf index))

        (message "restored saved folding")))))
                  


(add-hook 'org-mode-hook 'orgfold-activate)

(defun orgfold-activate ()
  (orgfold-restore)
  (add-hook 'before-save-hook 'orgfold-before-save nil t))


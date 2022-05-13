;;; orgfold.el

;; DON'T CLICK DOWNLOAD !!!

;; BEWARE THAT WHEN CLICKING THE DOWNLOAD/GIT BUTTON ON EMACS-WIKI,
;; IT DOES NOT TAKE YOU TO THIS VERSION OF THE FILE


;; Copyright (C) 2009-2022

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
;; org buffers. This variant saves folding information in a separate
;; file.
;; 

(defun orgfold-get-fold-info-file-name ()
  (concat (buffer-file-name) ".fold"))

(defun orgfold-save ()
  (save-excursion
    (goto-char (point-min))

    (let (foldstates)
      (unless (looking-at outline-regexp)
        (outline-next-visible-heading 1))

      (while (not (eobp))
        (push (when (seq-some (lambda (o) (overlay-get o 'invisible))
                         (overlays-at (line-end-position)))
                t)
              foldstates)
        (outline-next-visible-heading 1))

      (with-temp-file (orgfold-get-fold-info-file-name)
        (prin1 (nreverse foldstates) (current-buffer))))))

(defun orgfold-restore ()
  (save-excursion
    (goto-char (point-min))
    (let* ((foldfile (orgfold-get-fold-info-file-name))
           (foldstates
            (when (file-readable-p foldfile)
              (with-temp-buffer
                (insert-file-contents foldfile)
                (when (> (buffer-size) 0)
                  (read (current-buffer)))))))

      (when foldstates
        (show-all)
        (goto-char (point-min))

        (unless (looking-at outline-regexp)
          (outline-next-visible-heading 1))

        (while (and foldstates
                    (not (eobp)))
          (when (pop foldstates)
            (hide-subtree))

          (outline-next-visible-heading 1))

        (message "restored saved folding")))))


(add-hook 'org-mode-hook 'orgfold-activate)

(defun orgfold-activate ()
  (orgfold-restore)
  (add-hook 'kill-buffer-hook 'orgfold-kill-buffer nil t))

(defun orgfold-kill-buffer ()
  ;; don't save folding info for unsaved buffers
  (unless (buffer-modified-p)
    (orgfold-save)))

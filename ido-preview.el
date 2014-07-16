;;; ido-preview.el --- commands for viewing current option contests before opening
;;
;; Copyright 2012 Horishnii Oleksii
;;
;; Author: Horishnii Oleksii <desu@horishniy.org.ua>
;; Version: 0.1
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Commentary:
;;
;; This package provides `ido-preview-forward' and
;; `ido-preview-backward' for viewing contests of current item you
;; choose. It shows file content for files, function description for
;; functions, buffer content for buffers.
;; Please, send bug reports to my email.
;;
;; Add something like the following to your .emacs:
;;
;;; Warning: this bind my functions to C-p and C-n, and rebind
;;; *already bound* in ido C-p and C-n functions to C-M-p and C-M-n.
;;; If you don't want that hackery behavior, bind it to keys you like.
;; (add-hook 'ido-setup-hook
;;   (lambda()
;;     (define-key ido-completion-map (kbd "C-M-p") (lookup-key ido-completion-map (kbd "C-p")))
;;     (define-key ido-completion-map (kbd "C-M-n") (lookup-key ido-completion-map (kbd "C-n"))) ; currently, this makes nothing. Maybe they'll make C-n key lately.
;;     (define-key ido-completion-map (kbd "C-p") 'ido-preview-backward)
;;     (define-key ido-completion-map (kbd "C-n") 'ido-preview-forward)))
;;
;; Since 24.3 flet is deprecated(thanks Ming for pointing
;; that out), so using noflet now. Download it from
;; reference section.
;;
;; References:
;; https://github.com/nicferrier/emacs-noflet -- noflet, required
;; http://www.emacswiki.org/emacs/ido-preview.el -- self-reference
;; http://www.emacswiki.org/emacs/InteractivelyDoThings -- ido main page
;;
;;; Code:

(require 'noflet)

(defun ido-preview-cond()
  "Function using lot of dynamic variables inside.
Return string.
The general rule: (car ido-matches) - item we are watching here."
  (cond
    ((and file
       (file-exists-p file)
       (not (file-directory-p file))
       ido-current-directory)
      (save-excursion
        (save-window-excursion
          (when (find-file file)
            (prog1 (buffer-substring (point-min) (point-max)) (kill-buffer))))))
    ((consp (car ido-matches))
      (cond
        ((and (cdar ido-matches) (stringp (cadar ido-matches))) ; interaction with kill-ring-ido.el
          (cadar ido-matches))
        (t (ido-name ido-matches))))
    ((and (stringp (car ido-matches)) (functionp (intern (car ido-matches))))
      (noflet ((message(&rest args))) (save-window-excursion (describe-function (intern (car ido-matches))))))
    ((and (stringp (car ido-matches)) (bufferp (get-buffer (car ido-matches))))
      (save-excursion (set-buffer (get-buffer (car ido-matches))) (buffer-substring (point-min) (point-max))))
    ((stringp (car ido-matches)) (car ido-matches))
    (t "No matches, sir.")))

(defun ido-preview-forward(&optional arg)
  "Complete the minibuffer contents as far as possible.
Return nil if there is no valid completion, else t.
If no characters can be completed, display a list of possible completions.
If you repeat this command after it displayed such a list,
scroll the window of possible completions."
  (interactive "P")
  ;; If the previous command was not this,
  ;; mark the completion buffer obsolete.
  (unless (or (equal 'ido-preview-forward last-command) (equal 'ido-preview-backward last-command))
    (setq minibuffer-scroll-window nil))

  (let ((window minibuffer-scroll-window))
    ;; If there's a fresh completion window with a live buffer,
    ;; and this command is repeated, scroll that window.
    (if (window-live-p window)
      (with-current-buffer (window-buffer window)
        (scroll-other-window arg)
        nil)
      (progn
        (let ((enable-recursive-minibuffers t)
               (file (ido-name (car ido-matches))))
          (if file
            (setq file (concat ido-current-directory file)))
          (get-buffer-create " *preview-ido*")
          ;; using the fact we are in (save-window-excursion) while browsing files.
          (other-window 1) ; currently we are at minibuffer-window, so jump at real buffer
          (delete-other-windows) ; AHAHAHA~!
          (setq minibuffer-scroll-window (get-buffer-window (switch-to-buffer " *preview-ido*"))) ;set current window the thing we preview, and setting minibuffer-scroll-window to scroll.
          (other-window -1)
          (with-current-buffer " *preview-ido*"
            (text-mode)
            (erase-buffer)
            (insert (ido-preview-cond))
            (ignore-errors (set-auto-mode))))))))

(defun ido-preview-backward(arg)
  "Complete the minibuffer contents as far as possible.
Return nil if there is no valid completion, else t.
If no characters can be completed, display a list of possible completions.
If you repeat this command after it displayed such a list,
scroll the window of possible completions."
  (interactive "P")

  ;; If the previous command was not this,
  ;; mark the completion buffer obsolete.
  (unless (or (equal 'ido-preview-forward last-command) (equal 'ido-preview-backward last-command))
    (setq minibuffer-scroll-window nil))

  (let ((window minibuffer-scroll-window))
    ;; If there's a fresh completion window with a live buffer,
    ;; and this command is repeated, scroll that window.
    (if (window-live-p window)
      (with-current-buffer (window-buffer window)
        (scroll-other-window
          ;; reversing argument
          (cond
            ((not arg) '-)
            (t (- arg))))
        nil)
      (progn
        (let ((enable-recursive-minibuffers t)
               (file (ido-name (car ido-matches))))
          (if file
            (setq file (concat ido-current-directory file)))
          (get-buffer-create " *preview-ido*")
          ;; using the fact we are in (save-window-excursion) while browsing files.
          (other-window 1) ; currently we are at minibuffer-window, so jump at real buffer
          (delete-other-windows) ; AHAHAHA~!
          (setq minibuffer-scroll-window (get-buffer-window (switch-to-buffer " *preview-ido*"))) ;set current window the thing we preview, and setting minibuffer-scroll-window to scroll.
          (other-window -1)
          (with-current-buffer " *preview-ido*"
            (text-mode)
            (erase-buffer)
            (insert (ido-preview-cond))
            (ignore-errors (set-auto-mode))))))))

(provide 'ido-preview)

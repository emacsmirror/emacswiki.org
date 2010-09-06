;;; comint-kill-output-to-kill-ring.el ---  Kill last comint output and put it in the kill ring
;; -*- Mode: Emacs-Lisp -*-

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;; Usage:
;;
;; Typically bound to to the comint-mode-map as follows:
;;
;; (add-hook 'comint-mode-hook
;;   (lambda()
;;    (define-key comint-mode-map [(control c) (control o)] 'comint-kill-output-to-kill-ring)))
;;
;; History:
;;    2010-09-06: Initial version <dov.grobgeld@gmail.com>
;;
(defun comint-kill-output-to-kill-ring ()
  "Kills all output from last command and puts it in kill buffer
Does not delete the prompt."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer)))
        (replacement nil)
        (inhibit-read-only t))
    (save-excursion
      (let ((pmark (progn (goto-char (process-mark proc))
                          (forward-line 0)
                          (point-marker))))
        ;; Add the text to the kill ring.
        (copy-region-as-kill comint-last-input-end pmark)
        (delete-region comint-last-input-end pmark)
        (goto-char (process-mark proc))
        (setq replacement (concat "*** output flushed to kill ring ***\n"
                                  (buffer-substring pmark (point))))
        (delete-region pmark (point))))
    ;; Output message and put back prompt
    (comint-output-filter proc replacement)))

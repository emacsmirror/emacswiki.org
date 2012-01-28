;;; tellicopy.el --- A minor mode for saving the thing at point to `kill-ring'

;; Author: Leo <sdl.web@gmail.com>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This code is directly copied out of my init file and the code has
;; been in intensive daily use with incremental improvements over
;; some months (~ 2 years).

;; Originally it was just a function that encompasses all the
;; features I want. It has been re-designed to make it easier to
;; extend.

;; FEATURES:

;; 1. M-w copies one of URL EMAIL or LINE; C-u 20 copies following 20
;;    lines; negative prefix is accepted as well.

;; 2. M-w follow by one of the chars: l, f, w, s, d copies the
;;    corresponding thing at point as in `thing-at-point-alist'. For
;;    example, `M-w l' will copy the whole thing enclosed in
;;    parentheses, in LaTeX it also copies the whole inline formula
;;    at point if any. It accepts prefix too i.e. 'M-w 2 w' copies
;;    two words at point. For convenience 'C-u 2 M-w w' does the
;;    same.

;; I will develop the code to reside in the tellicopy namespace
;; later on. -- Leo (2010-06-04)

(eval-when-compile (require 'cl))
(require 'thingatpt)

(global-set-key (kbd "M-w") 'save-thing-at-point)

(defvar thing-at-point-alist
  '((?l . list) (?f . filename) (?w . word) (?s . sexp) (?d . defun))
  "A list of (Key . THING).")

(defvar save-thing-at-point-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "-" 'negative-argument)
    (define-key map [t] 'save-thing-at-point-exit)
    (define-key map (vector meta-prefix-char t) 'save-thing-at-point-exit)
    (define-key map "\M-w" 'save-thing-at-point)
    (mapc (lambda (c) (define-key map (char-to-string c) 'digit-argument))
          (number-sequence ?0 ?9))
    (mapc (lambda (c)
            (define-key map (char-to-string c) 'save-thing-at-point-select))
          (mapcar 'car thing-at-point-alist))
    map)
  "Keymap for `save-thing-at-point-mode'.")

(defun save-thing-at-point-exit ()
  "Exit `save-thing-at-point-mode' and pass on the `last-input-event'."
  (interactive)
  (save-thing-at-point-mode -1)
  (push last-input-event unread-command-events))

(defun save-thing-at-point-select (&optional n)
  (interactive "p")
  (let* ((thing (cdr (assoc last-command-event thing-at-point-alist)))
         (bounds (bounds-of-thing-at-point thing))
         beg end)
    (if (and thing bounds)
        (progn
          (unless (memq (abs n) '(0 1))
            (ignore-errors
              (save-excursion
                (if (> n 0)
                    (unless (= (point) (car bounds)) (forward-thing thing -1))
                  (unless (= (point) (cdr bounds)) (forward-thing thing 1)))
                (setq beg (point))
                (forward-thing thing n)
                (setq end (point)))))
          (unless (and beg end)
            (setq beg (car bounds)
                  end (cdr bounds)))
          (message "%s" (kill-new (buffer-substring beg end) 'replace)))
      (message "No `%s' at point." thing))))

(define-minor-mode save-thing-at-point-mode
  "A minor mode for saving the thing at point to `kill-ring'.

\\{save-thing-at-point-mode-map}"
  :lighter ""
  (add-to-list 'minor-mode-overriding-map-alist
               `(save-thing-at-point-mode . ,save-thing-at-point-mode-map)))

(defun save-thing-at-point (&optional n)
  (interactive "p")
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end))
    (setq n (or n 1))
    (let ((guess (loop for thing in '(url email line)
                       for bounds = (bounds-of-thing-at-point thing)
                       for tap = (when bounds
                                   (if (= n 1)
                                       (thing-at-point thing)
                                     (save-excursion
                                       (buffer-substring (beginning-of-thing thing)
                                                         (progn (forward-thing thing n)
                                                                (point))))))
                       when tap
                       do (when (and (eq thing 'line)
                                     (>= (length tap) 1)
                                     (equal (substring tap -1) "\n"))
                            (setq tap (substring tap 0 -1)))
                       (return tap))))
      (message "%s" (kill-new guess))
      (save-thing-at-point-mode t)
      (setq prefix-arg current-prefix-arg))))

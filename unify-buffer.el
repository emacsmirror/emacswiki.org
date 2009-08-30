;;; unify-buffer.el --- Concatenate multiple buffers
;; $Id: unify-buffer.el,v 1.5 2008/11/27 19:33:23 rubikitch Exp $

;; Copyright (C) 2008  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: convenience
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/unify-buffer.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Concatenate multiple buffers into one buffer.
;; It is useful to view multiple buffers simultaneously.

;; This package provides two commands:
;;  `unify-buffers': unifies buffers
;;  `unify-file-buffers': unifies buffers with file-name

;;; History:

;; $Log: unify-buffer.el,v $
;; Revision 1.5  2008/11/27 19:33:23  rubikitch
;; Implemented `unify-buffer-goto'
;;
;; Revision 1.4  2008/11/27 19:12:16  rubikitch
;; New command: `unify-buffer-next-header', `unify-buffer-previous-header'
;; Renamed command: `unify-buffers-goto' -> `unify-buffer-goto'
;;
;; Revision 1.3  2008/11/27 13:06:44  rubikitch
;; added commentary
;;
;; Revision 1.2  2008/11/27 10:16:14  rubikitch
;; New command `unify-file-buffers'
;;
;; Revision 1.1  2008/11/27 10:10:12  rubikitch
;; Initial revision
;;

;;; Code:

(defvar unify-buffer-version "$Id: unify-buffer.el,v 1.5 2008/11/27 19:33:23 rubikitch Exp $")
(eval-when-compile (require 'cl))

(defface unify-buffer-header-face
  '((t (:background "white" :foreground "black" :weight bold)))
  "Face for the header."
  )

(defmacro ub-aif (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))  
(put 'ub-aif 'lisp-indent-function 2)

;; (unify-buffers "test" "*scratch*" "unify-buffer.el")
;; (kill-buffer "test")
(defun unify-buffers (&optional unify-buffer-name &rest buffers)
  "Concatenate multiple buffers into one big buffer. Then display it."
  (interactive)
  (unless buffers
    (setq buffers
          (loop for b = (read-buffer "Unify Buffer: " "")
                until (string= b "")
                collect b)))
  (unless unify-buffer-name
    (setq unify-buffer-name
          (read-string "Unify Buffer name: " nil nil "*Unify Buffer*")))
  (pop-to-buffer (apply 'unify-buffers-noselect unify-buffer-name buffers)))

(defun unify-buffers-noselect (unify-buffer-name &rest buffers)
  (with-current-buffer (generate-new-buffer unify-buffer-name)
    (dolist (b buffers)
      (setq b (if (stringp b) (get-buffer b) b))
      (insert (propertize (concat (or (buffer-file-name b)
                                      (buffer-name b))
                                  "\n")
                          'face 'unify-buffer-header-face
                          'unify-buffer-header t
                          'unify-buffer-origbuf b
                          'unify-buffer-origfile (buffer-file-name b)))
      (save-excursion (insert-buffer-substring b))
      (put-text-property (point) (point-max) 'unify-buffer-origbuf b)
      (put-text-property (point) (point-max) 'unify-buffer-origfile
                         (buffer-file-name b))
      (goto-char (point-max))
      (unless (bolp)
        (newline)))
    (goto-char (point-min))
    (unify-buffers-mode t)
    (current-buffer)))

;; (unify-file-buffers "unify-files" "~/.emacs" "~/.zshrc" "~/.screenrc")
;; (kill-buffer "unify-files")
(defun unify-file-buffers (&optional unify-buffer-name &rest files)
  "Concatenate multiple file buffers into one big buffer. Then display it."
  (interactive)
  (unless files
    (setq files
          (loop for f = (read-file-name "Unify File Buffer: " nil "")
                until (string= f "")
                collect f)))
  (unless unify-buffer-name
    (setq unify-buffer-name
          (read-string "Unify File Buffer name: " nil nil "*Unify Buffer*")))
  (pop-to-buffer (apply 'unify-file-buffers-noselect unify-buffer-name files)))

(defun unify-file-buffers-noselect (unify-buffer-name &rest files)
  (apply 'unify-buffers-noselect unify-buffer-name
         (mapcar 'find-file-noselect files)))

(defun unify-buffer-header-p (position)
  (get-text-property position 'unify-buffer-header))

(defun unify-buffer-next-header ()
  (interactive)
  (when (unify-buffer-header-p (point))
    (forward-line 1))
  (ub-aif (next-single-property-change (point) 'unify-buffer-header)
      (goto-char it)
    (forward-line -1)))

(defun unify-buffer-previous-header ()
  (interactive)
  (when (unify-buffer-header-p (point))
    (forward-line -1))
  (ub-aif (previous-single-property-change (point) 'unify-buffer-header)
      (goto-char it))
  (forward-line -1))

(defun unify-buffer-goto ()
  "Go to the original buffer."
  (interactive)
  (let ((origpt (point))
        (pt (if (unify-buffer-header-p (point))
                1
              (- (point) (previous-single-property-change (point) 'unify-buffer-header) -1)))
        (wstart (window-start))
        (origbuf (get-text-property (point) 'unify-buffer-origbuf))
        (origfile (get-text-property (point) 'unify-buffer-origfile)))
    (if origfile
        (find-file origfile)
      (switch-to-buffer origbuf))
    (goto-char pt)
    (set-window-start (selected-window) (- pt (- origpt wstart)))))

(defvar unify-buffers-mode-map (make-sparse-keymap))
(define-key unify-buffers-mode-map "\C-m" 'unify-buffer-goto)
(define-key unify-buffers-mode-map "}" 'unify-buffer-next-header)
(define-key unify-buffers-mode-map "{" 'unify-buffer-previous-header)


;; unify-buffers-mode is a minor-mode because future version will
;; inherit original major modes.
(define-minor-mode unify-buffers-mode
  "Unify buffers minor mode."
  nil nil unify-buffers-mode-map
  (cond (unify-buffers-mode
         (view-mode 1))
        (t
         (view-mode 0))))
         

(provide 'unify-buffer)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "unify-buffer.el")
;;; unify-buffer.el ends here

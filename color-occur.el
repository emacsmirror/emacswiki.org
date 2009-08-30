;;; color-occur.el
;; -*- Mode: Emacs-Lisp -*-

;;  $Id: color-occur.el,v 2.2 2005/05/14 03:05:59 akihisa Exp $

;; Author: Matsushita Akihisa <akihisa@mail.ne.jp>
;; Keywords: occur highlight convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; color-occur highlight occur buffer and file buffer. Furthermore
;; color-occur show the matching line of file buffer in other window.

;;; Install:

;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;;     (require 'color-occur)

;; The latest version of this program can be downloaded from
;; http://www.bookshelf.jp/elc/color-occur.el

;; Usage:
;; occur <regexp> shows all occurrences of <regexp> in current buffers
;; C-c C-c or RET gets you to the occurrence
;; q : quit
;; <up>, n : next matches
;; <down>, p : previous matches

;;; History:

;; color-occur 1.0 was released to the net on 12/01/2002

;;; Code:

(defface color-occur-face
  '((((class color)
      (background dark))
     (:background "SkyBlue" :bold t :foreground "Black"))
    (((class color)
      (background light))
     (:background "ForestGreen" :bold t))
    (t
     ()))
  "Face used for list-matching-lines-face")

(setq list-matching-lines-face 'color-occur-face)

;;; Internal variables
(defvar before-occur-use-migemo nil)
(defvar before-occur-point nil)
(defvar before-occur-buffer nil)
(defvar before-occur-word "")
(defvar color-occur-overlays nil)
(defvar color-occur-underline-overlays nil)
(defvar occur-buffer nil)
(make-variable-buffer-local 'occur-buffer)

(define-key occur-mode-map '[up] 'color-occur-prev)
(define-key occur-mode-map '[down] 'color-occur-next)
(define-key occur-mode-map "\M-n" 'color-occur-next)
(define-key occur-mode-map "\M-p" 'color-occur-prev)
(define-key occur-mode-map "\C-v" 'color-occur-scroll-up)
(define-key occur-mode-map "\M-v" 'color-occur-scroll-down)
(define-key occur-mode-map "n" 'color-occur-next)
(define-key occur-mode-map "p" 'color-occur-prev)
(define-key occur-mode-map "j" 'color-occur-next)
(define-key occur-mode-map "k" 'color-occur-prev)
(define-key occur-mode-map "q" 'color-occur-exit)
(define-key occur-mode-map " " 'color-occur-scroll-up)
(define-key occur-mode-map "b" 'color-occur-scroll-down)
(define-key occur-mode-map "\C-m" 'color-occur-mode-goto-occurrence)
(define-key occur-mode-map "\C-c\C-c" 'color-occur-mode-goto-occurrence)

(defun color-occur-color ()
  "*Highlight the file buffer"
  (let ((ov))
    (save-excursion
      (if before-occur-word
          (progn
            (goto-char (point-min))
            (while (re-search-forward before-occur-word nil t)
              (progn
                (setq ov (make-overlay (match-beginning 0)
                                       (match-end 0)))
                (overlay-put ov 'face 'color-occur-face)
                (overlay-put ov 'priority 0)
                (setq color-occur-overlays (cons ov color-occur-overlays))))
            (make-local-hook 'after-change-functions)
            (remove-hook 'after-change-functions 'color-occur-remove-overlays)
            ;;(add-hook 'after-change-functions 'color-occur-remove-overlays)
            )))))

(defun color-occur-remove-overlays (&optional beg end length)
  (interactive)
  (if color-occur-underline-overlays
      (progn
        (delete-overlay color-occur-underline-overlays)
        (setq color-occur-underline-overlays nil)))
  (if (and beg end (= beg end))
      ()
    (while color-occur-overlays
      (delete-overlay (car color-occur-overlays))
      (setq color-occur-overlays (cdr color-occur-overlays)))))

(defadvice occur
  (before save-excursion activate)
  (setq before-occur-buffer (current-buffer))
  (setq before-occur-point (point))
  (if before-occur-use-migemo
      (setq before-occur-word (migemo-get-pattern regexp))
    (setq before-occur-word regexp))
  (color-occur-color))

(defadvice occur
  (after color-occur-color activate)
  (if (get-buffer "*Occur*")
      (set-buffer (get-buffer "*Occur*")))
  (setq occur-buffer before-occur-buffer)
  (if (boundp 'fm-working)
      (setq fm-working nil))
  (color-occur-color))

(defadvice occur
  (after setq-last activate)
  (if (and (not (string= (buffer-name (current-buffer)) "Occur"))
           (get-buffer "*Occur*"))
      (switch-to-buffer-other-window (get-buffer "*Occur*"))))

(defadvice occur-mode-goto-occurrence
  (after pop-to-buffer activate)
  (if (featurep 'xemacs)
      (bury-buffer (get-buffer "*Occur*"))
    (color-occur-remove-overlays)
    (kill-buffer (get-buffer "*Occur*")))
;;  (switch-to-buffer before-occur-buffer)
  (delete-other-windows))

(defun color-occur-mode-goto-occurrence ()
  (interactive)
  (occur-mode-goto-occurrence)
  (if (featurep 'xemacs)
      (progn
        (color-occur-remove-overlays)
        (kill-buffer (get-buffer "*Occur*")))))

(defun color-occur-exit ()
  "*Exit occur buffer"
  (interactive)
  (kill-buffer (get-buffer "*Occur*"))
  (switch-to-buffer before-occur-buffer)
  (goto-char before-occur-point)
  (color-occur-remove-overlays)
  (delete-other-windows)
  )

(if (featurep 'xemacs)
    (progn
      (defun occur-mode-find-occurrence (line)
        (switch-to-buffer-other-window before-occur-buffer)
        (goto-line line)
        (switch-to-buffer-other-window (get-buffer "*Occur*")))

      (defun color-occur-next (&optional n)
        "Move to the Nth (default 1) next match in the *Occur* buffer."
        (interactive "p")
        (if (not n) (setq n 1))
        (forward-line n)
        (save-excursion
          (end-of-line)
          (if (re-search-backward "^[ ]*\\([0-9]+\\):" nil t)
              (setq line (string-to-int
                          (buffer-substring (match-beginning 1)
                                            (match-end 1))))))

        (occur-mode-find-occurrence line)

        (if (not color-occur-underline-overlays)
            (setq color-occur-underline-overlays
                  (make-overlay
                   (line-beginning-position) (1+ (line-end-position))))
          (move-overlay color-occur-underline-overlays
                        (line-beginning-position) (1+ (line-end-position))))
        (overlay-put color-occur-underline-overlays 'face 'underline)
        ;;(switch-to-buffer-other-window (get-buffer "*Occur*"))
        )

      (defun color-occur-prev (&optional n)
        "Move to the Nth (default 1) next match in the *Occur* buffer."
        (interactive "p")
        (if (not n) (setq n 1))
        (color-occur-next (- n))
        ))

  (defun color-occur-next (&optional n)
    "Move to the Nth (default 1) next match in the *Occur* buffer."
    (interactive "p")
    (if (not n) (setq n 1))
    (let ((r) (line nil) pos)
      (while (> n 0)
        (if (get-text-property (point) 'occur-target)
            (forward-char 1))
        (setq r (next-single-property-change (point) 'occur-target))
        (if r
            (progn
              (goto-char r)
              (save-excursion
                (if (re-search-backward "^[ ]*\\([0-9]+\\):" nil t)
                    (setq line (string-to-int
                                (buffer-substring (match-beginning 1)
                                                  (match-end 1))))))
              (setq pos
                    (condition-case err
                        (occur-mode-find-occurrence)
                      (error
                       nil)))
              (if pos
                  (pop-to-buffer (marker-buffer pos))
                (pop-to-buffer occur-buffer))
              (when pos
                (goto-char (marker-position pos))
                (if (not color-occur-underline-overlays)
                    (setq color-occur-underline-overlays
                          (make-overlay
                           (line-beginning-position) (1+ (line-end-position)) (marker-buffer pos)))
                  (move-overlay color-occur-underline-overlays
                                (line-beginning-position) (1+ (line-end-position)) (marker-buffer pos)))
                (overlay-put color-occur-underline-overlays 'face 'underline))
              (switch-to-buffer-other-window (get-buffer "*Occur*"))
              )
          (message "no more matches")
          (forward-line 1))
        (setq n (1- n)))
      ))

  (defun color-occur-prev (&optional n)
    "Move to the Nth (default 1) previous match in the *Occur* buffer."
    (interactive "p")
    (if (not n) (setq n 1))
    (let ((r))
      (while (> n 0)

        (setq r (get-text-property (point) 'occur-target))
        (if r (forward-char -1))

        (setq r (previous-single-property-change (point) 'occur-target))
        (if r
            (progn
              (goto-char (- r 1))
              (save-excursion
                (end-of-line)
                (if (re-search-backward "^[ ]*\\([0-9]+\\):" nil t)
                    (setq line (string-to-int
                                (buffer-substring (match-beginning 1)
                                                  (match-end 1))))))
              (setq pos
                    (condition-case err
                        (occur-mode-find-occurrence)
                      (error
                       nil)))
              (if pos
                  (pop-to-buffer (marker-buffer pos))
                (pop-to-buffer occur-buffer))
              (when pos
                (goto-char (marker-position pos))
                (if (not color-occur-underline-overlays)
                    (setq color-occur-underline-overlays
                          (make-overlay
                           (line-beginning-position) (1+ (line-end-position)) (marker-buffer pos)))
                  (move-overlay color-occur-underline-overlays
                                (line-beginning-position) (1+ (line-end-position)) (marker-buffer pos)))
                (overlay-put color-occur-underline-overlays 'face 'underline))
              (switch-to-buffer-other-window (get-buffer "*Occur*"))
              )
          (message "no earlier matches")
          (forward-line -1))
        (setq n (1- n))))))

(defun color-occur-scroll-down ()
  (interactive)
  (scroll-down)
  (forward-line 1)
  (color-occur-prev))

(defun color-occur-scroll-up ()
  (interactive)
  (scroll-up)
  (forward-line 1)
  (color-occur-prev))

(provide 'color-occur)

;;; $Log: color-occur.el,v $
;;; Revision 2.2  2005/05/14 03:05:59  akihisa
;;; for multi-occur
;;;
;;; Revision 2.1  2005/02/18 11:08:15  akihisa
;;; *** empty log message ***
;;;
;;; Revision 2.0  2004/08/10 02:00:29  akihisa
;;; update version
;;;
;;; Revision 1.1.1.1  2004/08/10 01:48:09  akihisa
;;; plisp
;;;
;;; Revision 1.23  2004/06/28 13:40:09  akihisa
;;; *** empty log message ***
;;;
;;; Revision 1.22  2003/10/04 08:33:10  akihisa
;;; *** empty log message ***
;;;
;;; Revision 1.1.1.1  2003/10/04 08:12:05  akihisa
;;; myelisp
;;;
;;; Revision 1.21  2003/06/20 11:22:13  akihisa
;;; *** empty log message ***
;;;
;;; Revision 1.20  2003/06/17 14:45:33  akihisa
;;; C
;;;
;;; Revision 1.19  2003/06/13 12:37:54  akihisa
;;; *** empty log message ***
;;;

;;; color-occur.el ends here

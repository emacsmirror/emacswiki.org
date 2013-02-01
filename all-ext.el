;;; all-ext.el --- Extension of M-x all

;; Filename: all-ext.el
;; Description: Extension of M-x all
;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Maintainer: rubikitch <rubikitch@ruby-lang.org>
;; Copyright (C) 2013, rubikitch, all rights reserved.
;; Time-stamp: <2013-02-01 21:19:47 rubikitch>
;; Created: 2013-01-31 16:05:17
;; Version: 0.1
;; URL: http://www.emacswiki.org/emacs/download/all-ext.el
;; Package-Requires: ((all "1.0"))
;; Keywords: all, search, replace, anything, helm, occur
;; Compatibility: GNU Emacs 24.2.2
;;
;; Features that might be required by this library:
;;
;; `all', `anything', `helm'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary: 
;; 
;; Extend M-x all to be editable M-x occur:
;;   - Show line number before line content (using overlay)
;;   - Can navigate with M-x next-error / M-x previous-error
;;
;; Call M-x all from anything/helm:
;;   1. Call anything/helm command showing lineno and content
;;      such as M-x anything-occur / anything-browse-code /
;;              helm-occur / helm-browse-code etc
;;   2. Press C-c C-a to show anything/helm contents into *All* buffer
;;   3. You can edit *All* buffer!
;;
;;; Installation:
;;
;; Put all-ext.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'all-ext)
;; ;; optional
;; (require 'helm-config) ;; or (require 'anything-config)
;;
;; No need more.

;;; Code:

(require 'all)
(eval-when-compile (require 'cl))

;;;; Line number overlay
(defun all-make-lineno-overlay (lineno)
  (let ((o (make-overlay (point) (point))))
    (overlay-put o 'before-string (format "%7d:" lineno))
    (overlay-put o 'face 'default)
    o))
(defun all-make-lineno-overlays-from-here (to lineno)
  (all-make-lineno-overlay lineno)
  (while (search-forward "\n" (1- to) t)
    (setq lineno (1+ lineno))
    (all-make-lineno-overlay lineno)))

;;; REDEFINED original
(defun all-insert (start end regexp nlines)
  "Redefined original `all-insert' to display line number overlay."
  ;; Insert match.
  (let ((marker (copy-marker start))
        (buffer (current-buffer)))
    (with-current-buffer standard-output
      (let ((from (point)) to)
        (insert-buffer-substring buffer start end)
        (setq to (point))
        (goto-char from)
        (all-make-lineno-overlays-from-here
          to (with-current-buffer buffer (line-number-at-pos start)))
        (overlay-put (make-overlay from to) 'all-marker marker)
        (goto-char from)
        (while (re-search-forward regexp to t)
          (put-text-property (match-beginning 0) (match-end 0)
                             'face 'match))
        (goto-char to)
        (if (> nlines 0)
            (insert "--------\n"))))))

(defun kill-All-buffer-maybe ()
  (when (get-buffer "*All*")
    (kill-buffer "*All*")))
(defadvice all (before delete-All-buffer activate)
  "Kill *All* buffer to delete all line number overlays"
  (kill-All-buffer-maybe))
;; (progn (ad-disable-advice 'all 'before 'delete-all-buffer) (ad-update 'all))

;;;; Call `all' from anything/helm
(declare-function anything-run-after-quit "ext:anything")
(declare-function helm-run-after-quit "ext:helm")
(defvar anything-buffer)
(defvar anything-current-buffer)
(defvar helm-buffer)
(defvar helm-current-buffer)

(eval-after-load "anything-config"
  '(define-key anything-map (kbd "C-c C-a") 'all-from-anything-occur))
(defun all-from-anything-occur ()
  "Call `all' from `anything' content."
  (interactive)
  (anything-run-after-quit
   'all-from-anything-occur-internal "anything-occur"
   anything-buffer anything-current-buffer))

(eval-after-load "helm"
  '(define-key helm-map (kbd "C-c C-a") 'all-from-helm-occur))
(defun all-from-helm-occur ()
  "Call `all' from `helm' content."
  (interactive)
  (helm-run-after-quit
   'all-from-anything-occur-internal "helm-occur"
   helm-buffer helm-current-buffer))

(defun all-from-anything-occur-internal (from anybuf srcbuf)
  (kill-All-buffer-maybe)
  (let ((all-initialization-p t)
        (buffer srcbuf))
    (with-output-to-temp-buffer "*All*"
      (with-current-buffer standard-output
	(all-mode)
	(setq all-buffer buffer)
        (insert "From " from "\n")
	(insert "--------\n"))
      (if (eq buffer standard-output)
	  (goto-char (point-max)))
      (with-current-buffer anybuf
        (save-excursion
          (goto-char (point-min))
          (forward-line 1)              ;ignore title line
          ;; Find next match, but give up if prev match was at end of buffer.
          (loop with regexp = (format "^\\(%s:\\| *\\)\\([0-9]+\\)[ :]\\(.+\\)$"
                                      (buffer-name srcbuf))
                while (re-search-forward regexp nil t)
                for lineno = (string-to-number (match-string 2))
                for content = (match-string 3)
                do
                (with-current-buffer srcbuf
                  (save-excursion
                    (goto-char (point-min))
                    (goto-char (point-at-bol lineno))
                    (all-from-anything-occur-insert
                     (point) (progn (forward-line 1) (point)) lineno content)))))))))
(defun all-from-anything-occur-insert (start end lineno content)
  (let ((marker (copy-marker start)))
    (with-current-buffer standard-output
      (let ((from (point)) to)
        (insert content "\n")
        (setq to (point))
        (goto-char from)
        (all-make-lineno-overlays-from-here to lineno)
        (goto-char to)
        (overlay-put (make-overlay from to) 'all-marker marker)))))

;;; `next-error' and `previous-error' from `all' (shoddy implementation)
(defun all-next-error (&optional argp reset)
  (let ((w (get-buffer-window "*All*")))
    (if (not w)
        (error "Cannot find *All* buffer window.")
      (with-selected-window w
        (when (= (point-at-bol) (point-min))
          (forward-line 1))
        (forward-line argp)
        (all-mode-goto)))))
(defadvice all-mode (after next-error activate)
  (setq next-error-function 'all-next-error))
;; (progn (ad-disable-advice 'all-mode 'after 'next-error) (ad-update 'all-mode))

(provide 'all-ext)
;;; all-ext.el ends here

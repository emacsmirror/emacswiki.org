;;; gst-inspect.el --- wrapper for gst-inspect

;; Copyright (C) 2008 Marco (Bj) Bardelli

;; Author: Marco (Bj) Bardelli <fanaj@jbook.sana.figa>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; To use with Gtreamer suite.
;; To navigate the output of `gst-inpect'. 
;; TODO build bin pipeline and store in xml.

;;; Code:

(unless (fboundp 'chomp)
  (defun chomp (s)
  "chomp `perl-like' function. Take a string or symbol
and return a string without final or initial whitespace class chars."
    (while (string-match "^\\s-+\\|\\s-+$" s)
      (setq s (replace-match "" t nil s))) s)))

;; Variables
(defgroup gst-inspect nil
  "gst-inspect group."
  :group 'my
  :prefix "gst-inspect-")

(defcustom gst-inspect-name-component-in-buffer-name nil
  "*"
  :group 'gst-inspect
  :type 'boolean)

(defvar gst-inspect-buffer-name "*Gst-Inspect*"
  "")

(defvar gst-inspect-program "gst-inspect"
  "")

(defvar gst-inspect-header-regexp "\\`.*$"
  "")

(defface gst-inspect-header
  '((t (:inherit diff-index)))
  "Face used for the first line."
  :group 'gst-inspect)
;; this can to be simply `diff-index'
(defvar gst-inspect-header-face 'gst-inspect-header
  "Face name used for the first line.")
  
(defvar gst-inspect-font-lock-kewords
  (list
    ;; header line
    '("\\`.*$" . gst-inspect-header-face)
;    '(gst-inspect-header-regexp . gst-inspect-header-face)
;    '(gst-inspect-header-regexp . font-lock-warning-face)
;    (list gst-inspect-header-regexp '(0 gst-inspect-header-face))
;    (list gst-inspect-header-regexp '(0 font-lock-warning-face))
    ;; body of output
    (cons "^\\(.*?\\): +?\\(.*?\\): +?\\(.*\\)$"
	  '((1 font-lock-constant-face)
	    (2 font-lock-variable-name-face)
	    (3 font-lock-type-face))))
  "")

(defvar gst-inspect-component-font-lock-kewords
  (list
    ;; output
;;     (cons "^[^ ].*?:$" '((t
;; 			   ( :inherit default
;; 			     :foreground "red"))))
    (cons "^  ?\\(.*?\\):\\( \\|\t\\|\n\\)\\(.*\\)$"
	  '((1 font-lock-type-face)
	    (3 font-lock-variable-name-face))))
  "")

(defvar gst-inspect-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "RET") 'gst-inspect-component-at-line)
    (define-key km "g" 'gst-inspect-update)
    (define-key km "\t" 'next-line)
    (define-key km "q" 'kill-buffer-and-window)
    km)
  "keymap for gst-inspect-mode")

(defun gst-inspect-mode ()
  ""
  (interactive)
  (let ((gst-inspect-buffer (get-buffer-create gst-inspect-buffer-name)))
    (set-buffer gst-inspect-buffer)
    (when (zerop (buffer-size))
      (kill-all-local-variables)
      (use-local-map gst-inspect-mode-map)
      (abbrev-mode 0)
      (auto-fill-mode 0)
      (setq buffer-read-only t
	    truncate-lines t
	    major-mode 'gst-inspect-mode
	    mode-name "Gst-Inspect")
      (set (make-local-variable 'font-lock-defaults)
	   '(gst-inspect-font-lock-kewords t nil nil beginning-of-line)))
    (gst-inspect-update)
    (pop-to-buffer gst-inspect-buffer)
    (message (substitute-command-keys
	      "type \\[quit-window] to quit"))))

(fset 'gst-inspect 'gst-inspect-mode)

(defun gst-inspect-update ()
  ""
  (interactive)
  ;; i'm in the right buffer
  (message "Inspecting Gstreamer Plugins ...")
  (if buffer-read-only
      (setq buffer-read-only nil))
  (erase-buffer)
  (call-process gst-inspect-program nil gst-inspect-buffer-name
		nil "--print")
  (goto-char (point-min))
  (put-text-property (point-min) (1+ (point-at-eol)) 'intangible t)
  (setq buffer-read-only t)
  (message "Inspecting Gstreamer Plugins ...done."))

(defun gst-inspect-get-component-buffer-name (component)
  ""
  (unless (stringp component) (error nil))
  (if gst-inspect-name-component-in-buffer-name
      (concat
       (substring gst-inspect-buffer-name 
		  0 (1- (length gst-inspect-buffer-name)))
       " : " component " *")
    "*Gst-Inspect Component*"))

(defun gst-inspect-component-at-line ()
  ""
  (interactive)
  (let ((bol (point-at-bol))(eol (point-at-eol)) buf-name comp)
    (setq comp
	  (chomp (nth 1 (split-string (buffer-substring bol eol) ":" t))))
    (setq buf-name (gst-inspect-get-component-buffer-name comp))
    (set-buffer (get-buffer-create buf-name))
    (set (make-local-variable 'font-lock-defaults)
	 '(gst-inspect-component-font-lock-kewords t nil nil beginning-of-line))
    (when (not (zerop (buffer-size)))
      (setq buffer-read-only nil)
      (erase-buffer))
    (call-process gst-inspect-program nil t nil comp)
    (beginning-of-buffer)
    (local-set-key "q" 'kill-buffer-and-window)
    (setq buffer-read-only t
;	  font-lock-mode t
	  truncate-lines t
	  major-mode 'gst-inspect-mode
	  mode-name "Gst-Inspect")
    (font-lock-mode t)
    (pop-to-buffer buf-name)))


;;;; Pipeline builder
(defun gst-pb-get-plugs-list-internal ()
  ""
  (let ((ret
	 (split-string (shell-command-to-string
			"gst-inspect --print | cut -d':' -f2") "\n" t)))
    (setq ret
	  (mapcar
	   (lambda (x) (chomp x))
	   (cdr ret)))
    (remove (last ret) ret)
  ))

(defvar gst-pb-plugins-list (gst-pb-get-plugs-list-internal)
  "")


(provide 'gst-inspect)
;;; gst-inspect.el ends here

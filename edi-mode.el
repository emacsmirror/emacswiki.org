;;; edi-mode.el --- edit raw EDI files

;; Copyright (C) 2004,2005,2007 Jeremy Cowgar

;; Author: Jeremy Cowgar <jeremy@cowgar.com>
;; Maintainer: Jeremy Cowgar <jeremy@cowgar.com>
;; Version: 1.0.3
;; Keywords: EDI
;; URL: http://www.emacswiki.org/elisp/edi-mode.el

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Use `edi-mode' to edit raw edi files. Here is how to do that:
;;
;; (add-to-list 'auto-mode-alist '("\\.edi" . edi-mode))
;;
;; Or, if you are > Emacs version 21, you can recognize them
;; like:
;;
;; (add-to-list 'magic-mode-alist '("^ISA" . edi-mode))
;;
;; Key Bindings:
;;   C-c C-r -- convert a single line EDI file to a multi-line
;;              edi file (Readable)
;;   C-c C-e -- covert a multi-line edi file to a single line
;;              edi file (Non-readable)
;;   C-c C-c -- query for, and count the give edi segment
;;

;;; History
;;
;; 1.0.3    2007-03-09
;;  * Added nothing more than a reference to setting up via 
;;    magic-mode-alist. Many EDI files do not have a common
;;    extension.
;;
;; 1.0.2    2005-12-20
;;  * Highlights Segment Id's at begining of line as well as after a
;;    segment terminator (Christian Plate)
;;
;;  * Counts segments correctly while in EDI format (one line only).
;;
;; 1.0.1    2005-02-08
;;  * Updates for (S)XEmacs (Steve Youngs <steve@sxemacs.org>)
;;
;; 1.0.0    2004-12-10
;;  * Initial Creation

;;; Code:

(defun edi-edi-to-readable ()
  "Make a 1 line EDI file into multiple lines by replacing the segment terminator ~ with a ~\n"
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(while (search-forward "~" nil t) (replace-match "~\n")))
  (set-buffer-modified-p nil))

(defun edi-readable-to-edi ()
  "Converts an EDI file that is mixed with ~\n terminators to one that is simply ~"
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(while (search-forward "~\n" nil t) (replace-match "~")))
  (set-buffer-modified-p nil))

(defun edi-count-segments (segment)
  "Simply counts the number of segments found"
  (interactive "sSegment? ")
  (save-excursion
	(goto-char (point-min))
	(let ((a 0))
      (while (re-search-forward (format "\\(^\\|~\\)%s" segment) nil t)
        (setq a (+ a 1)))
      (message (format "%i %s segments found" a segment))
      )))

(defvar edi-font-lock-keywords
  '(("~\\([A-Z0-9]+\\)" .
     (1 font-lock-function-name-face))
    ("\\(^\\|~\\)[A-Z0-9]+" . font-lock-function-name-face)
    ("[*]" . font-lock-comment-face)
    ("[~]" . font-lock-keyword-face)))

(define-derived-mode edi-mode text-mode "EDI"
  "Simple mode to make it easier to edit EDI files.

\\{edi-mode-map}"
  (define-key edi-mode-map "\C-c\C-r" 'edi-edi-to-readable)
  (define-key edi-mode-map "\C-c\C-e" 'edi-readable-to-edi)
  (define-key edi-mode-map "\C-c\C-c" 'edi-count-segments)
  (set (make-local-variable 'font-lock-defaults)
       '(edi-font-lock-keywords t))
  (font-lock-mode 1)
  (when (featurep 'goto-addr)
    (goto-address))
  (set (make-local-variable 'skeleton-transformation) 'identity)
  (setq indent-tabs-mode nil))

(provide 'edi-mode)

;;; edi-mode.el ends here

;;; rmail-spamprobe.el --- interact with spamprobe from within RMAIL
;;
;; Copyright (C) 2005, 2006 Henrik Enberg
;;
;; Author: Henrik Enberg <enberg@printf.se>
;; Keywords: mail

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
;; USA

;;; Commentary:

;; Some code for interacting with spamprobe from within RMAIL. This code
;; assumes that you have a functional mailsystem with procmail calling
;; spamprobe and inserting an X-SpamProbe header in filtered messages
;; before the messages reaches RMAIL.

;; Making it so is left as an exercise to the reader.  The spamprobe
;; documentation has pointers on how to do this.

;; The single entry point is `rmail-spamprobe-retrain-dwim', which will
;; examine the message for its current score, have spamprobe reverse it
;; and then send off the message to procmail again for a new round of
;; processing.

;; For ease of use, binding this function to a key in `rmail-mode-map'
;; and `rmail-summary-mode-map' would make sense.  I also suggest adding
;; "x-spamprobe" to the list of displayed headers in
;; `rmail-displayed-headers'.

;;; Code:

(require 'rmail)

(defun rmail-spamprobe-original-message (&optional n)
  "Return an approximation of a pre-babylized RMAIL message.
With  optional arg N, use the nth message in the current RMAIL file,
else use the current message."
  (with-current-buffer rmail-buffer
    (or n (setq n rmail-current-message))
    (save-restriction
      (narrow-to-region (rmail-msgbeg n) (rmail-msgend n))
      (let ((msg (buffer-substring-no-properties (point-min) (point-max))))
	(with-temp-buffer
	  (insert msg)
	  (goto-char (point-min))
	  ;; maybe kill babyl lines at the beginning
	  (when (looking-at "\C-_\C-l")
	    (forward-line 2)
	    (while (looking-at "Summary-line:\\|X-Coding-System:")
	      (forward-line 1))
	    (when (looking-at "^\\(Mail-from:[ \t]+\\)")
	      (goto-char (match-end 1)))
	    (delete-region (point-min) (point)))
	  ;; delete the visible headers block
	  (when (re-search-forward "^\\*\\*\\* EOOH \\*\\*\\*\n" nil t)
	    (forward-line -1)
	    (delete-region (point) (search-forward "\n\n")))
	  ;; we should now have something similar to the original,
	  ;; pre-babylized message in the current buffer.
	  (buffer-string))))))

(defun rmail-spamprobe-narrow-to-header ()
  "Narrow to the header of a mail message.
Point is placed at the beginning of the header."
  (narrow-to-region
   (goto-char (point-min))
   (if (search-forward "\n\n" nil t)
       (1- (point))
     (point-max)))
  (goto-char (point-min)))

(defun rmail-spamprobe-get-header-field (field-name)
  "Return the text value for FIELD-NAME, or nil if no such header exists."
  (let ((name (concat "^" (regexp-quote field-name) "[ \t]*:[ \t]*"))
	(inhibit-point-motion-hooks t)
	(case-fold-search t)
	value start)
    (save-excursion
      (save-restriction
	(rmail-spamprobe-narrow-to-header)
	(when (re-search-forward name nil t)
	  (setq start (point))
	  (end-of-line)
	  (push (buffer-substring start (point)) value)
	  ;; Header may be folded, pick up continuation lines.
	  (while (progn (forward-line 1) (looking-at "[ \t]+"))
	    (setq start (match-end 0))
	    (end-of-line)
	    (push (buffer-substring start (point)) value))))
      (when value
	(mapconcat 'identity (nreverse value) " ")))))

(defun rmail-spamprobe-delete-header-field (field-name)
  "Delete the header field FIELD-NAME."
  (let ((name (concat "^" (regexp-quote field-name) "[ \t]*:[ \t]*"))
	(inhibit-point-motion-hooks t)
	(case-fold-search t)
	start)
    (save-excursion
      (save-restriction
	(rmail-spamprobe-narrow-to-header)
	(when (re-search-forward name nil t)
	  (setq start (match-beginning 0))
	  (end-of-line)
	  (while (progn (forward-line 1) (looking-at "[ \t]+"))
	    (end-of-line))
	  (delete-region start (point)))))))

;;;###autoload
(defun rmail-spamprobe-retrain-dwim ()
  (interactive)
  (let ((msg (rmail-spamprobe-original-message))
	(action nil))
    (with-temp-buffer
      (insert msg)
      (goto-char (point-min))
      (let ((field (rmail-spamprobe-get-header-field "x-spamprobe")))
	(if (not field)
	    (error "spamprobe doesn't know about this message")
	  (setq action (cond ((string-match "SPAM" field)
			      "good")
			     ((string-match "GOOD" field)
			      "spam")))))
      (if (not (stringp action))
	  (error "unable to determine what to do with message")
	;; delete the original spamprobe header
	(rmail-spamprobe-delete-header-field "x-spamprobe")
	;; first, let spamprobe reclassify the message
	(call-process-region
	 (point-min) (point-max)
	 (executable-find "spamprobe")
	 nil 0 nil action)
	;; then pipe it through procmail again
	(call-process-region
	 (point-min) (point-max)
	 (executable-find "procmail")
	 nil 0 nil)))
    (rmail-delete-forward)))

(provide 'rmail-spamprobe)

;;; rmail-spamprobe.el ends here

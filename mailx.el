;;; mailx.el --- A front-end to mailx

;; Copyright (C) 2002 by Shawn Betts

;; Author: Shawn Betts <sabetts@sfu.ca>
;; Keywords: mail

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Its really annoying when subpar systems don't have GNUS
;; installed. I can't stand using another mail program because they
;; all seem to clobber my mailbox in some way. Then when I get home I
;; boot GNUS and have to undo all the frobs the mail program made and
;; get GNUS to sort out the mess. mailx is a quick hack to allow you
;; to read your mail without modifying your mailbox.
;;
;; I use it when I'm at school and all I have is some crusty elm and
;; pine. Its quick, dirty, and...uhm...works.

;;; Code:

(require 'font-lock)
(require 'mail-utils)

(defvar mailx-program "mailx"
  "Name of the mail program")

(defvar mailx-mode-map nil)

(defvar mailx-message-mode-map nil)

(defvar mailx-mode-hook nil
  "hook called after entering mailx-mode")

(defvar mailx-font-lock-keywords
  (let* ((cite-chars "[>|}]")
	 (cite-prefix "A-Za-z")
	 (cite-suffix (concat cite-prefix "0-9_.@-`'\"")))
    (list '("^\\(From\\|Sender\\):" . font-lock-function-name-face)
	  '("^Reply-To:.*$" . font-lock-function-name-face)
	  '("^Subject:" . font-lock-comment-face)
	  '("^\\(To\\|Apparently-To\\|Cc\\|Newsgroups\\):"
	    . font-lock-keyword-face)
	  ;; Use MATCH-ANCHORED to effectively anchor the regexp left side.
	  `(,cite-chars
	    (,(concat "\\=[ \t]*"
		      "\\(\\([" cite-prefix "]+[" cite-suffix "]*\\)?"
		      "\\(" cite-chars "[ \t]*\\)\\)+"
		      "\\(.*\\)")
	     (beginning-of-line) (end-of-line)
	     (2 font-lock-constant-face nil t)
	     (4 font-lock-comment-face nil t)))
	  '("^\\(X-[A-Za-z0-9-]+\\|In-reply-to\\|Date\\):.*$"
	    . font-lock-string-face)))
  "Additional expressions to highlight in mailx mode.")

(if mailx-mode-map
    ()
  (setq mailx-mode-map (make-keymap))
  (define-key mailx-mode-map "h" 'mailx-show-message-list)
  (define-key mailx-mode-map "n" 'mailx-next-message)
  (define-key mailx-mode-map "m" 'compose-mail)
  (define-key mailx-mode-map "r" 'mailx-reply-to-message)
  (define-key mailx-mode-map [(control m)] 'mailx-show-message)
  (define-key mailx-mode-map "q" 'mailx-quit))

(if mailx-message-mode-map
    ()
  (setq mailx-message-mode-map (make-keymap))
  (define-key mailx-message-mode-map "h" 'mailx-message-goto-list)
  (define-key mailx-message-mode-map " " 'scroll-up)
  (define-key mailx-message-mode-map [backspace] 'scroll-down)
  (define-key mailx-message-mode-map "r" 'mailx-reply-to-message)
  (define-key mailx-message-mode-map "q" 'mailx-quit))

(defun mailx ()
  (interactive)
  (switch-to-buffer "*mailx*")
  (if (eq major-mode 'mailx-mode)
      ()
    ;; Its not an mailx buffer, so just wipe it out (ouch!)
    (let ((buf (get-buffer "*mailx*")))
      (mailx-mode)
      (erase-buffer)
      (mailx-start-program))))

(defun mailx-mode ()
  "A Major mode for reading email with mailx.
\\{mailx-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map mailx-mode-map)
  (setq major-mode 'mailx-mode)
  (setq mode-name "mailx")
  (setq buffer-read-only t)
  (run-hooks 'mailx-mode-hook))

(defun mailx-message-mode ()
  "A Major mode for reading mailx messages"
  (interactive)
  (kill-all-local-variables)
  (use-local-map mailx-message-mode-map)
  (setq major-mode 'mailx-message-mode)
  (setq mode-name "mailx message")
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults 
	'(mailx-font-lock-keywords
	  t nil nil nil
	  (font-lock-maximum-size . nil)
	  (font-lock-inhibit-thing-lock . (lazy-lock-mode fast-lock-mode))))
  (setq buffer-read-only t))

(defun mailx-read-message (proc string)
  (with-current-buffer (get-buffer "*mailx-message*")
    (let ((inhibit-read-only t))
      (insert string)))
  (mailx-process-filter proc string))

(defun mailx-read-list (proc string)
  (with-current-buffer (get-buffer "*mailx*")
    (let ((inhibit-read-only t))
      (insert string)))
  (mailx-process-filter proc string))

(defun mailx-process-filter (proc string)
  "Default process filter. Just ignore input."
  )
  
(defun mailx-start-program ()
  "Start the mailx program if needed"
  (if (processp (get-process "mailx"))
      ()
    ;; The program isn't running, so start it up
    (let ((proc (start-process "mailx" "*mailx-process*" mailx-program "-N")))
      (set-process-filter proc 'mailx-read-list)
      ;; Allow at most 100000 messages in the message list buffer
      (process-send-string proc "set screen=100000\n")
      ;; Make sure the messages remain in the inbox
      (process-send-string proc "hold\n")
      (process-send-string proc "header\n"))))

;;; User functions
(defun mailx-next-message ()
  "Display the next message"
  (interactive)
  (forward-line)
  (mailx-show-message))

(defun mailx-show-message ()
  "Display the contents of the current message."
  (interactive)
  (display-buffer (get-buffer-create "*mailx-message*"))
  (with-current-buffer (get-buffer "*mailx-message*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (mailx-message-mode)))
  (save-excursion
    (set-buffer (get-buffer "*mailx*"))
    (beginning-of-line)
    (if (null (re-search-forward "^[>]?\\s-*[A-Z]\\s-*\\([0-9]+\\)" nil t))
	(message "No message on line")
      (set-process-filter (get-process "mailx") 'mailx-read-message)
      (process-send-string (get-process "mailx") (concat (match-string 1) "\n")))))


(defun mailx-show-message-list ()
  (interactive)
  (pop-to-buffer (get-buffer "*mailx*"))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (set-process-filter (get-process "mailx") 'mailx-read-list)
  (process-send-string (get-process "mailx") "header\n"))

(defun mailx-reply-to-message (just-sender)
  "Reply to the message in the *mailx-message* buffer."
  (interactive "P")
  (let (from reply-to cc subject date to message-id references
	     resent-to resent-cc resent-reply-to)
    (with-current-buffer (get-buffer "*mailx-message*")
      (save-restriction
	;; Narrow to just the headers
	(widen)
	(goto-char (point-min))
	(search-forward "\n\n")
	(narrow-to-region (point-min) (point))
	;; This is basically a verbatim copy of the fat setq in rmail-reply
	(setq from (mail-fetch-field "from")
	      reply-to (or (mail-fetch-field "reply-to" nil t) from)
	      cc (and (not just-sender)
		      (mail-fetch-field "cc" nil t))
	      subject (mail-fetch-field "subject")
	      date (mail-fetch-field "date")
	      to (or (mail-fetch-field "to" nil t) "")
	      message-id (mail-fetch-field "message-id")
	      references (mail-fetch-field "references" nil nil t)
	      resent-reply-to (mail-fetch-field "resent-reply-to" nil t)
	      resent-cc (and (not just-sender)
			     (mail-fetch-field "resent-cc" nil t))
	      resent-to (or (mail-fetch-field "resent-to" nil t) ""))))
    ;; Merge the resent-to and resent-cc into the to and cc.
    (if (and resent-to (not (equal resent-to "")))
	(if (not (equal to ""))
	    (setq to (concat to ", " resent-to))
	  (setq to resent-to)))
    (if (and resent-cc (not (equal resent-cc "")))
	(if (not (equal cc ""))
	    (setq cc (concat cc ", " resent-cc))
	  (setq cc resent-cc)))
    ;; Add `Re: ' to subject if not there already.
    (and (stringp subject)
	 (setq subject
	       (concat rmail-reply-prefix
		       (if (let ((case-fold-search t))
			     (string-match rmail-reply-regexp subject))
			   (substring subject (match-end 0))
			 subject))))
    ;; setup the mail buffer
    (mailx-start-mail nil 
		      reply-to
		      subject
		      nil 
		      (if just-sender
			  nil
			(let* ((cc-list (rmail-dont-reply-to
					 (mail-strip-quoted-names
					  (if (null cc) to (concat to ", " cc))))))
			  (if (string= cc-list "") nil cc-list)))
		      (get-buffer "*mailx-message*"))))

(defun mailx-start-mail (&optional noerase to subject in-reply-to cc replybuffer actions)
  "Setup the mail buffer. Use mail-other-window if we're in the
*mailx* buffer and mail otherwise."
  (if (equal (current-buffer) (get-buffer "*mailx*"))
      (mail-other-window noerase to subject in-reply-to cc replybuffer actions)
    (mail noerase to subject in-reply-to cc replybuffer actions)))
  
(defun mailx-message-goto-list ()
  (interactive)
  (pop-to-buffer (get-buffer "*mailx*")))

(defun mailx-quit ()
  (interactive)
  ;; Don't give the process a chance to from the mailbox by killing
  ;; it before it has a chance to react.
  (kill-process (get-process "mailx"))
  (kill-buffer (get-buffer "*mailx*"))
  (kill-buffer (get-buffer "*mailx-message*")))

(provide 'mailx)
;;; mailx.el ends here

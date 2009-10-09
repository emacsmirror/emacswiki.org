;;; sms.el --- major mode for composing and sending SMS text messages

;; Filename: sms.el
;; Description: major mode for composing and sending SMS text messages
;; Author: Joe Bloggs (vapniks@yahoo.com)
;; Maintainer: Joe Bloggs
;; Copyright (C) 2009, Joe Bloggs
;; Created: Thu Oct  8 23:16:05 2009
;; Version: 0.1
;; Last-Updated: Thu Oct  8 23:16:15 2009
;;           By: Joe Bloggs
;; URL:
;; Keywords: bbdb, sms, texting
;; Compatibility: GNU Emacs 23.1.50.1, BBDB 2.36
;;
;; Features that are required by this library: not necessary, but useful if bbdb & bbdb-sms are loaded
;;

;;; This file is NOT part of GNU Emacs

;;; LICENSE:
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
;; This library contains a major mode for composing and sending text messages.
;; It contains the function sms-send-buffer-as-text-message which is bound to "C-c C-c"
;; by default. This function will in turn call the sms-send-text-message function to send
;; the contents of the buffer to the phone numbers listed at the top of the buffer after "To:".
;; Currently sms-send-text-message is bound to sms-send-text-message-android which can be used
;; on Emacs running in Debian on Android phones with Android Scripting Environment running, as detailed
;; here: http://www.emacswiki.org/emacs/EmacsOnAndroid
;; You can create your own function for sending text messages on your device and bind sms-send-text-message to it.
;;
;; If you have bbdb-sms installed then you can use tab completion of names in the "To:" header of the *SMS* buffer,
;; it will complete the name and phone number according to entries in your bbdb.
;;
;; The following keybindings are defined by default:
;;
;;       "C-c C-c" ; send the buffer as a text message to the phone numbers after "To:" at the top
;;       "C-c C-k" ; kill the buffer
;;       "<tab>"   ; if bbdb-sms.el is loaded, complete the name at point and insert the associated phone number(s)

;;; INSTALLATION:
;;
;; Put sms.el in your load-path.
;; The load-path is usually ~/.emacs.d/
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'sms)
;;

;;; Change log:
;;
;; 2009/10/08
;;      First released.

;;; Acknowledgements:

;;; TODO: add validity checking? such as [0-9]+ in sms-extract-plain-numbers

;;; Require:

;;; CODE:

(defgroup sms nil
  "Major mode for composing and sending SMS text messages."
  :prefix "sms-"
  :group 'applications)

(defcustom sms-mode-hook nil
  "Hook to be run when `sms-mode' is started."
  :type '(repeat symbol)
  :group 'sms)

;; Maximum length of text message. May change this in the future and accomodate multi-text messages
(defcustom sms-max-length 160
  "Maximum length of text message."
  :type 'integer
  :group 'sms)

(put 'sms-mode 'font-lock-defaults '(message-font-lock-keywords t))

(defcustom sms-phone-number-separators " \t-"
  "The phone number separators that may appear in a phone number.
A phone number such as 12345678, will be clear if written as:
123 456 78, or 123-456-78."
  :type 'regexp
  :group 'sms)

(defcustom sms-send-text-message 'sms-send-text-message-android
  "The function used to send the contents of the buffer as a  text message.
This should be a function that takes two arguments.
The first argument should be a string containing a phone number,
and the second argument a string containing the text message to be sent."
  :type 'symbol
  :group 'sms)

(defun sms-create-buffer (recipients)
  "Create a new buffer in sms-mode for writing text messages.
The list of recipients will be obtained from the recipients argument,
and displayed at the top of the buffer.
The newly created buffer will be named `*SMS*', or if there is already a buffer with that name
then a suffix of the form `<N>' will be added.
The newly created buffer will be returned by this function."
  (let ((smsbuffer (generate-new-buffer "*SMS*"))
	toheader
	(breakline "--text follows this line--\n"))
    (with-current-buffer smsbuffer
      (setq toheader (concat "To: " recipients "\n"))
      (goto-char (point-min))
      (insert (concat toheader breakline))
      (sms-mode))
    smsbuffer))

(defun sms-legal-phone-numberp (num)
  "Return t if NUM is a legal phone number.
A legal phone number have an optional leading +, followed by some numbers"
  (string-match "^+?[0-9]+$" num))

(defun sms-get-TO-header-list (&optional buffer)
  "Get the content in *SMS* buffer BUFFER's To: header list as a string."
  (or buffer (setq buffer (current-buffer)))
  (when (sms-bufferp buffer)
    (with-current-buffer buffer
      (mail-fetch-field "to"))))

(defun sms-extract-recipients-list ()
  "Extract the recipients from current *SMS* buffer's header.
Return a list of strings.
This function must be called from within a *SMS* buffer"
  (unless (sms-bufferp (current-buffer))
    (error "sms-extract-recipients-list not called in a *SMS* buffer"))
  
  (let* ((recipients (split-string (sms-get-TO-header-list) ","))
	 (phone-nums nil))
    (dolist (recipient recipients)
      (let ((recipient-num-list nil))
	(if (string-match "<" recipient)
	  ;; an element inserted by bbdb
	    (setq recipient-num-list (sms-extract-angle-bracketed-numbers recipient))
	  ;; an element added by user
	  (setq recipient-num-list (sms-extract-plain-numbers recipient)))
	(dolist (num recipient-num-list)
	  (unless (string= num "")
	    (push num phone-nums)))))
    (nreverse phone-nums)))

(defun sms-extract-angle-bracketed-numbers (recipient)
"Extract phone numbers from recipient string.
Where each phone number is surrounded by angle brackets <>."
  (let ((phone-nums nil)
	(regexp "<[0-9]+>")
	(begin-idx nil)
	(end-idx 0))
    (while (setq beg-idx (string-match regexp recipient end-idx))
      (setq end-idx (match-end 0))
      (push (substring recipient (1+ beg-idx) (1- end-idx)) phone-nums))
    (nreverse phone-nums)))

(defun sms-extract-plain-numbers (recipient)
"Extract user added phone number from RECIPIENT string."
  ;; just remove the number separators
  ;; TODO: add validity checking? such as [0-9]+
  ;; so 123dd123 is not a legal num
  (let ((num (sms-string-remove-number-separators recipient)))
    (list num)))

(defun sms-string-remove-number-separators (str)
  "Return a copy of STR with all separators in `sms-phone-number-separators' removed."
  (remove-if #'(lambda (c) (string-match (char-to-string c) sms-phone-number-separators)) str))

(defun sms-send-buffer-as-text-message (recipients)
  "Send contents of current buffer as SMS text message to RECIPIENTS."
  (interactive (list (sms-extract-recipients-list)))
  (let* ((buffercontents (buffer-string))
	 (bufferlength (length buffercontents))
	 (numrecipients (length recipients)))
  (if (> bufferlength sms-max-length)
      ;; if the message is too long, issue a warning
      (message (concat "Message is " (int-to-string (- bufferlength sms-max-length)) " characters too long!"))
    ;; otherwise send the message
    (dolist (num recipients)
      (funcall sms-send-text-message num buffercontents))
    (kill-buffer nil))))

(defun sms-number-stringp (elem)
  "Return t if ELEM is a string which only contains numbers, otherwise nil."
  (unless (stringp elem)
    (error "%s is not string" elem))
  (dotimes (idx (length elem) t)
    (let ((c (elt elem idx)))
      (unless (and (<= c ?9)
		   (>= c ?0))
	(error "Phone number %s contains non-number character" elem)))))

(defun sms-bufferp (&optional buffer)
  "Return t if BUFFER is a sms-buffer, otherwise nil.
If BUFFER is not provided, current buffer will be used"
  (interactive)
  (or buffer (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (eq major-mode 'sms-mode)))

;; Function for sending SMS text message to single phone number on Android phone.
;; This requires that an Android Scripting Environment shell is running on the phone,
;; and that the sendText.py script is located in /root/scripts/
(defun sms-send-text-message-android (number text)
  "Send TEXT to NUMBER as SMS text message on Android phone."
  (interactive (list (read-string "Phone number: ") (read-string "Message: ")))
  (shell-command
   (concat
    "export AP_PORT=$(netstat -napt|sed -n 's/^tcp.*127.0.0.1:\\([0-9]*\\).*LISTEN.*ase$/\\1/gp');/root/scripts/sendtext.py "
    (remove-if (lambda (c) (or (char-equal c ?\t) (char-equal c ?\s))) number)
    " " text)))

;; keybindings for sms-mode
(defvar sms-mode-map
  (let ((sms-mode-map (make-keymap)))
    (define-key sms-mode-map (kbd "C-c C-c") 'sms-send-buffer-as-text-message)
    (define-key sms-mode-map (kbd "C-c C-k") (lambda nil (interactive) (kill-buffer (current-buffer))))
    (define-key sms-mode-map (kbd "<tab>")   (lambda nil (interactive) (if (featurep 'bbdb-sms) (bbdb-sms-complete-name))))
    sms-mode-map)
  "Keymap for SMS major mode.")

(add-to-list 'auto-mode-alist '("\\.sms\\'" . sms-mode))


;; main function for starting sms-mode
(defun sms-mode ()
  "Major mode for editing SMS text messages.

\\{sms-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (set (make-local-variable 'font-lock-defaults)
       '(message-font-lock-keywords t))
  (use-local-map sms-mode-map)
  (setq major-mode 'sms-mode)
  (setq mode-name "SMS")
  (run-hooks 'sms-mode-hook))

(provide 'sms)

;;; end
;;; sms.el ends here

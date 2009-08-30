;;; email.el
;; File      : email.el  - extensions to mail-mode.
;; Copyright (c) 1994 G Dinesh Dutt
;; Version   : 1.1
;; Author    : G. Dinesh Dutt (brat@htilbom.ernet.in)
;; Maintainer: G. Dinesh Dutt (brat@htilbom.ernet.in)
;; Keywords  : mail-mode extensions
;;
;; This file runs under GNU Emacs, and has the same copyright terms as
;; GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
 
;; Commentary :
;;  This package permits easy archiving of outgoing mails, sending cc's to
;; a specified set of poeple automatically and add a reference id to the
;; subject of the mail, all based on the mail headers only.  A set of three
;; associative-lists is used to determine the behaviour.  Its a complement of
;; vm-auto-folder-alist (used to archive incoming messages).
 
;; Installation :
;; Setup the following in your .emacs :
;;  (require 'email)
;;  (setq send-mail-function 'email-send)
;;
;; Setup any other variable that you need like email-archive-file-name.  You
;; must not use mail-auto-archive-filename as it adds an FCC field automatically;; on invoking the mail-mode and this prevents me from adding an FCC field.  Set;; the value of that variable to email-auto-archive-filename instead.  Also, if
;; call some other function before calling sendmail-send-it, set the variable
;; email-send-mail-function.  The key "\C-c\C-e" in mail-mode-map is used to
;; provide the feature of expanding all the headers within the mail buffer
;; itself.
;;
;; Configure your alists. For eg. :
;; (setq email-fcc-alist
;;    '(("Subject"
;;   ("Test mail"."/dev/null"))
;;  ("To"
;;   ("^navali"."/~/Mail/navali/outbox")
;;   ("staff"."~/Mail/staff/outbox")
;;   ("^bitftp"."~/Mail/ftp/requests")
;;   ("^ftpmail"."~/Mail/ftp/requests")
;;   ("smurty"."~/Mail/smurty/outbox")
;;   ("\\.ncst\\."."/~/Mail/sysadmin/ToNcst"))
;;  ("Resent-To"
;;   ("plg"."/dev/null"))
;;  ))
;;
;; (setq email-cc-alist
;;    '(("To"
;;  ("psi.com\\b"."alan, srini")
;;  ("^phatak@"."srini"))))
;;
;; (setq email-id-alist   ;; The second element of the alist
;;    '(("To"           ;; must be non-nil always. Setting this
;;  ("^acrouch".t)                  ;; to nil results in wrong behaviour.
;;  ("psi.com\\b".t)
;;  ("^srini".t)
;;  ("^geta".t)
;;  ("^phatak@".t))))
;;
;; See the documentation of variables for further customisation.
;;
;; BUGS(Features i.e. :-))
;;
;; A default archive file (specfied via the mail-auto-archive-file-name) can
;; be specified using using another variable called email-archive-file-name.
;; This modification is needed because setting the former variable adds FCC
;; field automatically and this prevents me from adding the archive filename.
 
(require 'sendmail)
 
;; Customisable variables
 
(defvar email-mail-headers-list '("To" "Subject" "Resent-To" "From")
  "*List of header fields that must be scanned.")
 
(defvar email-fcc-alist nil
  "*Non-nil value should be an alist that provides a header-FCC mapping.
The alist is of the form
\((HEADER-NAME
  (REGEXP.FOLDER-NAME) ...
... ))
HEADER-NAME is one of the mail headers from the list `email-mail-headers-list'.
REGEXP is a string which is the pattern to match for and FOLDER-NAME is the nameof the folder into which the message is to be saved.  Once a match is found,
further searches are discarded.  One cannot therefore, save to multiple files.") 
(defvar email-cc-alist nil
  "*Non-nil value indicates an alist that provides a header-CC mapping.
The alist if of the form :
\((HEADER-NAME
  (REGEXP.CC-RECIPIENT-LIST) ...
... ))
HEADER-NAME is one of the mail headers from the list `email-mail-headers-list'.
REGEXP is a string which is the pattern to match for and CC-RECIPIENT-LIST is a
string indicating a comma-separated list of cc recipients.  Once a match is
found, further searches are discarded.")
 
(defvar email-id-alist nil
  "*Non-nil must be a alist that will say if an id must be added for a mail
The alist is of the form :
\((HEADER-NAME
  (REGEXP.t) ...
... ))
HEADER-NAME is one of the mail headers from the list `email-mail-headers-list'.
REGEXP is a string which is the pattern to match for.  The second half of the
subalist is \"t\" because its not needed.  Maybe it could be used to define a
different id format for different matches.")
 
(defvar email-use-fcc nil
  "*If t, the message to be sent is processed for fcc before sending.")
 
(defvar email-use-cc nil
  "*If t, the message to be sent is processed for cc before sending.")
 
(defvar email-use-ids nil
  "*If t, all mail messages must be scanned for possibility of adding an id.")
 
(defvar email-id-format (concat (user-login-name) "- ")
  "*If non-nil, specifies the format of the id being appended to the subject
of the mail.  Default is \"USERNAME-ID, \" where USERNAME is `(user-login-name)'                                                                                
and ID is the number retrieved from the id file.")
 
(defvar email-id-file "~/.mailrc"
  "*This gives the name of the file from which the id is to be retrieved.
By default, its the .mailrc file.  The file must be writable by the user for
addition of id to the subject of the message being sent.")
 
;; This variable is needed because, I cannot use mail-archive-file-name for the
;; same purpose, as that would add the FCC field in the headers when the mail
;; mode is invoked and we do not add an FCC field if one already exists.
 
(defvar email-archive-file-name nil
  "*If non-nil, this gives the folder where outgoing messages are recorded
in case they do not match any of the regexps provided by `email-fcc-alist'.")
 
(defvar email-expand-headers t
  "*If non-nil, `(email-expand-headers)' will expand the headers in the
mail buffer itself.")
 
(defvar email-send-mail-function 'sendmail-send-it
  "*Name of function originally called when mail is to be sent.")
 
(define-key mail-mode-map "\C-c\C-e" 'email-expand-headers)
 
;; Start of Functions
 
(defun email-send ()
  "Preprocess a mail buffer to add cc, fcc and id.
This is typically invoked by binding the variable mail-send-function to this
function.  This function invokes sendmail-send-it as the final step."
  (let ((delimline)
 (mtmpbuf (generate-new-buffer " mymail temp"))
 (case-fold-search nil)
 (mailbuf (current-buffer)))
    (unwind-protect
 (save-excursion
   (set-buffer mtmpbuf)
   (erase-buffer)
   (insert-buffer-substring mailbuf)
   (if email-expand-headers
       (save-excursion
  (goto-char (point-min))
  (if (re-search-forward "^--text follows this line--" (point-max) t)
      (setq delimline (point-marker)))
  (if email-use-ids
      (email-add-id))
  (if email-use-fcc
      (email-add-fcc))
  (if email-use-cc
      (email-add-cc))))
   (setq email-expand-headers t)
   (funcall email-send-mail-function)
   (kill-buffer mtmpbuf)))))
 
(defun email-add-fcc ()
  "Add an FCC field for a given mail based on the value of email-fcc-alist.
If that variable is non-nil and no match is found, the value is set to the valueof email-auto-archive-filename."
  (let (email-outbox)
    (if (and email-fcc-alist (consp email-fcc-alist))
 (progn
   (setq email-outbox (email-get-key email-fcc-alist))
   (if (not email-outbox)
       (setq email-outbox email-archive-file-name))
   (save-excursion
   (goto-char (point-min))
   (if (and email-outbox (not (re-search-forward "^FCC: " delimline t)))
       (save-excursion
  (goto-char (point-min))
  (re-search-forward "^Subject: " delimline t)
  (end-of-line)
  (insert (concat "\nFCC: " email-outbox)))))))))
 
(defun email-add-cc ()
  "Add a CC field to a mail message based on the value of email-cc-alist.
The variable email-cc-alist is scanned for a match with the current mail headers.
If it succeeds, a cc field is added (or appended if a cc already exists) to the
mail headers."
  (if (and email-cc-alist (consp email-cc-alist))
      (let (email-cc)
 (setq email-cc (email-get-key email-cc-alist))
 (if (and email-cc (not (re-search-forward "^CC: " delimline t)))
     (save-excursion
       (goto-char (point-min))
       (re-search-forward "^To: " delimline t)
       (end-of-line)
       (insert (concat "\nCC: " email-cc)))
   (save-excursion
     (end-of-line)
     (insert (concat "," email-cc)))))))
       
(defun email-add-id ()
  "Prefix the Subject with email-id-format string based on email-id-alist.
This function retrieves the next id to be used from the .mailrc file.  The id line
must be the first line of the file and must be of the form #<id>. For eg. #100."  (if (and email-id-alist (consp email-id-alist))
      (let (email-id-key email-id)
 (setq email-id-key (email-get-key email-id-alist))
 (if email-id-key
     (save-excursion
       (setq email-id (email-retrieve-id))
       (re-search-forward "^Subject: " delimline t)
       (insert (concat email-id-format email-id ",")))))))
 
(defun email-retrieve-id ()
  "This function loads the user's .mailrc and retrieves the id from that file.
It increments that id and saves the file too."
  (let ((email-mailrc-buffer (get-buffer-create " email-id "))
 (email-id-val)
 (email-sw)
 (email-ew))
    (save-excursion
      (set-buffer email-mailrc-buffer)
      (if (and (file-exists-p email-id-file) (file-writable-p email-id-file))
   (save-excursion
     (insert-file email-id-file)
     (goto-char (point-min))
     (forward-char)
     (setq email-sw (point))
     (end-of-line)
     (setq email-ew (point))
     (setq email-id-val (buffer-substring email-sw email-ew))
     (goto-char (point-min))
     (kill-line)
     (insert (concat "#" (+ 1 (string-to-number email-id-val))))
     (write-file email-id-file)
     (kill-buffer email-mailrc-buffer)
     email-id-val)))))
 
(defun email-find-match (mainkey alist)
  "This is an associate function used by the email package.
It picks up the next word in the line (word is terminated bu a space, comma
or newline) and checks if thats present in the given alist.  The mainkey variable
gives the current mail header name."
  (let (email-start-word email-end-word email-buf)
    (setq email-start-word (point))
    (re-search-forward "[ ,\n]" delimline t)
    (setq email-end-word (1- (point)))
    (if (and email-start-word email-end-word)
 (setq email-buf (buffer-substring email-start-word email-end-word)))
    (cdr (assoc2 mainkey email-buf alist))))
 
(defun email-get-key (alist)
  "Given an alist, it returns the first match found in the alist.
Used by the email package."
  (let (mainkey-list mainkey outvar)
    (setq mainkey-list email-mail-headers-list)
    (catch 'foo
      (while mainkey-list
 (goto-char (point-min))
 (setq mainkey (car mainkey-list))
 (if (re-search-forward (concat "^"mainkey ": ") delimline t)
     (progn
       (setq outvar (email-find-match mainkey alist))
       (if (not outvar)
    (setq mainkey-list (cdr mainkey-list))
  (throw 'foo outvar)))
   (setq mainkey-list (cdr mainkey-list)))))))
 
(defun email-expand-headers ()
  "Expand mail headers inline.
This expands the header fields like CC, FCC and modifies the subject field if
necessary within the current mail buffer.  This is provided so that one can
confirm the behaviour of the email package given the user's settings."
  (interactive)
  (let ((delimline)
 (case-fold-search nil))
    (if email-expand-headers
 (save-excursion
   (goto-char (point-min))
   (if (re-search-forward "^--text follows this line--" (point-max) t)
       (setq delimline (point-marker)))
   (if email-use-ids
       (email-add-id))
   (if email-use-fcc
       (email-add-fcc))
   (if email-use-cc
       (email-add-cc))
   (setq email-expand-headers nil)))))
 
 
(defun assoc2 (mainkey key alist)
  "Returns the list for the key within the alist for mainkey.
Used by the email package."
  (let ((assoc2-sublist))
    (catch 'foo
      (if (not (and (consp alist) (assoc mainkey alist)))
   (throw 'foo 'nil))
      (setq assoc2-sublist (cdr (assoc mainkey alist)))
      (while assoc2-sublist
 (if (string-match (car (car assoc2-sublist)) key)
     (throw 'foo (car assoc2-sublist)))
   (setq assoc2-sublist (cdr assoc2-sublist)))
      (throw 'foo 'nil))))
 
(provide 'email)
 
;;; email.el ends here

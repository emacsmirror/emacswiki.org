;;; w32-send-mapi.el --- send e-mail messages using MAPI

;; Copyright (C) 2005 Mathias Dahl

;; Version: 0.3
;; Keywords: w32, e-mail, vbscript, MAPI
;; Author: Mathias Dahl <mathias.removethis.dahl@gmail.com>
;; Maintainer: Mathias Dahl
;; URL: http://www.emacswiki.org/cgi-bin/wiki/WThirtyTwoSendMAPI

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Even though I have to use MS Outlook at work I like writing my
;; e-mails in a normal *mail* buffer in Emacs. I usually copy and
;; paste the text from there into the mail. One day I got tired of
;; that and hacked together this little package.
;;
;; It uses MAPI to send a message by fetching information from a
;; normal *mail* buffer to build some vbscript code that is executed
;; by wscript.exe.
;;

;;; Prerequisites:

;;
;; * MAPI must be working on your machine
;;
;; * It needs wscript.exe to be able to execute the vbscript doing the
;;   actual work.
;;

;;; Usage:

;; Open up a mail buffer (C-x m).
;; Add one or more recipients, a subject and a body.
;; Execute it: M-x w32-send-mapi.

;;; History:

;; Version 0.3, 2007-07-04
;; * Added w32-send-mapi-send
;; * Added w32-send-mapi-send-region

;; Version 0.2, 2005-03-07
;; * Fixed some things after suggestions from Reiner Steib.
;; * Used `format' instead of a lot of calls to `insert'.
;; * Replaced "Chr(13) & Chr(10)" with "vbCrLf" in generated code

;; Version 0.1, 2005-03-07
;; * First release.

;;; Bugs:

;; Probably a bunch of them.

;;; Todo:

;;; Code:

(require 'message)

(defvar w32-send-mapi-vbscript-template "
Option Explicit
Dim oMess
Dim oSess

Sub Main

  ' To make generated code smaller and (probably) slightly faster
  Dim vbCrLf
  vbCrLf = Chr(13) & Chr(10)

  ' Constant for main recipient
  Const mapToList = 1

  ' Constant for CC
  Const mapCcList = 2

  ' Constant for BCC
  Const mapBccList = 3

  On Error Resume Next

  ' Try to create MAPI session
  Set oSess = CreateObject(\"MSMAPI.MAPISession\")

  If Err.Number <> 0 Then
    MsgBox \"Could not find MAPI. Exiting...\"
    Exit Sub
  End If

  Set oMess = CreateObject(\"MSMAPI.MAPIMessages\")

  On Error GoTo 0

  ' We do not care about new mails right now
  oSess.DownLoadMail = False

  ' Logon MAPI
  oSess.SignOn

  ' Set session ID for the message, using our active session
  oMess.SessionID = oSess.SessionID

  ' Start composing message
  oMess.Compose

  ' Subject
  oMess.MsgSubject = \"%s\"

  ' Body
  oMess.MsgNoteText = \"%s\"

  ' For now, just add a normal recipient. For other types of
  ' recipients, use for exampe mapCcList to add CC
  ' recipients. Increase index for each recipient.

  ' Note that you do not need to add a recipient at all for the
  ' MAPI call to work.

  ' Recipient names inserted here

  ' Limitataion:

  ' Note that we make use of only the name, not the e-mail
  ' address. This means we rely on the MUA to expand the names to
  ' valid e-mail addresses before the mail is sent.

  ' Generated recipient-code inserted here:

%s

  ' Catch error which is thrown by the Send method if user
  ' cancels mail

  On Error Resume Next

  ' Open up the mail application

  oMess.Send True

  If Err.Number <> 0 Then
    MsgBox \"Could not send message. Reason: \" & Err.Description
    Exit Sub
  End If

  On Error GoTo 0

  oSess.SignOff

End Sub

' Execute main Sub
Main

' Clean up
Set oSess = Nothing
Set oMess = Nothing

"
  "VBScript code template")

(defun w32-send-mapi-mail-header-recipients ()
  "Fetch a list of strings containing the recipients from the
mail header"
  (save-excursion
    (goto-char (point-min))
    (split-string
     (cdr (assoc 'to (mail-header-extract-no-properties))) "[,;]")))

(defun w32-send-mapi-mail-header-subject ()
  "Fetch the subject from the mail header."
  (save-excursion
    (goto-char (point-min))
    (cdr (assoc 'subject (mail-header-extract-no-properties)))))

(defun w32-send-mapi-generate-recipients-code (to-list)
  "Using a list of recipients, create the vbscript code
necessary to add them."
  (let ((count 0)
        (to-list-code nil))
    (if (stringp to-list)
        (setq to-list (list to-list)))
    (while to-list
      (setq to-list-code
            (concat to-list-code
                    "  oMess.RecipType = mapToList\n"
                    "  oMess.RecipIndex = " (int-to-string count) "\n"
                    "  oMess.RecipDisplayName = Trim(\""
                    (car to-list) "\")\n"))
      (setq count (1+ count))
      (setq to-list (cdr to-list)))
    to-list-code))

(defun w32-send-mapi-send (recipients subject body)
  "Create MAPI message.
RECIPIENTS is a string of one recipient or a list of strings with
recipients. SUBJECT is the subject of the message and BODY the
message body."
  (let ((buf (get-buffer-create "*w32-send-mapi*"))
        (script-file-name (expand-file-name "~/w32-send-mapi.vbs")))
    (set-buffer buf)
    (erase-buffer)
    (insert (format w32-send-mapi-vbscript-template subject
                    (with-temp-buffer
                      (insert body)
                      (goto-char (point-min))
                      ;; Escape all quotation characters
                      (replace-string "\"" "\"\"")
                      (goto-char (point-min))
                      ;; Sligthly ugly way to handle newlines (seems to work well
                      ;; though)
                      (replace-string "\n" "\" & vbCrLf & _ \n       \"")
                      (buffer-substring-no-properties
                       (point-min) (point-max)))
                    (if recipients
                        (w32-send-mapi-generate-recipients-code recipients)
                      "' No recipients")))
    ;; Write the script to a file
    (write-file script-file-name)
    (kill-buffer buf)
    (setq script-file-name
          (substitute ?\\ ?/ script-file-name))
    ;; Execute script
    (w32-shell-execute nil "wscript.exe" script-file-name)))

(defun w32-send-mapi ()
  "Send a mail using MAPI using the information in a normal
*mail* buffer. Requires wscript.exe to work."
  (interactive)
  (save-excursion
    (let ((script-file-name (expand-file-name "~/w32-send-mapi.vbs"))
          to-list-code
          to-list
          subject-text
          body-text
          (case-fold-search t))
      ;; Get recipients and subject
      (setq to-list (w32-send-mapi-mail-header-recipients))
      (setq subject-text (w32-send-mapi-mail-header-subject))
      ;; Body
      (goto-char (point-min))
      (re-search-forward (concat "^" (regexp-quote
                                      mail-header-separator)))
      (skip-chars-forward "\n")
      (setq body-text (buffer-substring-no-properties
                       (point) (point-max)))
      (w32-send-mapi-send to-list subject-text body-text))))

(defun w32-send-mapi-send-region (beg end recipients subject)
  "Send region via MAPI."
  (interactive "r\nsRecipients (separate with comma or semicolon): \nsSubject: ")
  (w32-send-mapi-send (split-string recipients "[,;]")
                      subject
                      (buffer-substring-no-properties beg end)))

(provide 'w32-send-mapi)

;;; w32-send-mapi.el ends here

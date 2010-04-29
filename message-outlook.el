;;; message-outlook.el --- Create function for sending messages with M$ outlook
;; 
;; Filename: message-outlook.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Wed Apr 28 08:17:06 2010 (-0500)
;; Version: 
;; Last-Updated: Wed Apr 28 10:46:17 2010 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 81
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;  Requires wscript. 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:


(defvar message-outlook-script-start "
Set objOutlk = createobject(\"Outlook.Application\")
Dim objOutlk    'Outlook
Dim objMail     'Email item
Dim strMsg
Const olMailItem = 0

'Create a new message
Set objOutlk = createobject(\"Outlook.Application\")
Set objMail = objOutlk.createitem(olMailItem)
"
  "vbs script start for outlook mail message"
  )

(defvar message-outlook-script-stop "
objMail.%s 'Use this To display before sending, otherwise call objMail.Send to send without reviewing
'Clean up
Set objMail = nothing
Set objOutlk = nothing
"
  "vbs script stop for outlook mail message"
  )
(defcustom message-outlook-display-instead-of-send 't
  "If non-nil, messages are displayed instead of sent through outlook.  Many scripts are disabled from sending programatically because of email viruses."
  :group 'message-sending)

(defun message-send-mail-quote-vbs (body &optional file)
  "Quotes VBS send-mail"
  (with-temp-buffer
    (insert body)
    (goto-char (point-min))
    ;; Escape all quotation characters
    (replace-string "\"" "\"\"")
    (goto-char (point-min))
    ;; Sligthly ugly way to handle newlines (seems to work well
    ;; though)
    (replace-string "\n" "\" & vbCrLf & _ \n       \"")
    (when file
      (goto-char (point-min))
      (while (re-search-forward "/" nil t)
        (replace-match "\\" nil 't))
      )
    (buffer-substring-no-properties
     (point-min) (point-max)))
  )


(defun message-send-mail-with-outlook ()
  "Create specified outlook message"
  (let (
        (s-start message-outlook-script-start)
        (s-end (format message-outlook-script-stop (if message-outlook-display-instead-of-send "display" "Send")))
        (to (message-fetch-field "To"))
        (bcc (message-fetch-field "Bcc"))
        (cc (message-fetch-field "Cc"))
        (subject (message-fetch-field "Subject"))
        (importance (message-fetch-field "Importance"))
        field
        value
        (msg "")
        (buf (buffer-substring (point-min) (point-max)))
        (vbs (make-temp-file "message-outlook" nil ".vbs"))
        )
    (with-temp-buffer
      (insert buf)
    (run-hooks 'message-send-mail-hook)
    (when to
      (setq msg (concat msg "objMail.To = \"" 
                        (message-send-mail-quote-vbs to) "\"\n"))
      (message-remove-header "To")
      )
    (when bcc
      (setq msg (concat msg "objMail.Bcc = \"" 
                        (message-send-mail-quote-vbs bcc) "\"\n"))
      (message-remove-header "Bcc")
      )
    (when cc
      (setq msg (concat msg "objMail.Cc = \"" 
                        (message-send-mail-quote-vbs cc) "\"\n"))
      (message-remove-header "Cc")
      )
    (when subject
      (setq msg (concat msg "objMail.Subject = \"" 
                        (message-send-mail-quote-vbs subject) "\"\n"))
      (message-remove-header "Subject")
      )
    (when (and importance (and (string= importance "high")))
      (setq msg (concat msg "objMail.Importance = 2\n"))
      (message-remove-header "Importance")
      )
    (when nil
      (message-remove-header "From")
      (save-excursion
        (save-restriction
          (message-narrow-to-headers-or-head)
          (goto-char (point-min))
          (while (re-search-forward "^\\(.+\\):" nil t)
            (setq field (match-string 1))
            (setq value (message-fetch-field field))
            (message-remove-header field)
            (when value
              (setq msg (concat msg "Dim property As ItemProperty\nSet property =objMail.ItemProperties.Add(\""
                                (message-send-mail-quote-vbs field) "\")\nproperty.value=\"" "\"\nobjMail.Save\n"))
              (goto-char (point-min))
              )
            )
          )
        )
      )
    ; Now get attachments;  Currently doesn't support buffer attachments.
    (goto-char (point-min))
    (while (re-search-forward "<#\\(?:part\\|external\\) \\(.\\|\n\\)*?\\<\\(?:file\\)?name=\"\\(.*\\)\"[ \t\n]*disposition=attachment>" nil t)
      (setq value (match-string 2))
      (replace-match "")
      (delete-region (point) (progn (re-search-forward "<#/\\(?:part\\|external\\)>[ \t\n]*" nil t) (point)))
      (setq msg (concat msg "objMail.attachments.add(\"" (message-send-mail-quote-vbs value 't) "\")\n"))
      )
    (goto-char (point-min))
    (delete-region (point-min) (progn (search-forward "--text follows this line--")))
    (setq value (buffer-substring (point-min) (point-max)))
    (setq msg (concat msg "objMail.body = \"" (message-send-mail-quote-vbs value 't) "\"\n"))
    (setq msg (concat s-start msg s-end))
    (with-temp-file vbs
      (insert msg)
      )
    (shell-command-to-string (concat "wscript.exe " vbs))
    (delete-file vbs)
    )
  )
  )
(setq message-send-mail-function 'message-send-mail-with-outlook)
(provide 'message-outlook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; message-outlook.el ends here

;;; org-outlook.el --- Outlook org
;; 
;; Filename: org-outlook.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer:
;; Created: Mon May 10 09:44:59 2010 (-0500)
;; Version: 0.8
;; Last-Updated: Tue May 29 22:21:06 2012 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 166
;; URL: https://github.com/mlf176f2/org-outlook.el
;; Keywords: Org-outlook 
;; Compatibility:
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; * Introduction:
;; Org mode lets you organize your tasks. However, sometimes you may wish
;; to integrate org-mode with outlook since your company forces you to
;; use Microsoft Outlook.  [[file:org-outlook.el][org-outlook.el]] allows: 
;; - Creating Tasks from outlook items:
;;   - org-outlook-task. All selected items in outlook will be added to a
;;     task-list at current point. This version requires org-protocol and   
;;     org-protocol.vbs.  The org-protocol.vbs has can be generated with
;;     the interactive function `org-outlook-create-vbs'.
;; 
;; - Open Outlook Links in org-mode
;; 
;;   - Requires org-outlook-location to be customized when using Outlook
;;     2007 (this way you don’t have to edit the registry).
;; 
;; This is based loosely on:
;; http://superuser.com/questions/71786/can-i-create-a-link-to-a-specific-email-message-in-outlook
;; 
;; 
;; Note that you may also add tasks using visual basic directly. The script below performs the following actions:
;; 
;;    - Move email to Personal Folders under folder "@ActionTasks" (changes GUID)
;;    - Create a org-mode task under heading "* Tasks" for the file `f:\Documents\org\gtd.org'
;;    - Note by replacing "@ActionTasks", "* Tasks" and
;;      `f:\Documents\org\gtd.org' you can modify this script to your
;;      personal needs.
;; 
;; The visual basic script for outlook is:
;; 
;; 
;; 
;; Sub CreateTaskFromItem()
;;     Dim T As Variant
;;     Dim Outlook As New Outlook.Application
;;     Dim orgfile As Variant
;;     Dim Pos As Integer
;;     Dim taskf As Object
;;     
;;     Set myNamespace = Outlook.GetNamespace("MAPI")
;;     Set myPersonalFolder = myNamespace.Folders.item("Personal Folders")
;;     Set allPersonalFolders = myPersonalFolder.Folders
;;     
;;     T = ""
;;     For Each Folder In allPersonalFolders
;;         If Folder.Name = "@ActionTasks" Then
;;             Set taskf = Folder
;;             Exit For
;;         End If
;;     Next
;;     
;;     If Outlook.Application.ActiveExplorer.Selection.Count > 0 Then
;;         For I = 1 To Outlook.Application.ActiveExplorer.Selection.Count
;;                 Set objMail = Outlook.ActiveExplorer.Selection.item(I)
;;                 Set objMail = objMail.Move(taskf)
;;                 objMail.Save 'Maybe this will update EntryID
;;                 T = T + "** TODO " + objMail.Subject + " :OFFICE:" + vbCrLf
;;                 T = T + "[[outlook:" + objMail.EntryID + "][MESSAGE: " + objMail.Subject + " (" + objMail.SenderName + ")]]"
;;                 T = T + vbCrLf + vbCrLf
;;                 T = T + objMail.Body
;;                 T = T + vbCrLf + vbCrLf
;;         Next
;;         ' Now that we have the org-mode tasks, add to org-mode file
;;         orgfile = GetFile("f:\Documents\org\gtd.org")
;;         Pos = InStr(1, orgfile, "* Tasks", vbTextCompare)
;;         orgfile = Mid(orgfile, 1, Pos + Len("* Tasks") + 1) + vbCrLf + T + Mid(orgfile, Pos + Len("* Tasks") + 1, Len(orgfile))
;;         orgfile = Replace(orgfile, vbCrLf, Chr(10)) ' Change to unix line endings.
;;         WriteFile "f:\Documents\org\gtd.org", orgfile
;;     Else
;;         MsgBox "No Message(s) Selected"
;;     End If
;;  
;; End Sub
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 12-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Tue May 29 22:21:06 2012 (-0500) #166 (Matthew L. Fidler)
;;    Updated Visual Basic Script to be more robust, and have more options.
;; 07-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Tue May 29 22:21:06 2012 (-0500) #166 (Matthew L. Fidler)
;;    Should fix Issue #1.  Also added org-outlook-create-vbs to create the
;;    VBS code based on a user's setup.
;; 26-May-2012    Matthew L. Fidler  
;;    Last-Updated: Sat May 26 11:13:22 2012 (-0500) #163 (Matthew L. Fidler)
;;    Added (require 'cl), Thanks Robert Pluim
;; 21-Feb-2012    Matthew L. Fidler  
;;    Last-Updated: Tue Feb 21 11:15:02 2012 (-0600) #160 (Matthew L. Fidler)
;;    Bug fix for opening files.
;; 21-Feb-2012      
;;    Last-Updated: Tue Dec 13 08:41:29 2011 (-0600) #156 (Matthew L. Fidler)
;;    Bug fix.
;; 13-Dec-2011    Matthew L. Fidler  
;;    Last-Updated: Tue Dec 13 08:41:10 2011 (-0600) #155 (Matthew L. Fidler)
;;    Added more autoload cookies.
;; 08-Apr-2011      
;;    Last-Updated: Fri Apr  8 08:49:38 2011 (-0500) #151 (US041375)
;;    Added some autoload cookies.
;; 15-Feb-2011    Matthew L. Fidler  
;;    Last-Updated: Tue Jan 11 12:21:26 2011 (-0600) #147 (Matthew L. Fidler)
;;    Changed outlook-org to org-outlook.el
;; 11-Jan-2011    Matthew L. Fidler  
;;    Last-Updated: Tue Jan 11 00:21:21 2011 (-0600) #140 (Matthew L. Fidler)
;;    Finalized interface with org-protocol
;; 05-Jan-2011    Matthew L. Fidler  
;;    Last-Updated: Wed Jan  5 12:39:59 2011 (-0600) #42 (Matthew L. Fidler)
;;    Removed outlook copy.  I only use from outlook now.
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
(require 'cl)
(require 'org)
(require 'org-protocol)


(defgroup org-outlook nil
  "Org Outlook"
  :group 'org-mode)

(defcustom org-outlook-location (w32-short-file-name "c:/Program Files/Microsoft Office/OFFICE12/OFFICE12/OUTLOOK.exe")
  "* Microsoft Outlook 2007 location."
  :type 'string
  :group 'org-outlook)

(defcustom org-outlook-capture 'org-capture
  "Capturing system for org-protocol outlook: subprotocol.  Supports org-capture"
  :type 'sexp
  :group 'org-outlook)

(defcustom org-protocol-outlook-default-template-key nil
  "Default template for org-capture or remember."
  :type 'sexp
  :group 'org-outlook)

;;;###autoload
(defun org-outlook-open (id)
  "Open the Outlook item identified by ID.  ID should be an Outlook GUID."
  ;; Change this to work with Outlook 2007 without changing the
  ;; registry.
  (setq debug-on-error 't)
  (if (and org-outlook-location (file-exists-p org-outlook-location))
      (w32-shell-execute "open" org-outlook-location
                         (concat "/select \"outlook:" id "\""))
    (w32-shell-execute "open" (concat "outlook:" id))))

(defvar org-outlook-dir (file-name-directory
                         (or
                          load-file-name
                          (buffer-file-name)))
  "Org outlook directory")

;;;###autoload
(defun org-outlook-create-vbs ()
  "Creates Visual Basic Code for Org-protocol"
  (interactive)
  (let* ((move-to-folder (yes-or-no-p "Would you like to move the emails to another PST mailbox?"))
         (move-to-subfolder (if (not move-to-folder)
                                (yes-or-no-p "Would you like to move the emails to a subfolder?")
                              nil))
         (script (concat 
                 "'**************************************
' Name: URLEncode Function
' Description:Encodes a string to create legally formatted
'QueryString for URL. This function is more flexible
'than the IIS Server.Encode function because you can
'pass in the WHOLE URL and only the QueryString data
'will be converted. IIS strangely converts EVERYTHING
'(ie \"http://\" becomes \"http%3A%2F%2F\").
' By: Markus Diersbock
'
' Inputs:sRawURL - String to Encode
'
' Returns:Encoded String
'
'This code is copyrighted and has' limited warranties.
'Please see http://www.Planet-Source-Code.com/vb/scripts/ShowCode.asp?txtCodeId=43806&lngWId=1'for details.
'**************************************

' Changed by Matthew Fidler to have http:// become http%3A%2F%2F
' Also changed to have spaces be %20 instead of +


Public Function URLEncode(sRawURL As String) As String
    On Error GoTo Catch
    Dim iLoop As Integer
    Dim sRtn As String
    Dim sTmp As String
    Const sValidChars = \"1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\"
    If Len(sRawURL) > 0 Then
        ' Loop through each char
        For iLoop = 1 To Len(sRawURL)
            sTmp = Mid(sRawURL, iLoop, 1)
            If InStr(1, sValidChars, sTmp, vbBinaryCompare) = 0 Then
                ' If not ValidChar, convert to HEX and prefix with %
                sTmp = Hex(Asc(sTmp))
                If Len(sTmp) = 1 Then
                    sTmp = \"%0\" & sTmp
                Else
                    sTmp = \"%\" & sTmp
                End If
            End If
            sRtn = sRtn & sTmp
        Next iLoop
        URLEncode = sRtn
    End If
Finally:
    Exit Function
Catch:
    URLEncode = \"\"
    Resume Finally
End Function

' From http://www.freevbcode.com/ShowCode.asp?ID=3476
Public Function OutlookFolderNames(objFolder As Outlook.MAPIFolder, strFolderName As String) As Object
'*********************************************************
    On Error GoTo ErrorHandler
    Dim objOneSubFolder As Outlook.MAPIFolder
    If Not objFolder Is Nothing Then
        If LCase(strFolderName) = LCase(objFolder.Name) Then
            Set OutlookFolderNames = objFolder
        Else
            ' Check if folders collection is not empty
            If objFolder.Folders.Count > 0 And _
                   Not objFolder.Folders Is Nothing Then
                For Each oFolder In objFolder.Folders
                    Set objOneSubFolder = oFolder
                    ' only check mail item folder
                    If objOneSubFolder.DefaultItemType _
                         = olMailItem Then
                        If LCase(strFolderName) = _
                          LCase(objOneSubFolder.Name) Then
                            Set OutlookFolderNames = _
                                   objOneSubFolder
                            Exit For
                        Else
                            If objOneSubFolder.Folders.Count _
                                > 0 Then
                                Set OutlookFolderNames = _
                                  OutlookFolderNames _
                                (objOneSubFolder, strFolderName)
                            End If
                        End If
                    End If
                Next
            End If
        End If
    End If

    Exit Function

ErrorHandler:
    Set OutlookFolderNames = Nothing
End Function


Sub CreateTaskFromItem()
    Dim T As Variant
    Dim SndName As String
    Dim SndEmailAddress As String
    Dim Outlook As New Outlook.Application
    Dim allPersonalFolders As Outlook.MAPIFolder
    " (if (not (or move-to-folder move-to-subfolder)) ""
        (concat "Dim taskf As Object
    
    Set myNamespace = Outlook.GetNamespace(\"MAPI\")
"
        (if move-to-folder
            (concat "
    Set myPersonalFolder = myNamespace.Folders.Item(\""
                 (read-from-minibuffer "Personal Folder Name: ")
  "\")
    Set allPersonalFolders = myPersonalFolder")
          "Set allPersonalFolders = myNamespace.GetDefaultFolder(olFolderInbox)")
  "
    
    T = \"\"
    Set taskf = OutlookFolderNames(allPersonalFolders,\""
  (read-from-minibuffer "Subfolder to put tasks in: " "@ActionTasks") "\")\n")) "
    
    ' Send selected text to clipboard.
    ' SendKeys (\"%E\")
    ' SendKeys (\"C\")
    ' DoEvents
    
    
    Set objWeb = CreateObject(\"InternetExplorer.Application\")
    
        
    If Outlook.Application.ActiveExplorer.Selection.Count > 0 Then
        For i = 1 To Outlook.Application.ActiveExplorer.Selection.Count
            Set objMail = Outlook.ActiveExplorer.Selection.Item(i)
            On Error GoTo BlockedSnd
            SndName = ObjMail.SenderName
            SndEmailAddress = ObjMail.SenderEmailAddress
            GoTo SndDone
BlockedSnd:
            SndName = \"Blocked\"
            SndEmailAddress = \"Blocked@Microsoft.com\"
SndDone:
            " (if (or move-to-folder move-to-subfolder) "On Error GoTo CantMove
            Set objMail = objMail.Move(taskf)
            GoTo CanMove
CantMove:
            MsgBox \"Can't Move to the folder.
CanMove:
"
                "") "
            On Error GoTo 0
            objMail.Save 'Maybe this will update EntryID
            T = \"org-protocol:/outlook:/o/\" + URLEncode(objMail.EntryID) _
                    + \"/\" + URLEncode(objMail.Subject) _
                    + \"/\" + URLEncode(SndName) _
                    + \"/\" + URLEncode(SndEmailAddress) _
                    '+ \"/\" + URLEncode(objMail.Body)
            objWeb.Navigate T
            objWeb.Visible = True
        Next
    End If
End Sub")))
    (with-temp-file (expand-file-name "org-protocol.vbs" org-outlook-dir)
      (insert script))))

;;;###autoload
(eval-after-load "org" '(org-add-link-type "outlook" 'org-outlook-open))

;;;###autoload
(defun org-protocol-outlook (info)
  "Process an org-protocol://outlook:// style url.

The sub-protocol used to reach this function is set in
`org-protocol-protocol-alist'.

This function detects the Message ID, Subject, Sender and
optional text separated by '/'.  For example either

org-protocol://outlook:/ID/Subject/SenderName/SenderAddress

or

org-protocol://outlook:/o/ID/Subject/SenderName/SenderAddress

works.

By default, it uses the character
`org-protocol-outlook-default-template-key', which should be associated
with a template in `org-capture-templates'.

To use this plugin:
- Copy the outlook macro (below) into outlook
- Modify the outlook capture template (o) to capture the email as
  a task. An example is below.


 (\"o\" \"org-outlook\" entry (file \"~/org/refile.org\") \"* TODO Email %c %?
  %i
  %U\" :clock-in t :clock-resume t)

You may also use the following placeholders

Placeholders Replacement 
%:link URL of the email
%:description The title of the message
%:title The title of the message 
%:initial Selected text.
%:sender Sender's name
%:sender-email Sender's Email

- (optional) Modify the folder/location that outlook moves mail into (moving
  mail off the server changes the message ID.  Once off the
  server, the ID remains the same unless you move it back...)
- (optional) Modify the capture template used (I use ``o'')
- (optional) Make the macro CreateTaskFromItem accessable
  anywhere from outlook by adding it to the quick access toolbar
  and/or the standard toolbar.
"
  (if (and (boundp 'org-stored-links)
           (or (fboundp org-outlook-capture))
           (org-protocol-do-outlook-capture info org-outlook-capture))
      (message "Org-mode not loaded."))
  nil)

(defvar org-stored-links '())

;;;###autoload
(defun org-protocol-do-outlook-capture (info capture-func)
  "Support `org-capture' and `org-remember' alike.
CAPTURE-FUNC is either the symbol `org-remember' or `org-capture'."
  (let* ((parts (org-protocol-split-data info t))
         (template (or (and (= 1 (length (car parts))) (pop parts))
                       org-outlook-protocol-default-template-key))
         (url (concat "outlook:" (org-protocol-sanitize-uri (car parts))))
         (type (if (string-match "^\\([a-z]+\\):" url)
                   (match-string 1 url)))
         (title (or (cadr parts) ""))
         (sender (or (caddr parts) ""))
         (sender-email (or (cadddr parts) ""))
         (region "")
         (orglink (org-make-link-string
                   url (if (string-match "[^[:space:]]" (format "%s (%s)" title sender)) (format "%s (%s)" title sender) url)))
         (org-capture-link-is-already-stored t) ;; avoid call to org-store-link
         remember-annotation-functions)
                                        ;(with-temp-buffer
                                        ;  (clipboard-yank)
                                        ;  (setq region (buffer-substring (point-min) (point-max))))
    (setq org-stored-links
          (cons (list url title) org-stored-links))
    (kill-new orglink)
    (org-store-link-props :type type
                          :link url
                          :sender sender
                          :sender-email sender-email
                          :description title
                          :title title
                          :annotation orglink
                                        ;:initial region
                          )
    (raise-frame)
    (funcall capture-func nil template)))

;;;###autoload
(eval-after-load "org-protocol"
  '(progn 
    (if (not (boundp 'org-protocol-protocol-alist))
        (setq org-protocol-protocol-alist nil))
    (add-to-list 'org-protocol-protocol-alist
                 '("outlook" :protocol "outlook"
                   :function org-protocol-outlook :kill-client t))))

(provide 'org-outlook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-outlook.el ends here

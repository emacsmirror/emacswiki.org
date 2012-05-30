;;; org-outlook.el --- Outlook org
;; 
;; Filename: org-outlook.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer:
;; Created: Mon May 10 09:44:59 2010 (-0500)
;; Version: 0.3
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
;; Adapted from http://superuser.com/questions/71786/can-i-create-a-link-to-a-specific-email-message-in-outlook
;; Allows selecting then inserting
;;
;; org-outlook-task creates task(s) from the selected item(s).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
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

;;;###autoload
(org-add-link-type "outlook" 'org-outlook-open)

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

Public Declare Function ShellExecute Lib \"shell32.dll\" Alias \"ShellExecuteA\" ( _
    ByVal hWnd As Long, _
    ByVal lpOperation As String, _
    ByVal lpFile As String, _
    ByVal lpParameters As String, _
    ByVal lpDirectory As String, _
    ByVal nShowCmd As Long) As Long


'Slightly Modified http://www.freevbcode.com/ShowCode.Asp?ID=5137
Function URLEncode(EncodeStr As String) As String
    Dim i As Integer
    Dim erg As String
    
    erg = EncodeStr

    ' *** First replace '%' chr
    erg = Replace(erg, \"%\", Chr(1))

    ' *** then '+' chr
    erg = Replace(erg, \"+\", Chr(2))
    
    For i = 0 To 255
        Select Case i
            ' *** Allowed 'regular' characters
            Case 37, 43, 48 To 57, 65 To 90, 97 To 122
            
            Case 1  ' *** Replace original %
                erg = Replace(erg, Chr(i), \"%25\")
        
            Case 2  ' *** Replace original +
                erg = Replace(erg, Chr(i), \"%2B\")
                
            Case 32
                erg = Replace(erg, Chr(i), \"%20\") 'org-protocol likes %20 instead of +
        
            Case 3 To 15
                erg = Replace(erg, Chr(i), \"%0\" & Hex(i))
        
            Case Else
                erg = Replace(erg, Chr(i), \"%\" & Hex(i))
                
        End Select
    Next
    
    URLEncode = erg
    
End Function


Sub CreateTaskFromItem()
    Dim T As Variant
    Dim Outlook As New Outlook.Application
    Dim ie As Object
    Set ie = CreateObject(\"InternetExplorer.Application\")

    
    Dim orgfile As Variant
    Dim Pos As Integer
    Dim taskf As Object
    
    Set myNamespace = Outlook.GetNamespace(\"MAPI\")

    ' Change this to be your personal folder item.  If it remains
    ' on the server it keeps the Outlook ID originally given.  If
    ' you move it to another folder, it will assign it to another
    ' ID, but keep that ID as long as you don't move it back to the
    ' server. (*sigh*  I wish it kept the same ID.)

    ' Technically this is unnecessary, but with my limited exchange
    ' account size,  I move my emails to \"Personal Folders\\@ActionTasks\" and
    ' then (possibly) refile from there. 

    Set myPersonalFolder = myNamespace.Folders.item(\"Personal Folders\")
    Set allPersonalFolders = myPersonalFolder.Folders
    
    T = \"\"
    For Each Folder In allPersonalFolders
        If Folder.Name = \"@ActionTasks\" Then
            Set taskf = Folder
            Exit For
        End If
    Next

    ' End moving message.
    
    If Outlook.Application.ActiveExplorer.Selection.Count > 0 Then
        For i = 1 To Outlook.Application.ActiveExplorer.Selection.Count
                Set objMail = Outlook.ActiveExplorer.Selection.item(i)
                Set objMail = objMail.Move(taskf)
                objMail.Save 'Maybe this will update EntryID
                ' Note that o is the Outlook capture template.
                T = \"org-protocol:/outlook:/o/\" + URLEncode(objMail.EntryID) _
                    + \"/\" + URLEncode(objMail.Subject) _
                    + \"/\" + URLEncode(objMail.SenderName) _
                    + \"/\" + URLEncode(objMail.SenderEmailAddress)
                ShellExecute 0, \"open\", T, vbNullString, vbNullString, vbNormalFocus
        Next
    End If
End Sub

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
(progn
  (if (not (boundp 'org-protocol-protocol-alist))
      (setq org-protocol-protocol-alist nil))
  (add-to-list 'org-protocol-protocol-alist
               '("outlook" :protocol "outlook"
                 :function org-protocol-outlook :kill-client t)))

(provide 'org-outlook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-outlook.el ends here

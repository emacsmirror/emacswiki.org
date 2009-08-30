;;; atom-blogger.el --- Utilities for Atom Blogger API
;;;$Id: atom-blogger.el,v 1.4 2006/01/04 19:01:28 raman Exp $
;;; $Author: raman $
;;; Description:  ATOM Blogger API
;;; Keywords: Blogger Atom API
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; atom-blogger| T. V. Raman |raman@google.com
;;; An emacs interface to Blogger|
;;; $Date: 2006/01/04 19:01:28 $ |
;;;  $Revision: 1.4 $ |
;;; Location undetermined
;;; License: GPL
;;;

;;}}}
;;{{{ Copyright:

;;; Copyright (c) 2005--2006, Google Inc.
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without modification,
;;; are permitted provided that the following conditions are met:

;;;     * Redistributions of source code must retain the above copyright notice,
;;;       this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright notice,
;;;       this list of conditions and the following disclaimer in the documentation
;;;       and/or other materials provided with the distribution.
;;;     * The name of the author may not be used to endorse or promote products
;;;       derived from this software without specific prior written permission.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY 
;;; WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF 
;;; SUCH DAMAGE.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; REMEMBER: blogger-edit.xsl is part of atom-blogger package

;;; Commentary: 
;;{{{  introduction

;;; Simple wrapper using Curl to post/edit Blogger.com Blogs
;;; This module will provide commands that prompt for the auth
;;; token and Feed/Entry URL,
;;; and pass it on to Curl.
;;; The XML results will be placed in a buffer in xml mode.
;;;nxml-mode is highly recommend and will be used if available.

;;;Usage:

;;; atom-blogger-new-entry          Create a new entry
;;; atom-blogger-post-entry         Publish newly created entry
;;; atom-blogger-edit-entry         Edit existing entry.
;;; atom-blogger-put-entry          Publish edits when ready.

;;; Commands prompt for the URI of the entry being manipulated ---
;;; this is the service.edit URI.
;;; You can customize things via custom, most useful being
;;; atom-blogger-default-auth which can be set to a user:password
;;; pair to avoid repeated prompting.

;;}}}
;;{{{  Required modules

(require 'cl)
(require 'derived)

;;}}}
;;{{{ Variables

(defgroup atom-blogger nil
  "Custom mode for Blogging."
  :group 'applications)

(defcustom atom-blogger-author (user-full-name)
  "Author name under which we post."
  :type 'string
  :group 'atom-blogger)

(defcustom atom-blogger-default-auth nil
  "Auth pair to use by default."
  :type '(choice
          (const :tag "none" nil)
          (string :tag "User:Password" ""))
  :group 'atom-blogger)
  

(defvar atom-blogger-directory
  (and load-file-name (file-name-directory load-file-name))
  "Directory where atom-blogger is installed. ")

(defvar atom-blogger-generator-name "http://purl.org/net/emacs-atom-blogger/"
  "Name of this generator.")

(defcustom atom-blogger-curl-program "curl"
  "Name of CURL executable."
  :type 'string
  :group 'atom-blogger)

(defcustom atom-blogger-xslt-program "xsltproc"
  "XSLT Program to use for filtering XML content."
  :type 'string
  :group 'atom-blogger)

(defvar atom-blogger-publish-action nil
  "This is set up by the various interactive comands to trigger
  the appropriate action when one is ready to publish.")
(defvar atom-blogger-new-entry-template
  "<entry xmlns=\"http://purl.org/atom/ns#\">
  <generator url=\"http://purl.org/net/emacs-atom-blogger\">http://purl.org/net/emacs-atom-blogger</generator>
  <author> <name>%s </name> </author>
  <title mode=\"escaped\" type=\"text/html\">%s </title>
  <content type=\"application/xhtml+xml\" xml:space=\"preserve\">
    <div xmlns=\"http://www.w3.org/1999/xhtml\">
<!--content goes here -->
    </div>
  </content>
</entry>"
  "Template for new Blogger entries.")

;;}}}
;;{{{ Define atom-blogger mode:

(define-derived-mode atom-blogger-mode xml-mode
  "Atom Blogger Interaction"
  "Major mode for Blogger interaction\n\n
\\{atom-blogger-mode-map"
  (auto-fill-mode 1))

(declaim (special atom-blogger-mode-map))
(define-key atom-blogger-mode-map "\C-c\C-c" 'atom-blogger-publish)

(defvar atom-blogger-this-url nil
  "Buffer local variable that records URL we came from.")

(make-variable-buffer-local 'atom-blogger-this-url)

(defvar atom-blogger-this-auth nil
  "Records authentication for this buffer.")

(make-variable-buffer-local 'atom-blogger-this-auth)

;;}}}
;;{{{ Helpers:

(defun atom-blogger-xslt (start end xsl)
  "Apply specified XSLT transform to region."
  (shell-command-on-region start end 
                           (format "%s %s - 2>/dev/null"
                                   atom-blogger-xslt-program xsl)
                           'replace))

;;}}}
;;{{{ Interactive Commands:

(defun atom-blogger-get-entry (url auth)
  "Retrieve specified entry.
`url' is the URL of the entry, 
`auth' is of the form username:password."
  (let ((buffer (get-buffer-create "*atom entry*"))
        (nxml-auto-insert-xml-declaration-flag nil))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (shell-command
       (format "%s --compressed -u %s %s 2>/dev/null"
               atom-blogger-curl-program auth url)
       buffer)
      (atom-blogger-mode)
      (setq atom-blogger-this-url url
            atom-blogger-this-auth auth))
    (switch-to-buffer buffer)))

(defun atom-blogger-edit-entry (url auth)
  "Retrieve entry and prepare it for editting.
The retrieved entry is placed in a buffer ready for editing.
`url' is the URL of the entry, 
`auth' is of the form username:password."
  (interactive
   (list
    (read-from-minibuffer "Entry URL:")
    (or atom-blogger-default-auth
        (read-passwd "User:Password"))))
  (declare (special atom-blogger-default-auth
                    atom-blogger-publish-action))
  (setq atom-blogger-publish-action 'atom-blogger-put-entry)
  (atom-blogger-get-entry url auth)
  (atom-blogger-xslt (point-min) (point-max)
                     (expand-file-name "blogger-edit.xsl"
                                       atom-blogger-directory))
  (goto-char (point-min))
  (flush-lines "^ *$")
  (goto-char (point-min))
  (search-forward "<content" nil t)
  (message
   (substitute-command-keys "Use \\[atom-blogger-publish] to publish your edits .")))

(defun atom-blogger-new-entry (url auth)
  "Create a new Blog post."
  (interactive
   (list
    (read-from-minibuffer "Post URL:")
    (or atom-blogger-default-auth
        (read-passwd "User:Password"))))
  (declare (special atom-blogger-default-auth
                    atom-blogger-new-entry-template
                    atom-blogger-publish-action))
  (let* ((title (read-string "Title: "))
        (buffer (get-buffer-create title)))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (atom-blogger-mode)
      (setq atom-blogger-this-url url
            atom-blogger-this-auth auth)
      (goto-char (point-max))
      (insert
       (format atom-blogger-new-entry-template
               atom-blogger-author title)))
      (switch-to-buffer buffer)
      (setq atom-blogger-publish-action 'atom-blogger-post-entry)
      (search-backward "<div" nil t)
      (forward-line 1)
      (message
       (substitute-command-keys "Use \\[atom-blogger-publish] to publish your edits ."))))

(defun atom-blogger-post-entry ()
  "Publish the Blog entry in the current buffer."
  (interactive)
  (declare (special atom-blogger-this-url
                    atom-blogger-this-auth
                    atom-blogger-curl-program))
  (unless (and (eq major-mode 'atom-blogger-mode)
               atom-blogger-this-url
               atom-blogger-this-auth)
    (error "Not in a correctly initialized Atom Entry."))
  (shell-command-on-region (point-min) (point-max)
                           (format "%s --compressed -H 'Content-type: application/xml' -X POST --data-binary @- -u %s %s"
                                   atom-blogger-curl-program
                                   atom-blogger-this-auth
                                   atom-blogger-this-url))
  )

(defun atom-blogger-put-entry ()
  "Publish the editted Blog entry in the current buffer."
  (interactive)
  (declare (special atom-blogger-this-url
                    atom-blogger-this-auth
                    atom-blogger-curl-program))
  (unless (and (eq major-mode 'atom-blogger-mode)
               atom-blogger-this-url
               atom-blogger-this-auth)
    (error "Not in a correctly initialized Atom Entry."))
  (shell-command-on-region (point-min) (point-max)
                           (format "%s -H 'Content-type: application/xml' -X PUT --data-binary @- -u %s %s"
                                   atom-blogger-curl-program
                                   atom-blogger-this-auth
                                   atom-blogger-this-url)))
(defun atom-blogger-publish ()
  "Publish current entry."
  (interactive)
  (declare (special atom-blogger-this-url
                    atom-blogger-this-auth
                    atom-blogger-publish-action))
  (unless (and (eq major-mode 'atom-blogger-mode)
               atom-blogger-publish-action
               (commandp atom-blogger-publish-action)
               atom-blogger-this-url
               atom-blogger-this-auth)
    (error "Not in a correctly initialized Atom Entry."))
  (call-interactively atom-blogger-publish-action)
  (message "Publishing  to %s" atom-blogger-this-url))

(defun atom-blogger-delete-entry (url auth)
  "Delete specified entry.
`url' is the URL of the entry, 
`auth' is of the form username:password."
  (interactive
   (list
    (read-from-minibuffer "Entry URL:")
    (or atom-blogger-default-auth
        (read-passwd "User:Password"))))
  (shell-command
   (format "%s --compressed -X DELETE -u %s %s 2>/dev/null"
           atom-blogger-curl-program auth url)))


      
;;}}}
(provide 'atom-blogger)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

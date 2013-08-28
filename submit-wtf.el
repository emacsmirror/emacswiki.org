;;; submit-wtf.el ---  Submit some horrible code to TheDailyWTF.com
;;
;; Author: Plamen K. Kosseff <p.kosseff [ a t ] anti-ad.org>
;; Created: 2010
;; Package-Requires: ()
;; URL: http://www.emacswiki.org/cgi-bin/wiki/submit-wtf.el
;; X-URL: https://raw.github.com/blackd/blackd/master/emacs/submit-wtf.el
;; License: GPL2
;; Version: 1.0
;; Keywords: WTF

;; Copyright (C) 2010 Plamen K. Kosseff
;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary: I can't remember where did i take the http-post function from

(require 'widget)
(require 'http-get)



(defcustom submit-wtf-name "Anonymous coward"
  "The name used to submit the WTF."
  :group 'the-daily-wtf)

(defcustom submit-wtf-email nil
  "The email used to submit the WTF."
  :group 'the-daily-wtf)

(defcustom submit-wtf-subject "See what I found!"
  "Default subject."
  :group 'the-daily-wtf)


(defcustom submit-wtf-dont-publis nil
  "If t the submision will be marked not for publishing.
If you just need to shere your pain without warring about consquences."
  :group 'the-daily-wtf
  :type 'boolean)

(defvar wtf-name)
(defvar wtf-email)
(defvar wtf-subject)
(defvar wtf-dontPublish)
(defvar wtf-comment)
(defvar wtf-source)
(defvar wtf-proc)


(defun wtf-submit ()
  "Submits the current region (selection) to TheDailyWTF.com.
A form will be shown to enter details."
  (interactive 
   (let ((source (buffer-substring-no-properties (point) (mark t)))
         (buff (get-buffer-create "*submit-wtf*"))
         (current-buff (current-buffer)))
     (kill-buffer buff)
     (setq buff (get-buffer-create "*submit-wtf*"))
     
     ;;(setq c (current-kill 0 t))
     (with-current-buffer buff
       (erase-buffer)
       (widget-insert "Submit some horrible code to TheDailyWTF.com by Plamen K. Kosseff\n\n\n\n")
       (widget-insert "Your name:             ")
       (setq wtf-name (widget-create 'editable-field
                                     :size 32
                                     submit-wtf-name))
       (widget-insert "\n")
       (widget-insert "Your e-mail:           ")
       (setq wtf-email (widget-create 'editable-field
                                      :size 32
                                      (format "%s" submit-wtf-email)))
       (widget-insert "\n")
       (widget-insert "Subject:               ")
       (setq wtf-subject (widget-create 'editable-field
                                        :size 32
                                        submit-wtf-subject))
       (widget-insert "\n")
       (widget-insert "Do not publish:        ")
       (setq wtf-dontPublish (widget-create 'toggle))
       (widget-insert "\n")
       (widget-insert "Source: \n")
       (setq wtf-source (widget-create 'text 
                                       (format "%s" source)
                                       :parent))
       (widget-insert "\n")
       (widget-insert "Comment: \n")
       (setq wtf-comment (widget-create 'text ""))
       (widget-insert  "\n")
       (widget-create 'push-button 
                      :action 'wtf-submit-button
                      "Submit")
       (widget-insert  "   ")
       (widget-create 'push-button 
                      :action 'wtf-cancel-button
                      "Cancel")
       
       (use-local-map widget-keymap)
       (widget-setup)
       (widget-minor-mode)

       (set-window-buffer nil buff)))))


(defun wtf-submit-button (&rest ignore) 
  "Does the actual submit to the http://thedailywtf.com/SubmitWTF.asmx web service."
  (let ((xml (format "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<soap12:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap12=\"http://www.w3.org/2003/05/soap-envelope\">
  <soap12:Body>
    <Submit xmlns=\"http://thedailywtf.com/\">
      <name>%s</name>
      <emailAddress>%s</emailAddress>
      <subject>%s</subject>
      <comments>%s</comments>
      <codeSubmission>%s</codeSubmission>
      <doNotPublish>%s</doNotPublish>
    </Submit>
  </soap12:Body>
</soap12:Envelope>"
                     (widget-value wtf-name)
                     (widget-value wtf-email)
                     (widget-value wtf-subject)
                     (widget-value wtf-comment)
                     (widget-value wtf-source)
                     (if (widget-value wtf-dontPublish)
                         "true"
                       "false"))))
    (setq wtf-proc (http-post "http://thedailywtf.com/SubmitWTF.asmx" 
                              xml 
                              "application/soap+xml; charset=utf-8"
                              nil
                              'wtf-response-ignore))
    (kill-buffer (get-buffer-create "*submit-wtf*"))))


(defun wtf-cancel-button (&rest ignore) 
  (kill-buffer (get-buffer-create "*submit-wtf*")))

(defun wtf-response-ignore(&rest ignore)
  (kill-buffer (process-buffer wtf-proc)))

(defun http-post (url body content-type &optional headers sentinel
                      version verbose bufname)
  "Post to a URL in a buffer using HTTP 1.1, and return the process.
You can get the buffer associated with this process using 
`process-buffer'. Shamelessly ripped from http-post.el

PARAMTERS has been replaces by body

CONTENT-TYPE is a coding system to use.  Its upper case print name
will be used for the server.  Possible values are `iso-8859-1' or
`euc-jp' and others.

The optional HEADERS are an alist where each element has the form
\(NAME . VALUE).  Both must be strings and will be passed along with
the request.  The reason CONTENT-TYPE is not just passed along as one
of the headers is that part of the Content-Type value is fixed and
cannot be changed: The basic encoding is implemented using
`html-url-encode' and is called application/x-www-form-urlencoded.

With optional argument SENTINEL, the buffer is not shown.  It is the
responsability of the sentinel to show it, if appropriate.  A sentinel
function takes two arguments, process and message.  It is called when
the process is killed, for example.  This is useful when specifying a
non-persistent connection.  By default, connections are persistent.
Add \(\"Connection\" . \"close\") to HEADERS in order to specify a
non-persistent connection.  Usually you do not need to specify a
sentinel, and `ignore' is used instead, to prevent a message being
printed when the connection is closed.

If you want to filter the content as it arrives, bind
`http-filter-pre-insert-hook' and `http-filter-post-insert-hook'.

The optional argument VERSION specifies the HTTP version to use.  It
defaults to version 1.0, such that the connection is automatically
closed when the entire document has been downloaded.

If the optional argument VERBOSE is non-nil, a message will show the
command sent to the server.

The coding system of the process is set to `binary', because we need to
distinguish between \\r and \\n.  To correctly decode the text later,
use `decode-coding-region' and get the coding system to use from
`http-headers'."
  (interactive)
  (setq version (or version 1.1))
  (let* (host dir file port proc buf header content-length)
    (unless (string-match
             "http://\\([^/:]+\\)\\(:\\([0-9]+\\)\\)?/\\(.*/\\)?\\([^:]*\\)"
             url)
      (error "Cannot parse URL %s" url))
    (unless bufname (setq bufname
                          (format "*HTTP POST %s *" url)))
    (setq host (match-string 1 url)
          port (or (and (setq port (match-string 3 url)) (string-to-int port)) 80)
          dir  (or (match-string 4 url) "")
          file (or (match-string 5 url) "")
          buf (get-buffer-create bufname)
          proc (open-network-stream (concat "HTTP POST " url)
                                    buf (if http-proxy-host http-proxy-host host) (if http-proxy-port http-proxy-port port)))
    (set-process-sentinel proc (or sentinel 'ignore))
    (set-process-coding-system proc 'binary 'binary); we need \r\n in the headers!
    (set-process-filter proc 'http-filter)
    (set-marker (process-mark proc) (point-min) buf)
    (if sentinel
        (set-buffer buf)
      (switch-to-buffer buf))
    (erase-buffer)
    (kill-all-local-variables)
    
    (setq header
          (concat (format "POST %s%s%s HTTP/%.1f\r\n" 
                          (if http-proxy-host (concat "http://" host "/")	"/")
                          dir 
                          file
                          version)
                  (format "Host: %s\r\n" host)
                  (format "Content-Type: %s\r\n" content-type)
                  (format "Content-Length: %d\r\n" (length body))
                  "Connection: close\r\n"
                  "\r\n"))
    (when verbose
      (insert header body "\n\n"))
    (print (format "%s%s%s" header body "\n\n"))
    (process-send-string proc (concat header body "\r\n"))
    proc))


(provide 'the-daily-wtf)


;;; http-post-curl.el --- simple HTTP POST

;; Copyright (C) 2002, 2003  Alex Schroeder

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: David Hansen <david.hansen@physik.fu-berlin.de>
;; Version: 1.0.5
;; Keywords: hypermedia
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?HttpPost

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Use `http-post' to post to a URL.

;;; Change Log:

;; 1.0.6
;;   - Use curl for https urls.
;; 1.0.5
;;   - Added experimental cookies support.
;; 1.0.4
;;   - Fixed bug in `http-post' that ignored the headers argument.
;; 1.0.3
;;   - Minor fix.
;; 1.0.1
;;   - Moved http-url-encode to http-get.

;;; Code:

(require 'http-get)
(require 'http-cookies)

(defvar http-post-version "1.0.6")


;; The main function

(defun http-post (url parameters content-type &optional headers sentinel
                      version verbose bufname)
  "Post to a URL in a buffer using HTTP 1.1, and return the process.
You can get the buffer associated with this process using 
`process-buffer'.



PARAMETERS is an alist of parameters to use.  Each element has the
form \(NAME . VALUE).  These usually correspond to successful controls
on HTML forms.

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
responsibility of the sentinel to show it, if appropriate.  A sentinel
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
  (setq version (or version 1.0))
  (unless (string-match
           "https?://\\([^/:]+\\)\\(:\\([0-9]+\\)\\)?/\\(.*/\\)?\\([^:]*\\)"
           url)
    (error "Cannot parse URL %s" url))
  (let* (host dir file port proc buf header body content-length
              (use-curl (string-match "https" url))
              (separator (if use-curl "\\r\\n" "\r\n")))
    (unless bufname (setq bufname
			  (format "*HTTP POST %s *" url)))
    (setq host (match-string 1 url)
	  port (or (and (setq port (match-string 3 url))
                        (string-to-int port)) 80)
	  dir  (or (match-string 4 url) "")
	  file (or (match-string 5 url) "")
	  buf (get-buffer-create bufname))
    
    (let (result)
      (dolist (param parameters)
        (setq result (cons (concat (car param) "="
                                   (http-url-encode (cdr param)
                                                    content-type))
                           result)))
      (setq body (mapconcat 'identity result "&")))
    
    (setq header
          (concat (if use-curl
                      ""
                      (format "POST %s%s%s HTTP/%.1f%s"
                              (if http-proxy-host
                                  (concat "http://" host "/")
                                  "/") dir file version separator))
                  (format "Host: %s\r\n" host)
                  "Content-Type: application/x-www-form-urlencoded"
                  (format "; charset=%s%s"
                          (upcase (symbol-name content-type))
                          separator)
                  (format "Content-Length: %d%s" (length body) separator)))
    
    (when http-emacs-use-cookies
      (let ((cookie (http-cookies-build-header url)))
        (when cookie (add-to-list 'headers cookie))))
    (if headers
        (setq header (concat header
                             (mapconcat (lambda (pair)
                                          (concat (car pair) ": " (cdr pair)))
                                        headers
                                        separator)
                             (concat separator separator)))
        (setq header (concat header separator)))
    
    (if sentinel
	(set-buffer buf)
        (switch-to-buffer buf))
    (erase-buffer)
    (kill-all-local-variables)
    
    (setq proc (if use-curl
                   (start-process "curl" buf "curl" "--compressed" "-H" (concat "\"" header "\"") "-X" "POST" "--data-binary" (concat "\"" body "\"") url)
                   (open-network-stream
                    (concat "HTTP POST " url)
                    buf (if http-proxy-host http-proxy-host host)
                    (if http-proxy-port http-proxy-port port))))
    (set-process-sentinel proc (or sentinel 'ignore))
    (set-process-coding-system proc 'binary 'binary) ; we need \r\n
    (set-process-filter proc 'http-filter)
    (set-marker (process-mark proc) (point-min) buf)

    (with-current-buffer buf
      (setq http-host host)
      (setq http-url url)
      (when use-curl (setq curl-command (process-command proc))))

    (when verbose
      ;;(when t
      (message "%s" (concat header body "\n\n")))
    (unless use-curl
      (process-send-string proc (concat header body separator)))
    proc))


(provide 'http-post)

;;; http-post-curl.el ends here

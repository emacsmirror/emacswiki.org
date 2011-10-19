;;; httpget.el  - small utility interactive fn to perform an HTTP GET

;; Author     : Dino Chiesa <dpchiesa@hotmail.com>
;; Created    : May 2011
;; Modified   : Oct 2011
;; Version    : 0.2
;; Keywords   : http url get
;; X-URL      : http://cheeso.members.winisp.net/srcview.aspx?dir=emacs&file=httpget.el
;; Last-saved : <2011-October-18 18:20:10>

;;
;; This module defines one interactive function, `httpget', along with
;; a few helper routines it uses, for doing HTTP GETs easily from the
;; minibuffer.
;;
;; THIS MODULE DEPENDS ON AN EXTERNAL WGET PROGRAM, for example see
;; https://cheesoexamples.svn.codeplex.com/svn/wget/wget.cs
;;
;; This function is something like `browse-url-emacs' but more
;; convenient and more interactive and helpful.
;;
;; =======================================================
;;
;; Copyright (c) 2011 Dino Chiesa
;; All rights reserved.
;;
;; Licensed under a BSD-style license.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in
;;    the documentation and/or other materials provided with the
;;    distribution.
;;
;;  - The names of the contributors may not be used to endorse or
;;    promote products derived from this software without specific prior
;;    written permission.
;;
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER "AS IS" AND ANY
;; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
;; OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;


(defvar httpget--recv-buffer nil
  "buffer to recv http output.")

(defvar httpget--wget-prog nil
  "path to wget program.")


;; (defun httpget--fixup-linefeeds ()
;;   "function to remove the ^M that show up in downloaded URL content."
;;   (interactive)
;;   (save-excursion
;;     (while (search-forward "\xd" nil t)
;;       (replace-match "" nil t))))



(defun string/starts-with (s arg)
  "returns t if string S starts with ARG.  Else nil."
  (cond ((>= (length s) (length arg))
         (string-equal (substring s 0 (length arg)) arg))
        (t nil)))

(defun string/last-index-of (s c)
  "Returns the index of the last occurrence of character C in string S.
Returns nil if not found.

"
  (let ((i (1- (length s)))
        ix c2)
    (while (and (>= i 0) (not ix))
      (setq c2 (aref s i))
      (if (= c c2)
          (setq ix i))
      (decf i))
    ix))


(defun httpget (url buffername)
  "Perform an HTTP GET on the given URL, place the result into a
buffer with BUFFERNAME, and pop to that buffer when complete.

This is really handy when you want to use emacs to view a
Javascript file, an rss feed, or an HTML layout available on the
web. Very quickly you can download it and pop open the buffer.

When this fn is called interactively, it prompts for the URL and
buffername. If you have a url at the front of your kill-ring, it
uses that as the default. This is handy, in case you use the OS
\"clipboard\" to copy a url from some other application.  Also,
the default buffername is automatically derived from the
URL. Normally you just need to press <return> twice.  Pretty
quick and easy when called interactively.

In fact, most of the \"value add\" of this function is just the
interactive part.

See also `browse-url-emacs', which is not related, but it similar.

"
  ;;(interactive "surl ? \nBbuffer? ")

  (interactive
   (let (url bufname prompt ix suggested-bufname
             (possible-url (substring-no-properties (current-kill 0 t))))

     (setq prompt
           (if (and possible-url (stringp possible-url)
                    (or
                     (string/starts-with possible-url "http://")
                     (string/starts-with possible-url "https://")))
               (format "url (%s) ? " possible-url)
             (setq possible-url nil)
             "url ? "))
     (setq url (read-from-minibuffer prompt nil nil nil nil nil))
     (if (or (not url) (string= url ""))
         (if possible-url
             (setq url possible-url)
           (error "no url!")))

     (setq ix (string/last-index-of url ?/))
     (setq suggested-bufname
           (if (and ix (< ix (1- (length url))))
               (concat (substring url (1+ ix)) ".out")
             "httpget"))
     (setq suggested-bufname
           (generate-new-buffer-name suggested-bufname))
     (setq prompt (format "buffer name (%s) ? " suggested-bufname))
     (setq bufname (read-from-minibuffer prompt nil nil nil nil nil))
     (if (or (not bufname) (string= bufname ""))
         (setq bufname suggested-bufname))
     (list url bufname)))

  (let ((buf (generate-new-buffer buffername)))
    (switch-to-buffer buf)
    (call-process httpget--wget-prog nil t t
                  "-q" url "-")))

(provide 'httpget)


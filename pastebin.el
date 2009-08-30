;;; pastebin.el --- A simple interface to the www.pastebin.com webservice

;;; Copyright (C) 2008 by Tapsell-Ferrier Limited

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc.,   51 Franklin Street, Fifth Floor,
;;; Boston, MA  02110-1301  USA

;;; Commentary:
;;;
;;; Load this file and run:
;;;
;;;   M-x pastebin-buffer
;;;
;;; to send the whole buffer or select a region and run
;;;
;;;  M-x pastebin
;;;
;;; to send just the region.
;;;
;;; In either case the url that pastebin generates is left on the kill
;;; ring and the paste buffer.


;;; Code:

(defvar pastebin-type-assoc
  '((emacs-lisp-mode . "lisp")
    (scheme-mode . "lisp")
    (python-mode . "python")
    (nxml-mode . "XML")))

(defvar pastebin-retrieved nil)

(defvar pastebin-prefix-history '())

(defun pastebin-buffer (&optional prefix)
  "Send the whole buffer to pastebin.com.
Optional argument PREFIX will request the virtual host to use,
 eg:'emacs' for 'emacs.pastebin.com'."
  (interactive (if current-prefix-arg
                   (list (read-string "pastebin prefix:" nil 'pastebin-prefix-history))))
  (pastebin (point-min) (point-max) prefix))

(defun pastebin (start end &optional prefix)
  "An interface to the pastebin code snippet www service.

See pastebin.com for more information about pastebin.

Called interactively pastebin uses the current region for
preference for sending... if the mark is NOT set then the entire
buffer is sent.

Argument START is the start of region.
Argument END is the end of region.

If PREFIX is used pastebin prompts for a prefix to be used as the
virtual host to use.  For example use 'emacs' for 'emacs.pastebin.com'."
  (interactive
   (let ((pastebin-prefix
          (if current-prefix-arg
              (read-string "pastebin prefix:" nil 'pastebin-prefix-history))))
     (if (mark)
         (list (region-beginning) (region-end) pastebin-prefix)
       (list (point-min) (point-max) pastebin-prefix))))
  ;; Main function
  (let* ((data (buffer-substring-no-properties start end))
         (pastebin-url (format "http://%spastebin.com/pastebin.php" (if prefix
                                                                        (concat prefix ".") "")))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data
          (concat (format "paste=Send&format=%s&expiry=d&poster=%s&code2=%s"
                          (assoc-default major-mode pastebin-type-assoc nil "text")
                          (user-full-name)
                          (url-hexify-string data))))
         (content-buf (url-retrieve pastebin-url
                                    (lambda (arg)
                                      (cond
                                       ((equal :error (car arg))
                                        (signal (cdr arg)))
                                       ((equal :redirect (car arg))
                                        (setq pastebin-retrieved (cadr arg))
                                        (with-temp-buffer
                                          (insert pastebin-retrieved)
                                          (clipboard-kill-ring-save (point-min) (point-max)))))))))))

(provide 'pastebin)
;;; pastebin.el ends here

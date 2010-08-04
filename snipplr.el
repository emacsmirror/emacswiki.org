;;; snipplr.el --- Emacs interface to snipplr.com

;; Copyright (C) 2007 by Dale K. Hawkins

;; Emacs Lisp Archive Entry
;; Filename: snipplr.el
;; Author: Dale K. Hawkins <dkhawk@gmail.com>
;; Version: 1.0a
;; Keywords: extensions, tools

;;{{{ GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;}}}

;;; Commentary:

;; This file provides an emacs based interface to snipplr.com.

;;; Dependencies:
;; xml-rpc
;; http://elisp.info/package/xml-rpc/

;;; Issues:
;; * improve the html unescape sequences!

;;; Todo:

;; * Real snipplr mode
;; ** Click to insert

;;; History:

;; 0.1 - Proof of concept!
;; 0.2 - Added widget goodness

;;; Code:

(require 'xml-rpc)
(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(defcustom snipplr-user-key "Your key goes here"
  "*Snipplr user key.  Once you have a snipplr account, this key can be found on Settings tab."
  :type '(string)
  :group 'snipplr
  :link '(url-link "http://snipplr.com"))

(defun snipplr-checkkey()
  "*Confirm `snipplr-user-key' is a valid key.
Return t if the key is valid, false otherwise."
  (interactive)
  (= 1 (xml-rpc-method-call "http://snipplr.com/xml-rpc.php"
                            'user.checkkey
                            snipplr-user-key))
)

(defun snipplr-show-snippet(snippet)
  (let ((id (cdr (assoc "id" snippet)))
        (title (cdr (assoc "title" snippet)))
        )
    (widget-create 'push-button
                   :value snippet
                   :format (format "%%[[%d: %s]%%]" id title)
                   :notify (lambda (widget &rest ignore)
                             (progn
                               (snipplr-get (cdr (assoc "id" (widget-value widget))) nil
                                            (cdr (assoc "title" (widget-value widget))))
                               )))
    (widget-create 'push-button
                   :value snippet
                   :format "%[[Copy to Kill Ring]%]"
                   :notify (lambda (widget &rest ignore)
                             (progn
                               (snipplr-get (cdr (assoc "id" (widget-value widget))) t
                                            (cdr (assoc "title" (widget-value widget))))
                               )))
    (widget-insert "\n")
    ))

(defun snipplr-list(&optional tag)
  "*List all code snippets on snipplr.  The optional argument `tag' limits by tag."
  (interactive "stags: \n")
  (let ((snippets (xml-rpc-method-call "http://snipplr.com/xml-rpc.php"
                                       'snippet.list
                                       snipplr-user-key
                                       tag))
        (out-buffer (get-buffer-create "*snipplr*"))
        )
    (switch-to-buffer-other-window out-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (dolist (desc (map 'list 'snipplr-show-snippet snippets)) (and desc (insert desc)))
    (use-local-map widget-keymap)
    (widget-setup)
    )
  )

(defun snipplr-get(id &optional copy-to-kill title)
  "*Get a snipplr snippet by id and append it to the `kill-ring'."
  (interactive "nsnippet ID: ")
  (message "getting %s" (widget-value widget))
  (let* ((buffer-name (format "*snipplr-%d-%s*" id title))
         (out-buffer (get-buffer buffer-name))
         (snippet-source (if out-buffer
                             (save-excursion
                               (message "using cached version")
                               (set-buffer buffer-name)
                               (buffer-string)
                               )
                           (cdr (assoc "source" (xml-rpc-method-call "http://snipplr.com/xml-rpc.php"
                                                                     'snippet.get
                                                                     id)))))
         (out-buffer (get-buffer-create buffer-name))
         )
    (switch-to-buffer-other-window out-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; mode?
      (insert snippet-source)
      (snipplr-html-tag-unescape)
      )
    (setq buffer-read-only t)
    (and copy-to-kill (kill-new snippet-source) (message "snippet copied to kill-ring"))
    )
)

(if (or (string= snipplr-user-key "Your key goes here") (not (snipplr-checkkey)))
    (customize-group 'snipplr)
)

(defun snipplr-html-tag-unescape()
  (interactive)
  (save-restriction
    (beginning-of-buffer)
    (save-excursion
      (while (search-forward "&lt;" nil t)
        (replace-match "<" nil t)))
    (save-excursion
      (while (search-forward "&gt;" nil t)
        (replace-match ">" nil t)))
    (save-excursion
      (while (search-forward "&amp;" nil t)
        (replace-match "&" nil t)))
    (save-excursion
      (while (search-forward "&quot;" nil t)
        (replace-match "\"" nil t)))
    ))

(provide 'snipplr)

;;; snipplr.el ends here
